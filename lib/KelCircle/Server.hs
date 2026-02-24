{- |
Module      : KelCircle.Server
Description : HTTP server for kel-circle (WAI application)
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

WAI application providing JSON endpoints for circle
management and SSE notifications. Validates events
through the two-level gate before appending to the
store.
-}
module KelCircle.Server
    ( ServerConfig (..)
    , SSEMessage (..)
    , mkApp
    ) where

import Control.Concurrent.STM
    ( TChan
    , atomically
    , dupTChan
    , readTChan
    , writeTChan
    )
import Data.Aeson
    ( FromJSON
    , ToJSON
    , Value
    , decode
    , encode
    , object
    , toJSON
    , (.=)
    )
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import KelCircle.Crypto (validateCesrPrefix)
import KelCircle.Events
    ( BaseDecision (..)
    , CircleEvent (..)
    )
import KelCircle.InteractionVerify
    ( VerifyResult (..)
    , verifyInteraction
    )
import KelCircle.MemberKel
    ( KelEvent (..)
    , KelKeyState (..)
    , MemberKel (..)
    , kelEventCount
    , kelFromInception
    , kelKeyState
    , parseInceptionValue
    , validateInception
    )
import KelCircle.Processing (FullState (..))
import KelCircle.RotationVerify qualified as RV
import KelCircle.Server.JSON
    ( AppendResult (..)
    , RotationSubmission (..)
    , ServerError (..)
    , Submission (..)
    )
import KelCircle.State
    ( AuthMode (..)
    , Circle (..)
    , CircleState (..)
    , authMode
    )
import KelCircle.Store
    ( CircleStore (..)
    , StoredEvent (..)
    , appendCircleEvent
    , appendRotationEvent
    , readEventsFrom
    , readFullState
    , readMemberKel
    )
import KelCircle.Types (MemberId (..))
import KelCircle.Validate
    ( ValidationError (..)
    , validateAppDecision
    , validateBaseDecision
    , validateProposal
    , validateResolve
    , validateResponse
    )
import Network.HTTP.Types
    ( HeaderName
    , Status
    , hContentType
    , status200
    , status400
    , status401
    , status404
    , status422
    )
import Network.Wai
    ( Application
    , Request
    , Response
    , pathInfo
    , queryString
    , requestMethod
    , responseLBS
    , responseStream
    , strictRequestBody
    )

-- | SSE message types for the broadcast channel.
data SSEMessage
    = -- | Circle event appended (sequence number)
      CircleEvent Int
    | -- | Member KEL updated (member id, event count)
      KelUpdate MemberId Int
    deriving stock (Show, Eq)

{- | Server configuration, parameterized by application
types.
-}
data ServerConfig g d p r = ServerConfig
    { scStore :: CircleStore g p r
    -- ^ Persistent store
    , scAppFold :: g -> d -> g
    -- ^ Application fold function
    , scBaseAppGate :: g -> BaseDecision -> Bool
    -- ^ App-level gate for base decisions
    , scAppGate :: g -> d -> Bool
    -- ^ App-level gate for app decisions
    , scProposalGate :: g -> p -> Bool
    -- ^ App-level gate for proposals
    , scPassphrase :: Text
    -- ^ Bootstrap passphrase
    , scBroadcast :: TChan SSEMessage
    -- ^ SSE broadcast channel
    , scLog :: Text -> IO ()
    -- ^ Logger function
    }

{- | Build a WAI 'Application' from a 'ServerConfig'.
Routes: GET /info, GET /condition, GET /events?after=N,
POST /events, GET /stream, GET /members/:id/kel,
POST /members/:id/rotate.
Unmatched routes are passed to the optional fallback
application, or return 404.
-}
mkApp
    :: ( FromJSON d
       , FromJSON p
       , FromJSON r
       , ToJSON d
       , ToJSON p
       , ToJSON r
       )
    => ServerConfig g d p r
    -> Maybe Application
    -- ^ Optional fallback for unmatched routes
    -> Application
mkApp cfg mFallback req respond =
    case (requestMethod req, pathInfo req) of
        ("GET", ["info"]) ->
            handleInfo cfg respond
        ("GET", ["condition"]) ->
            handleCondition cfg respond
        ("GET", ["events"]) ->
            handleGetEvent cfg req respond
        ("POST", ["events"]) ->
            handlePostEvent cfg req respond
        ("GET", ["stream"]) ->
            handleStream cfg respond
        ("GET", ["members", midText, "kel"]) ->
            handleGetMemberKel
                cfg
                midText
                req
                respond
        ("POST", ["members", midText, "rotate"]) ->
            handleRotate cfg midText req respond
        _ -> case mFallback of
            Just fallback ->
                fallback req respond
            Nothing ->
                respond $
                    jsonResponse status404 $
                        BadRequest "not found"

-- --------------------------------------------------------
-- GET /info
-- --------------------------------------------------------

handleInfo
    :: ServerConfig g d p r
    -> (Response -> IO a)
    -> IO a
handleInfo cfg respond = do
    fs <- readFullState (scStore cfg)
    let cs = circleState (fsCircle fs)
        mode = authMode cs
        sid = sequencerId (fsCircle fs)
    respond $
        responseLBS
            status200
            jsonHeaders
            ( encode $
                object
                    [ "authMode" .= mode
                    , "sequencerId" .= sid
                    , "memberCount"
                        .= length (members cs)
                    , "nextSeq" .= fsNextSeq fs
                    ]
            )

-- --------------------------------------------------------
-- GET /condition
-- --------------------------------------------------------

handleCondition
    :: (ToJSON p, ToJSON r)
    => ServerConfig g d p r
    -> (Response -> IO a)
    -> IO a
handleCondition cfg respond = do
    fs <- readFullState (scStore cfg)
    let cs = circleState (fsCircle fs)
        mode = authMode cs
    respond $
        responseLBS
            status200
            jsonHeaders
            ( encode $
                object
                    [ "authMode" .= mode
                    , "members" .= members cs
                    , "sequencerId"
                        .= sequencerId (fsCircle fs)
                    , "proposals" .= fsProposals fs
                    , "nextSeq" .= fsNextSeq fs
                    ]
            )

-- --------------------------------------------------------
-- GET /events?after=N
-- --------------------------------------------------------

handleGetEvent
    :: ServerConfig g d p r
    -> Request
    -> (Response -> IO a)
    -> IO a
handleGetEvent cfg req respond =
    case parseAfter req of
        Nothing ->
            respond $
                jsonResponse status400 $
                    BadRequest
                        "missing or invalid ?after=N"
        Just after -> do
            events <-
                readEventsFrom
                    (scStore cfg)
                    (after + 2)
            case events of
                [] ->
                    respond $
                        jsonResponse status404 $
                            BadRequest
                                "no event at position"
                (se : _) ->
                    let evtVal :: Value
                        evtVal = case decode (seEventJson se) of
                            Just v -> v
                            Nothing ->
                                object
                                    [ "error"
                                        .= ( "corrupt event"
                                                :: Text
                                           )
                                    ]
                    in  respond $
                            responseLBS
                                status200
                                jsonHeaders
                                ( encode $
                                    object
                                        [ "signer"
                                            .= seSigner se
                                        , "event"
                                            .= evtVal
                                        , "signature"
                                            .= seSignature
                                                se
                                        ]
                                )

-- --------------------------------------------------------
-- POST /events
-- --------------------------------------------------------

handlePostEvent
    :: ( FromJSON d
       , FromJSON p
       , FromJSON r
       , ToJSON d
       , ToJSON p
       , ToJSON r
       )
    => ServerConfig g d p r
    -> Request
    -> (Response -> IO a)
    -> IO a
handlePostEvent cfg req respond = do
    body <- strictRequestBody req
    case decode body of
        Nothing -> do
            log' "POST /events: invalid JSON"
            respond $
                jsonResponse status400 $
                    BadRequest "invalid JSON"
        Just sub -> do
            log' $
                "POST /events: signer="
                    <> subSigner sub
            fs <- readFullState (scStore cfg)
            let cs = circleState (fsCircle fs)
                mode = authMode cs
            case mode of
                Bootstrap -> do
                    log' "  mode=bootstrap"
                    handleBootstrapPost
                        cfg
                        sub
                        fs
                        respond
                Normal ->
                    doAppend cfg sub fs respond
  where
    log' = scLog cfg

handleBootstrapPost
    :: (ToJSON d, ToJSON p, ToJSON r)
    => ServerConfig g d p r
    -> Submission d p r
    -> FullState g p r
    -> (Response -> IO a)
    -> IO a
handleBootstrapPost cfg sub fs respond =
    case subPassphrase sub of
        Nothing ->
            respond $
                jsonResponse
                    status401
                    PassphraseRequired
        Just pass
            | pass /= scPassphrase cfg ->
                respond $
                    jsonResponse
                        status401
                        WrongPassphrase
            | otherwise ->
                doAppend cfg sub fs respond

doAppend
    :: (ToJSON d, ToJSON p, ToJSON r)
    => ServerConfig g d p r
    -> Submission d p r
    -> FullState g p r
    -> (Response -> IO a)
    -> IO a
doAppend cfg sub fs respond = do
    let signer = MemberId (subSigner sub)
        evt = subEvent sub
        log' = scLog cfg
        sid = sequencerId (fsCircle fs)
    case validateCircleEvent cfg fs signer evt of
        Left ve -> do
            log' $
                "  validation error: "
                    <> T.pack (show ve)
            respond $
                jsonResponse status422 $
                    ValidationErr ve
        Right () -> do
            -- Validate inception for IntroduceMember
            mKel <-
                validateAndBuildKel
                    log'
                    evt
                    (subInception sub)
            case mKel of
                Left ve -> do
                    log' $
                        "  inception error: "
                            <> T.pack (show ve)
                    respond $
                        jsonResponse status422 $
                            ValidationErr ve
                Right kelData -> do
                    -- Verify interaction signature
                    let evtJson = toJSON evt
                    ixnResult <-
                        verifyInteractionSig
                            signer
                            (subSignature sub)
                            evtJson
                            (readMemberKel (scStore cfg))
                            kelData
                            sid
                    case ixnResult of
                        Left ve -> do
                            log' $
                                "  ixn verify error: "
                                    <> T.pack (show ve)
                            respond
                                $ jsonResponse
                                    status422
                                $ ValidationErr ve
                        Right mIxn -> do
                            (sn, kelCount) <-
                                appendCircleEvent
                                    (scStore cfg)
                                    (scAppFold cfg)
                                    (subSigner sub)
                                    (subSignature sub)
                                    evt
                                    kelData
                                    mIxn
                            log' $
                                "  appended seq="
                                    <> T.pack
                                        (show sn)
                            -- Broadcast circle event
                            atomically $
                                writeTChan
                                    (scBroadcast cfg)
                                    (CircleEvent sn)
                            -- Broadcast KEL update
                            let kelMid =
                                    case (kelData, mIxn) of
                                        ( Just (mid, _)
                                            , _
                                            ) ->
                                                Just mid
                                        ( _
                                            , Just
                                                (mid, _)
                                            ) ->
                                                Just mid
                                        _ -> Nothing
                            case kelMid of
                                Just mid ->
                                    atomically $
                                        writeTChan
                                            ( scBroadcast
                                                cfg
                                            )
                                            ( KelUpdate
                                                mid
                                                kelCount
                                            )
                                Nothing -> pure ()
                            respond $
                                responseLBS
                                    status200
                                    jsonHeaders
                                    ( encode $
                                        AppendResult
                                            sn
                                    )

{- | Validate and build KEL data for IntroduceMember.
Returns @Right Nothing@ for non-IntroduceMember events,
@Right (Just (mid, kel))@ on success, or
@Left ValidationError@ on failure.
-}
validateAndBuildKel
    :: (Text -> IO ())
    -> CircleEvent d p r
    -> Maybe Value
    -> IO
        ( Either
            ValidationError
            (Maybe (MemberId, MemberKel))
        )
validateAndBuildKel _ (CEBaseDecision (IntroduceMember mid _ _)) Nothing =
    pure $ Left (MissingInception mid)
validateAndBuildKel _ (CEBaseDecision (IntroduceMember mid _ _)) (Just inceptionVal) =
    case parseInceptionValue inceptionVal of
        Left err ->
            pure $
                Left (InvalidInception mid err)
        Right inception ->
            case validateInception mid inception of
                Left err ->
                    pure $
                        Left
                            (InvalidInception mid err)
                Right () ->
                    pure $
                        Right $
                            Just
                                ( mid
                                , kelFromInception
                                    inception
                                )
validateAndBuildKel _ _ _ =
    pure $ Right Nothing

{- | Verify the interaction event signature for a
signer. Returns @Right Nothing@ if the signer is the
sequencer (exempt), @Right (Just (mid, kelEvent))@ on
success, or @Left ValidationError@ on failure.
Uses IO to look up KELs from SQLite.
-}
verifyInteractionSig
    :: MemberId
    -- ^ Signer
    -> Text
    -- ^ Signature (CESR-encoded)
    -> Value
    -- ^ Circle event as JSON (anchor)
    -> (MemberId -> IO (Maybe MemberKel))
    -- ^ KEL lookup function
    -> Maybe (MemberId, MemberKel)
    -- ^ Inception KEL (bootstrap self-intro)
    -> MemberId
    -- ^ Sequencer ID
    -> IO
        ( Either
            ValidationError
            (Maybe (MemberId, KelEvent))
        )
verifyInteractionSig
    signer
    sig
    evtJson
    lookupKel
    mKelData
    sid
        | signer == sid = pure $ Right Nothing
        | otherwise = do
            mKel <- case mKelData of
                Just (mid, kel)
                    | mid == signer -> pure $ Just kel
                _ -> lookupKel signer
            pure $ case mKel of
                Nothing ->
                    Left (SignerHasNoKel signer)
                Just kel ->
                    case kelKeyState kel of
                        Left err ->
                            Left $
                                InteractionVerifyFailed
                                    signer
                                    err
                        Right kks ->
                            case verifyInteraction
                                kks
                                sig
                                evtJson of
                                Left err ->
                                    Left $
                                        InteractionVerifyFailed
                                            signer
                                            err
                                Right (VerifyFailed reason) ->
                                    Left $
                                        InteractionVerifyFailed
                                            signer
                                            reason
                                Right (Verified ke) ->
                                    Right $
                                        Just
                                            (signer, ke)

{- | Validate a circle event through the appropriate
gate.
-}
validateCircleEvent
    :: ServerConfig g d p r
    -> FullState g p r
    -> MemberId
    -> CircleEvent d p r
    -> Either ValidationError ()
validateCircleEvent cfg fs signer = \case
    CEBaseDecision bd -> do
        validateMemberIdCesr bd
        validateBaseDecision
            fs
            signer
            bd
            (scBaseAppGate cfg)
    CEAppDecision d ->
        validateAppDecision
            fs
            signer
            d
            (scAppGate cfg)
    CEProposal content _deadline ->
        validateProposal
            fs
            signer
            content
            (scProposalGate cfg)
    CEResponse _content pid ->
        validateResponse fs signer pid
    CEResolveProposal _pid _res ->
        validateResolve fs signer

{- | Validate CESR format of member IDs in base
decisions. Only @IntroduceMember@ carries a new member
ID that must be a valid CESR Ed25519 prefix.
-}
validateMemberIdCesr
    :: BaseDecision -> Either ValidationError ()
validateMemberIdCesr = \case
    IntroduceMember mid _ _ ->
        case validateCesrPrefix (unMemberId mid) of
            Left reason ->
                Left (InvalidMemberId mid reason)
            Right () -> Right ()
    _ -> Right ()

-- --------------------------------------------------------
-- GET /members/:id/kel?after=N
-- --------------------------------------------------------

handleGetMemberKel
    :: ServerConfig g d p r
    -> Text
    -> Request
    -> (Response -> IO a)
    -> IO a
handleGetMemberKel cfg midText req respond = do
    let mid = MemberId midText
    mKel <- readMemberKel (scStore cfg) mid
    case mKel of
        Nothing ->
            respond $
                jsonResponse status404 $
                    BadRequest "member KEL not found"
        Just kel ->
            let after = parseAfterKel req
                events =
                    drop after (mkelEvents kel)
            in  respond $
                    responseLBS
                        status200
                        jsonHeaders
                        ( encode $
                            object
                                [ "memberId"
                                    .= midText
                                , "eventCount"
                                    .= kelEventCount
                                        kel
                                , "after"
                                    .= after
                                , "events"
                                    .= map
                                        kelEventToJSON
                                        events
                                ]
                        )

-- | Convert a KEL event to JSON for the API response.
kelEventToJSON :: KelEvent -> Value
kelEventToJSON ke =
    object
        [ "event"
            .= TE.decodeUtf8 (keEventBytes ke)
        , "signatures" .= keSignatures ke
        ]

-- --------------------------------------------------------
-- POST /members/:id/rotate
-- --------------------------------------------------------

handleRotate
    :: ServerConfig g d p r
    -> Text
    -> Request
    -> (Response -> IO a)
    -> IO a
handleRotate cfg midText req respond = do
    body <- strictRequestBody req
    let mid = MemberId midText
        log' = scLog cfg
    case decode body of
        Nothing -> do
            log' "POST /rotate: invalid JSON"
            respond $
                jsonResponse status400 $
                    BadRequest "invalid JSON"
        Just rotSub -> do
            log' $
                "POST /rotate: member="
                    <> midText
            mKel <-
                readMemberKel (scStore cfg) mid
            case mKel of
                Nothing -> do
                    log' "  member KEL not found"
                    respond $
                        jsonResponse status404 $
                            BadRequest
                                "member KEL not found"
                Just kel ->
                    case kelKeyState kel of
                        Left err -> do
                            log' $
                                "  key state error: "
                                    <> err
                            respond
                                $ jsonResponse
                                    status422
                                $ ValidationErr
                                $ RotationFailed
                                    mid
                                    err
                        Right kks ->
                            doRotation
                                cfg
                                mid
                                kks
                                rotSub
                                respond

doRotation
    :: ServerConfig g d p r
    -> MemberId
    -> KelKeyState
    -> RotationSubmission
    -> (Response -> IO a)
    -> IO a
doRotation cfg mid kks rotSub respond = do
    let log' = scLog cfg
    case RV.verifyRotation
        kks
        (rsNewKeys rotSub)
        (rsNewThreshold rotSub)
        (rsNextKeyCommitments rotSub)
        (rsNextThreshold rotSub)
        (rsSignatures rotSub) of
        Left err -> do
            log' $ "  rotation error: " <> err
            respond $
                jsonResponse status422 $
                    ValidationErr $
                        RotationFailed mid err
        Right (RV.RotationFailed reason) -> do
            log' $
                "  rotation failed: " <> reason
            respond $
                jsonResponse status422 $
                    ValidationErr $
                        RotationFailed mid reason
        Right (RV.RotationVerified ke) -> do
            newCount <-
                appendRotationEvent
                    (scStore cfg)
                    mid
                    ke
            log' $
                "  rotation ok, kel events="
                    <> T.pack (show newCount)
            -- Broadcast KEL update
            atomically $
                writeTChan
                    (scBroadcast cfg)
                    (KelUpdate mid newCount)
            respond $
                responseLBS
                    status200
                    jsonHeaders
                    ( encode $
                        object
                            [ "memberId"
                                .= mid
                            , "eventCount"
                                .= newCount
                            ]
                    )

-- --------------------------------------------------------
-- GET /stream (SSE)
-- --------------------------------------------------------

handleStream
    :: ServerConfig g d p r
    -> (Response -> IO a)
    -> IO a
handleStream cfg respond =
    respond $
        responseStream status200 sseHeaders $
            \write flush -> do
                ch <-
                    atomically $
                        dupTChan (scBroadcast cfg)
                let loop = do
                        msg <-
                            atomically $ readTChan ch
                        case msg of
                            CircleEvent sn -> do
                                write $
                                    Builder.byteString
                                        "event: new\n\
                                        \data: {\"sn\":"
                                        <> Builder.intDec
                                            sn
                                        <> Builder.byteString
                                            "}\n\n"
                                flush
                            KelUpdate mid count -> do
                                write $
                                    Builder.byteString
                                        "event: kel\n\
                                        \data: \
                                        \{\"memberId\":\""
                                        <> Builder.byteString
                                            ( TE.encodeUtf8
                                                ( unMemberId
                                                    mid
                                                )
                                            )
                                        <> Builder.byteString
                                            "\",\
                                            \\"eventCount\":"
                                        <> Builder.intDec
                                            count
                                        <> Builder.byteString
                                            "}\n\n"
                                flush
                        loop
                loop

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

-- | Parse the ?after=N query parameter.
parseAfter :: Request -> Maybe Int
parseAfter req =
    case lookup "after" (queryString req) of
        Just (Just bs) ->
            case TR.signed
                TR.decimal
                (TE.decodeUtf8 bs) of
                Right (n, _) -> Just n
                Left _ -> Nothing
        _ -> Nothing

{- | Parse the ?after=N query parameter for KEL
endpoints. Defaults to 0 if not present.
-}
parseAfterKel :: Request -> Int
parseAfterKel req =
    case lookup "after" (queryString req) of
        Just (Just bs) ->
            case TR.decimal (TE.decodeUtf8 bs) of
                Right (n, _) -> n
                Left _ -> 0
        _ -> 0

jsonHeaders :: [(HeaderName, ByteString)]
jsonHeaders =
    [(hContentType, "application/json")]

sseHeaders :: [(HeaderName, ByteString)]
sseHeaders =
    [ (hContentType, "text/event-stream")
    , ("Cache-Control", "no-cache")
    ]

jsonResponse
    :: (ToJSON e) => Status -> e -> Response
jsonResponse status body =
    responseLBS status jsonHeaders (encode body)
