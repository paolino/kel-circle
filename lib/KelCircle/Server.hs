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
import Data.List (intersperse)
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
    , buildInteractionBytes
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
    , lookupKelKeyState
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
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 (KeyPair, sign)
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
    | -- | Member KELs updated (member id, event count)
      KelUpdates [(MemberId, Int)]
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
    , scSequencerKeyPair :: KeyPair
    -- ^ Sequencer's Ed25519 keypair for signing
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
                    -- Verify or sign interaction event
                    let evtJson = toJSON evt
                        sid =
                            sequencerId (fsCircle fs)
                        -- Resolve key state: use
                        -- inception KEL for self-intro,
                        -- otherwise use cache
                        resolveKks =
                            case kelData of
                                Just (mid, kel)
                                    | mid == signer ->
                                        pure $
                                            case kelKeyState kel of
                                                Right kks -> Just kks
                                                Left _ -> Nothing
                                _ ->
                                    lookupKelKeyState
                                        (scStore cfg)
                                        signer
                    ixnResult <-
                        if signer == sid
                            then
                                signSequencerInteraction
                                    (scSequencerKeyPair cfg)
                                    evtJson
                                    resolveKks
                                    signer
                            else
                                verifyInteractionSig
                                    signer
                                    (subSignature sub)
                                    evtJson
                                    resolveKks
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
                            (sn, kelUpdates) <-
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
                            -- Broadcast KEL updates
                            case kelUpdates of
                                [] -> pure ()
                                _ ->
                                    atomically $
                                        writeTChan
                                            ( scBroadcast
                                                cfg
                                            )
                                            ( KelUpdates
                                                kelUpdates
                                            )
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
signer. Returns @Right (Just (mid, kelEvent))@ on
success, or @Left ValidationError@ on failure.
Takes a pre-resolved key state from the cache.
-}
verifyInteractionSig
    :: MemberId
    -- ^ Signer
    -> Text
    -- ^ Signature (CESR-encoded)
    -> Value
    -- ^ Circle event as JSON (anchor)
    -> IO (Maybe KelKeyState)
    -- ^ Pre-resolved key state
    -> IO
        ( Either
            ValidationError
            (Maybe (MemberId, KelEvent))
        )
verifyInteractionSig signer sig evtJson resolveKks = do
    mKks <- resolveKks
    pure $ case mKks of
        Nothing ->
            Left (SignerHasNoKel signer)
        Just kks ->
            case verifyInteraction kks sig evtJson of
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
                    Right $ Just (signer, ke)

{- | Sign a circle event as a KERI interaction on
behalf of the sequencer. Uses the cached key state,
computes the next interaction event, signs it with
the server's keypair, and returns the new 'KelEvent'.
-}
signSequencerInteraction
    :: KeyPair
    -> Value
    -- ^ Circle event JSON (anchor)
    -> IO (Maybe KelKeyState)
    -- ^ Pre-resolved key state
    -> MemberId
    -- ^ Sequencer MemberId
    -> IO
        ( Either
            ValidationError
            (Maybe (MemberId, KelEvent))
        )
signSequencerInteraction kp evtJson resolveKks mid = do
    mKks <- resolveKks
    pure $ case mKks of
        Nothing ->
            Left (SignerHasNoKel mid)
        Just kks ->
            let nextSeq = kksSeqNum kks + 1
                evtBytes =
                    buildInteractionBytes
                        (kksPrefix kks)
                        nextSeq
                        (kksLastDigest kks)
                        evtJson
                sigBytes = sign kp evtBytes
                sigCesr =
                    Keri.Cesr.Encode.encode $
                        Primitive
                            { code = Ed25519Sig
                            , raw = sigBytes
                            }
                ke =
                    KelEvent
                        { keEventBytes =
                            evtBytes
                        , keSignatures =
                            [(0, sigCesr)]
                        }
            in  Right $ Just (mid, ke)

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
            mKks <-
                lookupKelKeyState (scStore cfg) mid
            case mKks of
                Nothing -> do
                    log' "  member KEL not found"
                    respond $
                        jsonResponse status404 $
                            BadRequest
                                "member KEL not found"
                Just kks ->
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
                    (KelUpdates [(mid, newCount)])
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
                            KelUpdates updates -> do
                                write $
                                    Builder.byteString
                                        "event: kel\n\
                                        \data: ["
                                        <> mconcat
                                            ( intersperse
                                                ( Builder.byteString
                                                    ","
                                                )
                                                ( map
                                                    kelUpdateEntry
                                                    updates
                                                )
                                            )
                                        <> Builder.byteString
                                            "]\n\n"
                                flush
                        loop
                loop

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

-- | Build a single KEL update entry for SSE.
kelUpdateEntry
    :: (MemberId, Int) -> Builder.Builder
kelUpdateEntry (mid, count) =
    Builder.byteString "{\"memberId\":\""
        <> Builder.byteString
            (TE.encodeUtf8 (unMemberId mid))
        <> Builder.byteString "\",\"eventCount\":"
        <> Builder.intDec count
        <> Builder.byteString "}"

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
