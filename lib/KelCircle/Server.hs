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
    , decode
    , encode
    , object
    , (.=)
    )
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Read qualified as TR
import KelCircle.Events
    ( BaseDecision
    , CircleEvent (..)
    )
import KelCircle.Processing (FullState (..))
import KelCircle.Server.JSON
    ( AppendResult (..)
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
    , readEventsFrom
    , readFullState
    )
import KelCircle.Types (MemberId (..))
import KelCircle.Validate
    ( ValidationError
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
    , scBroadcast :: TChan Int
    -- ^ SSE broadcast channel
    }

{- | Build a WAI 'Application' from a 'ServerConfig'.
Routes: GET /info, GET /condition, GET /events?after=N,
POST /events, GET /stream. Unmatched routes are passed
to the optional fallback application, or return 404.
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
                    (after + 1)
            case events of
                [] ->
                    respond $
                        jsonResponse status404 $
                            BadRequest
                                "no event at position"
                (se : _) ->
                    respond $
                        responseLBS
                            status200
                            jsonHeaders
                            ( encode $
                                object
                                    [ "signer"
                                        .= seSigner se
                                    , "event"
                                        .= TE.decodeUtf8
                                            ( LBS.toStrict $
                                                seEventJson se
                                            )
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
        Nothing ->
            respond $
                jsonResponse status400 $
                    BadRequest "invalid JSON"
        Just sub -> do
            fs <- readFullState (scStore cfg)
            let cs = circleState (fsCircle fs)
                mode = authMode cs
            case mode of
                Bootstrap ->
                    handleBootstrapPost
                        cfg
                        sub
                        fs
                        respond
                Normal ->
                    doAppend cfg sub fs respond

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
    case validateCircleEvent cfg fs signer evt of
        Left ve ->
            respond $
                jsonResponse status422 $
                    ValidationErr ve
        Right () -> do
            sn <-
                appendCircleEvent
                    (scStore cfg)
                    (scAppFold cfg)
                    (subSigner sub)
                    (subSignature sub)
                    evt
            atomically $
                writeTChan (scBroadcast cfg) sn
            respond $
                responseLBS
                    status200
                    jsonHeaders
                    (encode $ AppendResult sn)

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
    CEBaseDecision bd ->
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
                        sn <-
                            atomically $ readTChan ch
                        write $
                            Builder.byteString
                                "event: new\ndata: \
                                \{\"sn\":"
                                <> Builder.intDec sn
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
