{- |
Module      : E2E.TestHelpers
Description : Test infrastructure for E2E scenarios
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Reusable test environment, HTTP helpers, and submission
builders for integration tests.
-}
module E2E.TestHelpers
    ( -- * Test environment
      TestEnv (..)
    , withTestEnv

      -- * HTTP helpers
    , httpGet
    , httpPost
    , postEvent
    , postEventRaw
    , getInfo
    , getCondition
    , decodeOrFail

      -- * Test identities
    , TestId (..)
    , newTestId
    , mkBadTestId

      -- * Submission builders
    , TestSub (..)
    , testPass
    , bootstrapAdmin
    , introduceMember
    , introduceAdmin
    , removeMember
    , changeRole
    , submitProposal
    , respondToProposal
    , resolveProposal

      -- * Inception helpers
    , mkInceptionFor

      -- * Signing
    , signSubmission

      -- * Response decoders
    , InfoResp (..)
    , ConditionResp (..)
    , ConditionMember (..)
    , GetEventResp (..)
    ) where

import Control.Concurrent.STM (newBroadcastTChanIO)
import Data.Aeson
    ( FromJSON (..)
    , Value
    , decode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import KelCircle.Events
    ( BaseDecision (..)
    , CircleEvent (..)
    , Resolution
    )
import KelCircle.Server (ServerConfig (..), mkApp)
import KelCircle.Server.JSON
    ( AppendResult (..)
    , Submission (..)
    )
import KelCircle.Store
    ( CircleStore
    , closeStore
    , openStore
    )
import KelCircle.Types
    ( MemberId (..)
    , ProposalId
    , Role (..)
    , Timestamp
    )
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519
    ( KeyPair (..)
    , generateKeyPair
    , publicKeyBytes
    , sign
    )
import Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState.PreRotation (commitKey)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import Test.Tasty.HUnit (assertEqual)

-- --------------------------------------------------------
-- Test identities
-- --------------------------------------------------------

{- | A test identity backed by a real Ed25519 keypair.
The 'tidKey' is the CESR-encoded public key prefix.
-}
data TestId = TestId
    { tidKey :: Text
    -- ^ CESR-encoded Ed25519 public key prefix
    , tidKeyPair :: KeyPair
    -- ^ The underlying Ed25519 keypair
    }

instance Show TestId where
    show tid = "TestId " <> show (tidKey tid)

instance Eq TestId where
    a == b = tidKey a == tidKey b

{- | Generate a fresh test identity with a real
Ed25519 keypair and CESR-encoded public key prefix.
-}
newTestId :: IO TestId
newTestId = do
    kp <- generateKeyPair
    let cesrKey =
            Keri.Cesr.Encode.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw = publicKeyBytes (publicKey kp)
                    }
    pure
        TestId
            { tidKey = cesrKey
            , tidKeyPair = kp
            }

{- | Create a test identity with a non-CESR key
string, for testing validation rejection. Uses a
real keypair internally (the key text is overridden).
-}
mkBadTestId :: Text -> IO TestId
mkBadTestId badKey = do
    kp <- generateKeyPair
    pure
        TestId
            { tidKey = badKey
            , tidKeyPair = kp
            }

-- --------------------------------------------------------
-- Inception helpers
-- --------------------------------------------------------

{- | Create a signed inception event for a test
identity. Returns a JSON 'Value' suitable for the
@inception@ field in a 'Submission'.
-}
mkInceptionFor :: TestId -> IO Value
mkInceptionFor tid = do
    -- Next key for pre-rotation commitment
    nextKp <- generateKeyPair
    let nextCesr =
            Keri.Cesr.Encode.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes
                            (publicKey nextKp)
                    }
    case commitKey nextCesr of
        Left err ->
            error $ "commitKey failed: " <> err
        Right commitment -> do
            let cfg =
                    InceptionConfig
                        { icKeys = [tidKey tid]
                        , icSigningThreshold = 1
                        , icNextKeys = [commitment]
                        , icNextThreshold = 1
                        , icConfig = []
                        , icAnchors = []
                        }
                evt = mkInception cfg
                evtBytes = serializeEvent evt
                sigBytes =
                    sign
                        (tidKeyPair tid)
                        evtBytes
                sigCesr =
                    Keri.Cesr.Encode.encode $
                        Primitive
                            { code = Ed25519Sig
                            , raw = sigBytes
                            }
                evtText = TE.decodeUtf8 evtBytes
            pure $
                object
                    [ "event" .= evtText
                    , "signatures"
                        .= [
                               ( 0 :: Int
                               , sigCesr
                               )
                           ]
                    ]

-- --------------------------------------------------------
-- Passphrase
-- --------------------------------------------------------

-- | Passphrase used in all tests.
testPass :: Text
testPass = "e2e-bootstrap-pass"

-- --------------------------------------------------------
-- Test environment
-- --------------------------------------------------------

-- | Test environment with a running server.
data TestEnv = TestEnv
    { tePort :: Warp.Port
    , teMgr :: HC.Manager
    }

{- | Spin up a fresh server on a random port. Uses
the trivial application (Unit types for d, p, r).
-}
withTestEnv :: (TestEnv -> IO a) -> IO a
withTestEnv action = do
    dbPath <-
        emptySystemTempFile "kel-circle-e2e-.db"
    let sid = MemberId "server-sequencer"
    ch <- newBroadcastTChanIO
    (store :: CircleStore () () ()) <-
        openStore
            sid
            ()
            trivialAppFold
            dbPath
    let cfg =
            ServerConfig
                { scStore = store
                , scAppFold = trivialAppFold
                , scBaseAppGate = trivialBaseGate
                , scAppGate = trivialAppGate
                , scProposalGate =
                    trivialProposalGate
                , scPassphrase = testPass
                , scBroadcast = ch
                , scLog = \_ -> pure ()
                }
    mgr <- HC.newManager HC.defaultManagerSettings
    result <-
        Warp.testWithApplication
            (pure $ mkApp cfg Nothing)
            ( \port ->
                action
                    TestEnv
                        { tePort = port
                        , teMgr = mgr
                        }
            )
    closeStore store
    removeFile dbPath
    pure result

-- | Trivial app fold: Unit state, no-op.
trivialAppFold :: () -> () -> ()
trivialAppFold _ _ = ()

-- | Trivial base app gate: always passes.
trivialBaseGate :: () -> BaseDecision -> Bool
trivialBaseGate _ _ = True

-- | Trivial app gate: always passes.
trivialAppGate :: () -> () -> Bool
trivialAppGate _ _ = True

-- | Trivial proposal gate: always passes.
trivialProposalGate :: () -> () -> Bool
trivialProposalGate _ _ = True

-- --------------------------------------------------------
-- HTTP helpers
-- --------------------------------------------------------

-- | GET request to the test server.
httpGet
    :: TestEnv
    -> String
    -> IO (HC.Response LBS.ByteString)
httpGet te path = do
    req <-
        HC.parseRequest $
            "http://127.0.0.1:"
                <> show (tePort te)
                <> path
    HC.httpLbs req (teMgr te)

-- | POST request with JSON body.
httpPost
    :: TestEnv
    -> String
    -> LBS.ByteString
    -> IO (HC.Response LBS.ByteString)
httpPost te path body = do
    initReq <-
        HC.parseRequest $
            "http://127.0.0.1:"
                <> show (tePort te)
                <> path
    let req =
            initReq
                { HC.method = "POST"
                , HC.requestBody =
                    HC.RequestBodyLBS body
                , HC.requestHeaders =
                    [
                        ( "Content-Type"
                        , "application/json"
                        )
                    ]
                }
    HC.httpLbs req (teMgr te)

{- | POST a test submission and expect 200.
Returns the sequence number.
-}
postEvent :: TestEnv -> TestSub -> IO Int
postEvent te ts = do
    sub <- signSubmission ts
    resp <- httpPost te "/events" (Aeson.encode sub)
    assertEqual
        "POST /events status"
        status200
        (HC.responseStatus resp)
    ar <- decodeOrFail (HC.responseBody resp)
    pure (sequenceNumber ar)

{- | POST a test submission and return the raw
response (for testing error cases).
-}
postEventRaw
    :: TestEnv
    -> TestSub
    -> IO (HC.Response LBS.ByteString)
postEventRaw te ts = do
    sub <- signSubmission ts
    httpPost te "/events" (Aeson.encode sub)

-- | GET /info and decode.
getInfo :: TestEnv -> IO InfoResp
getInfo te = do
    resp <- httpGet te "/info"
    assertEqual
        "GET /info status"
        status200
        (HC.responseStatus resp)
    decodeOrFail (HC.responseBody resp)

-- | GET /condition and decode.
getCondition :: TestEnv -> IO ConditionResp
getCondition te = do
    resp <- httpGet te "/condition"
    assertEqual
        "GET /condition status"
        status200
        (HC.responseStatus resp)
    decodeOrFail (HC.responseBody resp)

-- | Decode JSON or fail the test.
decodeOrFail
    :: (FromJSON a) => LBS.ByteString -> IO a
decodeOrFail bs = case decode bs of
    Just x -> pure x
    Nothing ->
        error $
            "JSON decode failed: "
                <> show (LBS.take 200 bs)

-- --------------------------------------------------------
-- Unsigned test submissions
-- --------------------------------------------------------

-- | An unsigned test submission.
data TestSub = TestSub
    { tsSigner :: TestId
    , tsPassphrase :: Maybe Text
    , tsEvent :: CircleEvent () () ()
    , tsInception :: Maybe (IO Value)
    -- ^ Deferred inception builder (needs IO)
    }

{- | Sign a test submission. Builds the inception
event if one is configured.
-}
signSubmission :: TestSub -> IO (Submission () () ())
signSubmission ts = do
    mInception <- case tsInception ts of
        Nothing -> pure Nothing
        Just mkInc -> Just <$> mkInc
    pure
        Submission
            { subPassphrase = tsPassphrase ts
            , subSigner = tidKey (tsSigner ts)
            , subSignature =
                "sig:" <> tidKey (tsSigner ts)
            , subEvent = tsEvent ts
            , subInception = mInception
            }

-- --------------------------------------------------------
-- Submission builders
-- --------------------------------------------------------

{- | Bootstrap the first admin. The admin provides
their own signed inception event.
-}
bootstrapAdmin :: TestId -> TestSub
bootstrapAdmin tid =
    TestSub
        { tsSigner = tid
        , tsPassphrase = Just testPass
        , tsEvent =
            CEBaseDecision $
                IntroduceMember
                    (MemberId $ tidKey tid)
                    (tidKey tid)
                    Admin
        , tsInception =
            Just (mkInceptionFor tid)
        }

{- | Introduce a regular member. The introducing
admin provides the new member's signed inception.
-}
introduceMember :: TestId -> TestId -> TestSub
introduceMember signer newMember =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent =
            CEBaseDecision $
                IntroduceMember
                    (MemberId $ tidKey newMember)
                    (tidKey newMember)
                    Member
        , tsInception =
            Just (mkInceptionFor newMember)
        }

-- | Introduce a new admin.
introduceAdmin :: TestId -> TestId -> TestSub
introduceAdmin signer newAdmin =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent =
            CEBaseDecision $
                IntroduceMember
                    (MemberId $ tidKey newAdmin)
                    (tidKey newAdmin)
                    Admin
        , tsInception =
            Just (mkInceptionFor newAdmin)
        }

-- | Remove a member.
removeMember :: TestId -> TestId -> TestSub
removeMember signer target =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent =
            CEBaseDecision $
                RemoveMember
                    (MemberId $ tidKey target)
        , tsInception = Nothing
        }

-- | Change a member's role.
changeRole
    :: TestId -> TestId -> Role -> TestSub
changeRole signer target role =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent =
            CEBaseDecision $
                ChangeRole
                    (MemberId $ tidKey target)
                    role
        , tsInception = Nothing
        }

-- | Submit a proposal (trivial Unit content).
submitProposal
    :: TestId -> Timestamp -> TestSub
submitProposal signer deadline =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent = CEProposal () deadline
        , tsInception = Nothing
        }

-- | Respond to a proposal (trivial Unit content).
respondToProposal
    :: TestId -> ProposalId -> TestSub
respondToProposal signer pid =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent = CEResponse () pid
        , tsInception = Nothing
        }

-- | Resolve a proposal (sequencer only).
resolveProposal
    :: TestId
    -> ProposalId
    -> Resolution
    -> TestSub
resolveProposal signer pid res =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent = CEResolveProposal pid res
        , tsInception = Nothing
        }

-- --------------------------------------------------------
-- Response decoders
-- --------------------------------------------------------

-- | Decoded /info response.
data InfoResp = InfoResp
    { irAuthMode :: Text
    , irMemberCount :: Int
    , irNextSeq :: Int
    }
    deriving stock (Show)

instance FromJSON InfoResp where
    parseJSON = withObject "InfoResp" $ \o ->
        InfoResp
            <$> o .: "authMode"
            <*> o .: "memberCount"
            <*> o .: "nextSeq"

-- | A member in the condition response.
data ConditionMember = ConditionMember
    { cmMemberId :: Text
    , cmRole :: Text
    }
    deriving stock (Show)

instance FromJSON ConditionMember where
    parseJSON = withObject "ConditionMember" $ \o ->
        ConditionMember
            <$> o .: "memberId"
            <*> o .: "role"

-- | Decoded /condition response.
data ConditionResp = ConditionResp
    { crAuthMode :: Text
    , crMembers :: [ConditionMember]
    , crProposals :: [Value]
    , crNextSeq :: Int
    }
    deriving stock (Show)

instance FromJSON ConditionResp where
    parseJSON = withObject "ConditionResp" $ \o ->
        ConditionResp
            <$> o .: "authMode"
            <*> o .: "members"
            <*> o .: "proposals"
            <*> o .: "nextSeq"

-- | Decoded GET /events response.
data GetEventResp = GetEventResp
    { erSigner :: Text
    , erEvent :: Value
    , erSignature :: Text
    }
    deriving stock (Show)

instance FromJSON GetEventResp where
    parseJSON = withObject "GetEventResp" $ \o ->
        GetEventResp
            <$> o .: "signer"
            <*> o .: "event"
            <*> o .: "signature"
