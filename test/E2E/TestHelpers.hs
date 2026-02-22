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

      -- * Signing
    , signSubmission

      -- * Response decoders
    , InfoResp (..)
    , ConditionResp (..)
    , ConditionMember (..)
    ) where

import Control.Concurrent.STM (newBroadcastTChanIO)
import Data.Aeson
    ( FromJSON (..)
    , Value
    , decode
    , encode
    , withObject
    , (.:)
    )
import Data.ByteString.Lazy qualified as LBS
import Data.IORef
    ( IORef
    , atomicModifyIORef'
    , newIORef
    )
import Data.Text (Text)
import Data.Text qualified as T
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
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (removeFile)
import System.IO.Temp (emptySystemTempFile)
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty.HUnit (assertEqual)

-- --------------------------------------------------------
-- Unique key generation
-- --------------------------------------------------------

{-# NOINLINE keyCounter #-}
keyCounter :: IORef Int
keyCounter = unsafePerformIO (newIORef 0)

-- | Generate a unique key string.
uniqueKey :: IO Text
uniqueKey = do
    n <-
        atomicModifyIORef'
            keyCounter
            (\i -> (i + 1, i))
    pure $ "test-key-" <> T.pack (show n)

-- --------------------------------------------------------
-- Test identities
-- --------------------------------------------------------

-- | A test identity with a unique key.
newtype TestId = TestId
    { tidKey :: Text
    -- ^ Unique public key string
    }
    deriving stock (Show, Eq)

-- | Generate a fresh test identity.
newTestId :: IO TestId
newTestId = TestId <$> uniqueKey

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
                }
    mgr <- HC.newManager HC.defaultManagerSettings
    result <-
        Warp.testWithApplication
            (pure $ mkApp cfg)
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
    let sub = signSubmission ts
    resp <- httpPost te "/events" (encode sub)
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
    let sub = signSubmission ts
    httpPost te "/events" (encode sub)

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
    }

{- | Sign a test submission. The signature is a
dummy value for now (no real Ed25519). The server
does not verify signatures in E2E tests.
-}
signSubmission :: TestSub -> Submission () () ()
signSubmission ts =
    Submission
        { subPassphrase = tsPassphrase ts
        , subSigner = tidKey (tsSigner ts)
        , subSignature =
            "sig:" <> tidKey (tsSigner ts)
        , subEvent = tsEvent ts
        }

-- --------------------------------------------------------
-- Submission builders
-- --------------------------------------------------------

-- | Bootstrap the first admin.
bootstrapAdmin :: TestId -> TestSub
bootstrapAdmin tid =
    TestSub
        { tsSigner = tid
        , tsPassphrase = Just testPass
        , tsEvent =
            CEBaseDecision $
                IntroduceMember
                    (MemberId $ tidKey tid)
                    Admin
        }

-- | Introduce a regular member.
introduceMember :: TestId -> TestId -> TestSub
introduceMember signer newMember =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent =
            CEBaseDecision $
                IntroduceMember
                    (MemberId $ tidKey newMember)
                    Member
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
                    Admin
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
        }

-- | Submit a proposal (trivial Unit content).
submitProposal
    :: TestId -> Timestamp -> TestSub
submitProposal signer deadline =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent = CEProposal () deadline
        }

-- | Respond to a proposal (trivial Unit content).
respondToProposal
    :: TestId -> ProposalId -> TestSub
respondToProposal signer pid =
    TestSub
        { tsSigner = signer
        , tsPassphrase = Nothing
        , tsEvent = CEResponse () pid
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
