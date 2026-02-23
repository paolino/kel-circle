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

      -- * KEL state tracking
    , KelState (..)

      -- * Rotation helpers
    , postRotation
    , postRotationRaw

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

      -- * URL helpers
    , urlEncode

      -- * Response decoders
    , InfoResp (..)
    , ConditionResp (..)
    , ConditionMember (..)
    , GetEventResp (..)
    ) where

import Control.Concurrent.STM (newBroadcastTChanIO)
import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value
    , decode
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import KelCircle.Events
    ( BaseDecision (..)
    , CircleEvent (..)
    , Resolution
    )
import KelCircle.InteractionVerify (buildInteractionBytes)
import KelCircle.RotationVerify
    ( buildRotationBytes
    , rotationEventDigest
    )
import KelCircle.Server (ServerConfig (..), mkApp)
import KelCircle.Server.JSON
    ( AppendResult (..)
    , RotationSubmission (..)
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
import Keri.Event
    ( eventDigest
    , eventPrefix
    )
import Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    )
import Keri.Event.Interaction
    ( InteractionConfig (..)
    , mkInteraction
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

{- | Tracked KEL state for a test identity.
Updated after inception and each interaction event.
-}
data KelState = KelState
    { ksPrefix :: Text
    -- ^ KERI AID prefix (SAID)
    , ksSeqNum :: Int
    -- ^ Current last sequence number
    , ksLastDigest :: Text
    -- ^ SAID of last event
    }
    deriving stock (Show)

{- | A test identity backed by a real Ed25519 keypair.
The 'tidKey' is the CESR-encoded public key prefix.
'tidKelState' tracks the signer's KEL for building
interaction events.
-}
data TestId = TestId
    { tidKey :: Text
    -- ^ CESR-encoded Ed25519 public key prefix
    , tidKeyPair :: KeyPair
    -- ^ The underlying Ed25519 keypair
    , tidKelState :: IORef (Maybe KelState)
    -- ^ Tracked KEL state (set after inception)
    , tidNextKeyPair :: IORef (Maybe KeyPair)
    -- ^ Next keypair for pre-rotation commitment
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
    ref <- newIORef Nothing
    nextRef <- newIORef Nothing
    pure
        TestId
            { tidKey = cesrKey
            , tidKeyPair = kp
            , tidKelState = ref
            , tidNextKeyPair = nextRef
            }

{- | Create a test identity with a non-CESR key
string, for testing validation rejection. Uses a
real keypair internally (the key text is overridden).
-}
mkBadTestId :: Text -> IO TestId
mkBadTestId badKey = do
    kp <- generateKeyPair
    ref <- newIORef Nothing
    nextRef <- newIORef Nothing
    pure
        TestId
            { tidKey = badKey
            , tidKeyPair = kp
            , tidKelState = ref
            , tidNextKeyPair = nextRef
            }

-- --------------------------------------------------------
-- Inception helpers
-- --------------------------------------------------------

{- | Create a signed inception event for a test
identity. Returns a JSON 'Value' suitable for the
@inception@ field in a 'Submission'. Also initializes
the signer's KEL state (prefix, seqnum 0, digest).
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
                prefix' = eventPrefix evt
                digest = eventDigest evt
            -- Initialize KEL state for this identity
            writeIORef
                (tidKelState tid)
                ( Just
                    KelState
                        { ksPrefix = prefix'
                        , ksSeqNum = 0
                        , ksLastDigest = digest
                        }
                )
            -- Store next keypair for rotation
            writeIORef (tidNextKeyPair tid) (Just nextKp)
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
    , tsSignerExempt :: Bool
    -- ^ Sequencer is exempt from interaction signing
    }

{- | Sign a test submission. Builds the inception
event if one is configured, and signs the circle
event as a KERI interaction event against the
signer's current KEL state.
-}
signSubmission :: TestSub -> IO (Submission () () ())
signSubmission ts = do
    mInception <- case tsInception ts of
        Nothing -> pure Nothing
        Just mkInc -> Just <$> mkInc
    sig <-
        if tsSignerExempt ts
            then
                pure $
                    "dummy:" <> tidKey (tsSigner ts)
            else
                signInteraction
                    (tsSigner ts)
                    (tsEvent ts)
    pure
        Submission
            { subPassphrase = tsPassphrase ts
            , subSigner = tidKey (tsSigner ts)
            , subSignature = sig
            , subEvent = tsEvent ts
            , subInception = mInception
            }

{- | Sign the circle event as a KERI interaction
event. Reads the signer's KEL state, builds the
interaction bytes, signs with Ed25519, and updates
the KEL state with the new seqnum/digest.
-}
signInteraction
    :: TestId -> CircleEvent () () () -> IO Text
signInteraction tid evt = do
    mKs <- readIORef (tidKelState tid)
    case mKs of
        Nothing ->
            -- No KEL state: signer was never introduced.
            -- Produce a dummy signature; the server will
            -- reject for missing KEL or non-membership.
            pure $ "dummy:" <> tidKey tid
        Just ks -> do
            let nextSeq = ksSeqNum ks + 1
                anchor = toJSON evt
                ixnBytes =
                    buildInteractionBytes
                        (ksPrefix ks)
                        nextSeq
                        (ksLastDigest ks)
                        anchor
                sigBytes =
                    sign (tidKeyPair tid) ixnBytes
                sigCesr =
                    Keri.Cesr.Encode.encode $
                        Primitive
                            { code = Ed25519Sig
                            , raw = sigBytes
                            }
                -- Compute the new digest
                ixnEvt =
                    mkInteraction
                        InteractionConfig
                            { ixPrefix = ksPrefix ks
                            , ixSequenceNumber =
                                nextSeq
                            , ixPriorDigest =
                                ksLastDigest ks
                            , ixAnchors = [anchor]
                            }
                newDigest = eventDigest ixnEvt
            writeIORef
                (tidKelState tid)
                ( Just
                    ks
                        { ksSeqNum = nextSeq
                        , ksLastDigest = newDigest
                        }
                )
            pure sigCesr

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
        , tsSignerExempt = False
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
        , tsSignerExempt = False
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
        , tsSignerExempt = False
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
        , tsSignerExempt = False
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
        , tsSignerExempt = False
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
        , tsSignerExempt = False
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
        , tsSignerExempt = False
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
        , tsSignerExempt = True
        }

-- --------------------------------------------------------
-- Rotation helpers
-- --------------------------------------------------------

{- | POST a rotation and expect 200. Returns the new
KEL event count and a fresh 'TestId' with the rotated
keys (old TestId should not be used afterward).
-}
postRotation :: TestEnv -> TestId -> IO (Int, TestId)
postRotation te tid = do
    (body, newTid) <- mkRotationBody tid
    resp <-
        httpPost
            te
            ( "/members/"
                <> urlEncode (tidKey tid)
                <> "/rotate"
            )
            body
    assertEqual
        "POST rotate status"
        status200
        (HC.responseStatus resp)
    kr <- decodeOrFail (HC.responseBody resp)
    pure (kelRotRespEventCount kr, newTid)

{- | POST a rotation and return raw response for
error-case tests.
-}
postRotationRaw
    :: TestEnv
    -> TestId
    -> IO (HC.Response LBS.ByteString)
postRotationRaw te tid = do
    (body, _) <- mkRotationBody tid
    httpPost
        te
        ( "/members/"
            <> urlEncode (tidKey tid)
            <> "/rotate"
        )
        body

{- | Build a rotation submission body. Returns the JSON
body and a new 'TestId' with updated keys/KEL state.
-}
mkRotationBody
    :: TestId -> IO (LBS.ByteString, TestId)
mkRotationBody tid = do
    mKs <- readIORef (tidKelState tid)
    mNextKp <- readIORef (tidNextKeyPair tid)
    case (mKs, mNextKp) of
        (Just ks, Just nextKp) -> do
            -- The new signing key is the pre-committed one
            let newCesr =
                    Keri.Cesr.Encode.encode $
                        Primitive
                            { code = Ed25519PubKey
                            , raw =
                                publicKeyBytes
                                    (publicKey nextKp)
                            }
            -- Generate fresh next-next keypair
            nextNextKp <- generateKeyPair
            let nextNextCesr =
                    Keri.Cesr.Encode.encode $
                        Primitive
                            { code = Ed25519PubKey
                            , raw =
                                publicKeyBytes
                                    (publicKey nextNextKp)
                            }
            case commitKey nextNextCesr of
                Left err ->
                    error $
                        "commitKey failed: " <> err
                Right commitment -> do
                    let nextSeq = ksSeqNum ks + 1
                        rotBytes =
                            buildRotationBytes
                                (ksPrefix ks)
                                nextSeq
                                (ksLastDigest ks)
                                [newCesr]
                                1
                                [commitment]
                                1
                        -- OLD keys sign the rotation
                        sigBytes =
                            sign
                                (tidKeyPair tid)
                                rotBytes
                        sigCesr =
                            Keri.Cesr.Encode.encode $
                                Primitive
                                    { code = Ed25519Sig
                                    , raw = sigBytes
                                    }
                        rotSub =
                            RotationSubmission
                                { rsNewKeys = [newCesr]
                                , rsNewThreshold = 1
                                , rsNextKeyCommitments =
                                    [commitment]
                                , rsNextThreshold = 1
                                , rsSignatures =
                                    [(0, sigCesr)]
                                }
                        -- Compute rotation digest
                        newDigest =
                            rotationEventDigest
                                (ksPrefix ks)
                                nextSeq
                                (ksLastDigest ks)
                                [newCesr]
                                1
                                [commitment]
                                1
                    -- Build new TestId with rotated keys
                    kelRef <-
                        newIORef $
                            Just
                                ks
                                    { ksSeqNum = nextSeq
                                    , ksLastDigest =
                                        newDigest
                                    }
                    nextRef <-
                        newIORef (Just nextNextKp)
                    let newTid =
                            TestId
                                { tidKey = tidKey tid
                                , tidKeyPair = nextKp
                                , tidKelState = kelRef
                                , tidNextKeyPair =
                                    nextRef
                                }
                    pure
                        ( Aeson.encode rotSub
                        , newTid
                        )
        _ ->
            error
                "mkRotationBody: no KEL state \
                \or next keypair"

-- | URL-encode text for path segments.
urlEncode :: Text -> String
urlEncode = concatMap encodeChar . T.unpack
  where
    encodeChar '+' = "%2B"
    encodeChar '/' = "%2F"
    encodeChar '=' = "%3D"
    encodeChar c = [c]

-- | Decoded rotation response.
newtype KelRotResp = KelRotResp
    { kelRotRespEventCount :: Int
    }
    deriving stock (Show)

instance FromJSON KelRotResp where
    parseJSON = withObject "KelRotResp" $ \o ->
        KelRotResp <$> o .: "eventCount"

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
