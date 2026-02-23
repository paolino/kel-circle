{- |
Module      : E2E.Spec
Description : End-to-end scenarios through the HTTP API
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Multi-step workflows exercising the full circle
lifecycle via HTTP calls: bootstrap, member management,
proposals, responses, and resolution.
-}
module E2E.Spec (tests) where

import Data.Aeson (FromJSON (..), Value (..), withObject, (.:))
import Data.Aeson qualified as Aeson
import Data.IORef qualified
import Data.Text (Text)
import Data.Text qualified as T
import E2E.TestHelpers
import KelCircle.Events
    ( BaseDecision (..)
    , CircleEvent (..)
    , Resolution (..)
    )
import KelCircle.Server.JSON (Submission (..))
import KelCircle.Types (MemberId (..), Role (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types
    ( status200
    , status401
    , status404
    , status422
    )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- | All E2E test scenarios.
tests :: TestTree
tests =
    testGroup
        "E2E scenarios"
        [ bootstrapTests
        , memberTests
        , roleTests
        , proposalTests
        , authTests
        , rebootstrapTests
        , eventReplayTests
        , cesrValidationTests
        , memberKelTests
        , interactionVerificationTests
        ]

-- --------------------------------------------------------
-- Bootstrap
-- --------------------------------------------------------

bootstrapTests :: TestTree
bootstrapTests =
    testGroup
        "Bootstrap"
        [ testCase
            "bootstrap first admin"
            testBootstrapFirstAdmin
        , testCase
            "bootstrap rejects missing passphrase"
            testBootstrapMissingPass
        , testCase
            "bootstrap rejects wrong passphrase"
            testBootstrapWrongPass
        ]

testBootstrapFirstAdmin :: IO ()
testBootstrapFirstAdmin = withTestEnv $ \te -> do
    -- Initially in bootstrap mode
    info0 <- getInfo te
    irAuthMode info0 @?= "bootstrap"
    irMemberCount info0 @?= 1 -- server-sequencer
    admin1 <- newTestId
    sn <- postEvent te (bootstrapAdmin admin1)
    -- First appended event = sequence 1
    sn @?= 1

    info1 <- getInfo te
    irAuthMode info1 @?= "normal"
    irMemberCount info1 @?= 2 -- sequencer + admin

testBootstrapMissingPass :: IO ()
testBootstrapMissingPass = withTestEnv $ \te -> do
    admin1 <- newTestId
    sub <- signSubmission (bootstrapAdmin admin1)
    let sub' = sub{subPassphrase = Nothing}
    resp <- httpPost te "/events" (Aeson.encode sub')
    HC.responseStatus resp @?= status401

testBootstrapWrongPass :: IO ()
testBootstrapWrongPass = withTestEnv $ \te -> do
    admin1 <- newTestId
    sub <- signSubmission (bootstrapAdmin admin1)
    let sub' =
            sub{subPassphrase = Just "wrong"}
    resp <- httpPost te "/events" (Aeson.encode sub')
    HC.responseStatus resp @?= status401

-- --------------------------------------------------------
-- Member management
-- --------------------------------------------------------

memberTests :: TestTree
memberTests =
    testGroup
        "Member management"
        [ testCase
            "admin introduces member"
            testIntroduceMember
        , testCase
            "admin removes member"
            testRemoveMember
        , testCase
            "non-admin cannot introduce"
            testNonAdminCannotIntroduce
        , testCase
            "non-member cannot introduce"
            testNonMemberCannotIntroduce
        , testCase
            "duplicate member rejected"
            testDuplicateMember
        , testCase
            "sequencer cannot be removed"
            testCannotRemoveSequencer
        , testCase
            "non-member cannot be removed"
            testCannotRemoveNonMember
        ]

testIntroduceMember :: IO ()
testIntroduceMember = withTestEnv $ \te -> do
    admin1 <- newTestId
    user1 <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)
    _ <- postEvent te (introduceMember admin1 user1)

    cond <- getCondition te
    length (crMembers cond) @?= 3

testRemoveMember :: IO ()
testRemoveMember = withTestEnv $ \te -> do
    admin1 <- newTestId
    user1 <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)
    _ <- postEvent te (introduceMember admin1 user1)

    cond0 <- getCondition te
    length (crMembers cond0) @?= 3

    _ <- postEvent te (removeMember admin1 user1)

    cond1 <- getCondition te
    length (crMembers cond1) @?= 2

testNonAdminCannotIntroduce :: IO ()
testNonAdminCannotIntroduce =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId
        user2 <- newTestId

        _ <- postEvent te (bootstrapAdmin admin1)
        _ <-
            postEvent
                te
                (introduceMember admin1 user1)

        resp <-
            postEventRaw
                te
                (introduceMember user1 user2)
        HC.responseStatus resp @?= status422

testNonMemberCannotIntroduce :: IO ()
testNonMemberCannotIntroduce =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        nobody <- newTestId
        user1 <- newTestId

        _ <- postEvent te (bootstrapAdmin admin1)

        resp <-
            postEventRaw
                te
                (introduceMember nobody user1)
        HC.responseStatus resp @?= status422

testCannotRemoveSequencer :: IO ()
testCannotRemoveSequencer = withTestEnv $ \te -> do
    admin1 <- newTestId
    seqId <- mkBadTestId "server-sequencer"

    _ <- postEvent te (bootstrapAdmin admin1)

    -- Admin tries to remove the sequencer
    resp <- postEventRaw te (removeMember admin1 seqId)
    HC.responseStatus resp @?= status422

testCannotRemoveNonMember :: IO ()
testCannotRemoveNonMember = withTestEnv $ \te -> do
    admin1 <- newTestId
    nobody <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)

    -- Try to remove someone who was never introduced
    resp <- postEventRaw te (removeMember admin1 nobody)
    HC.responseStatus resp @?= status422

testDuplicateMember :: IO ()
testDuplicateMember = withTestEnv $ \te -> do
    admin1 <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)

    -- Try to introduce admin1 again
    resp <-
        postEventRaw
            te
            (introduceAdmin admin1 admin1)
    -- Should be rejected by the base gate
    HC.responseStatus resp @?= status422

-- --------------------------------------------------------
-- Role changes
-- --------------------------------------------------------

roleTests :: TestTree
roleTests =
    testGroup
        "Role changes"
        [ testCase
            "role change requires proposal (rejected as straight decision)"
            testRoleChangeRequiresProposal
        , testCase
            "admin introduction requires proposal in normal mode"
            testAdminIntroRequiresProposal
        ]

testRoleChangeRequiresProposal :: IO ()
testRoleChangeRequiresProposal =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId

        _ <- postEvent te (bootstrapAdmin admin1)
        _ <-
            postEvent
                te
                (introduceMember admin1 user1)

        -- ChangeRole requires majority,
        -- rejected as straight decision
        resp <-
            postEventRaw
                te
                (changeRole admin1 user1 Admin)
        HC.responseStatus resp @?= status422

testAdminIntroRequiresProposal :: IO ()
testAdminIntroRequiresProposal =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        admin2 <- newTestId

        _ <- postEvent te (bootstrapAdmin admin1)

        -- IntroduceMember _ Admin requires majority
        -- in normal mode
        resp <-
            postEventRaw
                te
                (introduceAdmin admin1 admin2)
        HC.responseStatus resp @?= status422

-- --------------------------------------------------------
-- Proposals
-- --------------------------------------------------------

proposalTests :: TestTree
proposalTests =
    testGroup
        "Proposals"
        [ testCase
            "open proposal and respond"
            testProposalAndResponse
        , testCase
            "resolve proposal"
            testResolveProposal
        ]

testProposalAndResponse :: IO ()
testProposalAndResponse = withTestEnv $ \te -> do
    admin1 <- newTestId
    user1 <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)
    _ <- postEvent te (introduceMember admin1 user1)

    -- Admin opens a proposal (deadline = 9999)
    proposalSn <-
        postEvent te (submitProposal admin1 9999)
    -- proposal id = sequence number when opened
    let pid = proposalSn

    -- User responds
    _ <-
        postEvent
            te
            (respondToProposal user1 pid)

    cond <- getCondition te
    length (crProposals cond) @?= 1

testResolveProposal :: IO ()
testResolveProposal = withTestEnv $ \te -> do
    -- The sequencer can resolve proposals
    seqId <- mkBadTestId "server-sequencer"
    admin1 <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)

    proposalSn <-
        postEvent te (submitProposal admin1 9999)
    let pid = proposalSn

    _ <-
        postEvent
            te
            ( resolveProposal
                seqId
                pid
                ProposerPositive
            )

    cond <- getCondition te
    -- Proposal still in list (resolved)
    length (crProposals cond) @?= 1

-- --------------------------------------------------------
-- Auth edge cases
-- --------------------------------------------------------

authTests :: TestTree
authTests =
    testGroup
        "Authorization"
        [ testCase
            "after bootstrap, passphrase not needed"
            testNormalNoPass
        ]

testNormalNoPass :: IO ()
testNormalNoPass = withTestEnv $ \te -> do
    admin1 <- newTestId
    user1 <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)

    -- Normal mode: passphrase not needed
    sn <-
        postEvent
            te
            (introduceMember admin1 user1)
    sn @?= 2

-- --------------------------------------------------------
-- Re-bootstrap
-- --------------------------------------------------------

rebootstrapTests :: TestTree
rebootstrapTests =
    testGroup
        "Re-bootstrap"
        [ testCase
            "removing last admin allows re-bootstrap"
            testRebootstrap
        ]

testRebootstrap :: IO ()
testRebootstrap = withTestEnv $ \te -> do
    admin1 <- newTestId
    admin2 <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)

    info0 <- getInfo te
    irAuthMode info0 @?= "normal"

    -- Remove the admin
    _ <- postEvent te (removeMember admin1 admin1)

    info1 <- getInfo te
    -- Should still be normal because
    -- server-sequencer is a Member (from genesis)
    -- but no Admin exists
    irAuthMode info1 @?= "bootstrap"

    -- New admin can bootstrap
    _ <- postEvent te (bootstrapAdmin admin2)

    info2 <- getInfo te
    irAuthMode info2 @?= "normal"

-- --------------------------------------------------------
-- Event replay
-- --------------------------------------------------------

eventReplayTests :: TestTree
eventReplayTests =
    testGroup
        "Event replay"
        [ testCase
            "GET /events replays from offset"
            testEventReplay
        , testCase
            "GET /events returns event as JSON object"
            testGetEventJsonDecoding
        ]

testEventReplay :: IO ()
testEventReplay = withTestEnv $ \te -> do
    admin1 <- newTestId
    user1 <- newTestId

    _ <- postEvent te (bootstrapAdmin admin1)
    _ <- postEvent te (introduceMember admin1 user1)

    -- Read first event (after=-1 means from id=0)
    e0 <- httpGet te "/events?after=-1"
    HC.responseStatus e0 @?= status200

    -- Read second event
    e1 <- httpGet te "/events?after=0"
    HC.responseStatus e1 @?= status200

    -- No third event (only 2 events exist)
    e2 <- httpGet te "/events?after=1"
    HC.responseStatus e2 @?= status404

testGetEventJsonDecoding :: IO ()
testGetEventJsonDecoding = withTestEnv $ \te -> do
    admin1 <- newTestId

    -- One bootstrap stores exactly one event
    sn <- postEvent te (bootstrapAdmin admin1)
    sn @?= 1

    -- Fetch the first event
    resp <- httpGet te "/events?after=-1"
    HC.responseStatus resp @?= status200
    er <- decodeOrFail (HC.responseBody resp)
    -- event field must be a JSON object, not a string
    case erEvent er of
        Object _ -> pure ()
        other ->
            assertFailure $
                "expected JSON object, got: " <> show other

    -- No second event should exist
    resp2 <- httpGet te "/events?after=1"
    HC.responseStatus resp2 @?= status404

-- --------------------------------------------------------
-- CESR validation
-- --------------------------------------------------------

cesrValidationTests :: TestTree
cesrValidationTests =
    testGroup
        "CESR validation"
        [ testCase
            "non-CESR member ID rejected"
            testNonCesrMemberIdRejected
        , testCase
            "valid CESR member ID accepted"
            testValidCesrMemberIdAccepted
        , testCase
            "bootstrap with non-CESR admin ID rejected"
            testBootstrapNonCesrRejected
        ]

testNonCesrMemberIdRejected :: IO ()
testNonCesrMemberIdRejected =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        badUser <- mkBadTestId "garbage-not-cesr"

        _ <- postEvent te (bootstrapAdmin admin1)

        -- Try to introduce with non-CESR member ID
        resp <-
            postEventRaw
                te
                (introduceMember admin1 badUser)
        HC.responseStatus resp @?= status422

testValidCesrMemberIdAccepted :: IO ()
testValidCesrMemberIdAccepted =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId

        _ <- postEvent te (bootstrapAdmin admin1)
        _ <-
            postEvent
                te
                (introduceMember admin1 user1)

        cond <- getCondition te
        -- sequencer + admin + user
        length (crMembers cond) @?= 3

testBootstrapNonCesrRejected :: IO ()
testBootstrapNonCesrRejected =
    withTestEnv $ \te -> do
        badAdmin <-
            mkBadTestId "not-a-cesr-prefix"

        resp <-
            postEventRaw
                te
                (bootstrapAdmin badAdmin)
        HC.responseStatus resp @?= status422

-- --------------------------------------------------------
-- Member KELs
-- --------------------------------------------------------

memberKelTests :: TestTree
memberKelTests =
    testGroup
        "Member KELs"
        [ testCase
            "bootstrap admin has KEL"
            testBootstrapAdminHasKel
        , testCase
            "introduced member has KEL"
            testIntroducedMemberHasKel
        , testCase
            "IntroduceMember without inception rejected"
            testMissingInceptionRejected
        , testCase
            "inception with wrong key rejected"
            testWrongKeyInceptionRejected
        ]

interactionVerificationTests :: TestTree
interactionVerificationTests =
    testGroup
        "Interaction verification"
        [ testCase
            "valid signature accepted, KEL grows"
            testValidInteractionSig
        , testCase
            "forged signature rejected"
            testForgedSignatureRejected
        , testCase
            "wrong-key signature rejected"
            testWrongKeySignatureRejected
        , testCase
            "tampered payload rejected"
            testTamperedPayloadRejected
        , testCase
            "member KEL grows with each event"
            testKelGrowsWithEvents
        ]

testBootstrapAdminHasKel :: IO ()
testBootstrapAdminHasKel = withTestEnv $ \te -> do
    admin1 <- newTestId
    _ <- postEvent te (bootstrapAdmin admin1)

    -- Check KEL exists via GET endpoint
    -- 2 events: inception (seqnum 0) + interaction (seqnum 1)
    resp <-
        httpGet
            te
            ( "/members/"
                <> urlEncode (tidKey admin1)
                <> "/kel"
            )
    HC.responseStatus resp @?= status200
    kelResp <- decodeOrFail (HC.responseBody resp)
    kelRespEventCount kelResp @?= 2

testIntroducedMemberHasKel :: IO ()
testIntroducedMemberHasKel =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId
        _ <- postEvent te (bootstrapAdmin admin1)
        _ <-
            postEvent
                te
                (introduceMember admin1 user1)

        resp <-
            httpGet
                te
                ( "/members/"
                    <> urlEncode (tidKey user1)
                    <> "/kel"
                )
        HC.responseStatus resp @?= status200
        kelResp <-
            decodeOrFail (HC.responseBody resp)
        kelRespEventCount kelResp @?= 1

testMissingInceptionRejected :: IO ()
testMissingInceptionRejected =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId
        _ <- postEvent te (bootstrapAdmin admin1)

        -- Submit IntroduceMember without inception
        let noInception =
                TestSub
                    { tsSigner = admin1
                    , tsPassphrase = Nothing
                    , tsEvent =
                        CEBaseDecision $
                            IntroduceMember
                                (MemberId $ tidKey user1)
                                (tidKey user1)
                                Member
                    , tsInception = Nothing
                    , tsSignerExempt = False
                    }
        resp <- postEventRaw te noInception
        HC.responseStatus resp @?= status422

testWrongKeyInceptionRejected :: IO ()
testWrongKeyInceptionRejected =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId
        wrongUser <- newTestId
        _ <- postEvent te (bootstrapAdmin admin1)

        -- user1's inception signed by wrongUser's key
        let wrongInception =
                TestSub
                    { tsSigner = admin1
                    , tsPassphrase = Nothing
                    , tsEvent =
                        CEBaseDecision $
                            IntroduceMember
                                (MemberId $ tidKey user1)
                                (tidKey user1)
                                Member
                    , tsInception =
                        Just
                            (mkInceptionFor wrongUser)
                    , tsSignerExempt = False
                    }
        resp <- postEventRaw te wrongInception
        HC.responseStatus resp @?= status422

-- --------------------------------------------------------
-- Interaction verification tests
-- --------------------------------------------------------

testValidInteractionSig :: IO ()
testValidInteractionSig = withTestEnv $ \te -> do
    admin1 <- newTestId
    user1 <- newTestId
    _ <- postEvent te (bootstrapAdmin admin1)
    _ <- postEvent te (introduceMember admin1 user1)

    -- Admin's KEL: inception (0) + bootstrap ixn (1)
    -- + introduce ixn (2) = 3 events
    resp <-
        httpGet
            te
            ( "/members/"
                <> urlEncode (tidKey admin1)
                <> "/kel"
            )
    HC.responseStatus resp @?= status200
    kelResp <- decodeOrFail (HC.responseBody resp)
    kelRespEventCount kelResp @?= 3

testForgedSignatureRejected :: IO ()
testForgedSignatureRejected =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId
        _ <- postEvent te (bootstrapAdmin admin1)
        _ <-
            postEvent
                te
                (introduceMember admin1 user1)

        -- User submits with a forged (random) sig
        let forged =
                TestSub
                    { tsSigner = user1
                    , tsPassphrase = Nothing
                    , tsEvent = CEProposal () 9999
                    , tsInception = Nothing
                    , tsSignerExempt = True
                    }
        sub <- signSubmission forged
        let badSub =
                sub
                    { subSignature =
                        T.replicate 88 "A"
                    }
        resp <-
            httpPost
                te
                "/events"
                (Aeson.encode badSub)
        HC.responseStatus resp @?= status422

testWrongKeySignatureRejected :: IO ()
testWrongKeySignatureRejected =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId
        _ <- postEvent te (bootstrapAdmin admin1)
        _ <-
            postEvent
                te
                (introduceMember admin1 user1)

        -- Build a valid-looking interaction sig but
        -- with a different keypair
        imposter <- newTestId
        -- Give imposter user1's KEL state
        mKs <-
            Data.IORef.readIORef (tidKelState user1)
        Data.IORef.writeIORef
            (tidKelState imposter)
            mKs
        let wrongKey =
                TestSub
                    { tsSigner = imposter
                    , tsPassphrase = Nothing
                    , tsEvent = CEProposal () 9999
                    , tsInception = Nothing
                    , tsSignerExempt = False
                    }
        sub <- signSubmission wrongKey
        -- Submit as user1 but with imposter's sig
        let badSub =
                sub{subSigner = tidKey user1}
        resp <-
            httpPost
                te
                "/events"
                (Aeson.encode badSub)
        HC.responseStatus resp @?= status422

testTamperedPayloadRejected :: IO ()
testTamperedPayloadRejected =
    withTestEnv $ \te -> do
        admin1 <- newTestId
        user1 <- newTestId
        _ <- postEvent te (bootstrapAdmin admin1)
        _ <-
            postEvent
                te
                (introduceMember admin1 user1)

        -- Sign a proposal but submit a different event
        let original =
                TestSub
                    { tsSigner = user1
                    , tsPassphrase = Nothing
                    , tsEvent = CEProposal () 9999
                    , tsInception = Nothing
                    , tsSignerExempt = False
                    }
        sub <- signSubmission original
        -- Tamper: change the event payload
        let tampered :: Submission () () ()
            tampered =
                sub
                    { subEvent =
                        CEProposal () 1234
                    }
        resp <-
            httpPost
                te
                "/events"
                (Aeson.encode tampered)
        HC.responseStatus resp @?= status422

testKelGrowsWithEvents :: IO ()
testKelGrowsWithEvents = withTestEnv $ \te -> do
    admin1 <- newTestId
    user1 <- newTestId
    user2 <- newTestId
    _ <- postEvent te (bootstrapAdmin admin1)
    -- KEL: icp(0) + ixn(1) = 2
    _ <- postEvent te (introduceMember admin1 user1)
    -- KEL: +ixn(2) = 3
    _ <- postEvent te (introduceMember admin1 user2)
    -- KEL: +ixn(3) = 4
    _ <- postEvent te (removeMember admin1 user2)
    -- KEL: +ixn(4) = 5

    resp <-
        httpGet
            te
            ( "/members/"
                <> urlEncode (tidKey admin1)
                <> "/kel"
            )
    HC.responseStatus resp @?= status200
    kelResp <- decodeOrFail (HC.responseBody resp)
    kelRespEventCount kelResp @?= 5

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

-- | URL-encode text for path segments.
urlEncode :: Text -> String
urlEncode = concatMap encodeChar . T.unpack
  where
    encodeChar '+' = "%2B"
    encodeChar '/' = "%2F"
    encodeChar '=' = "%3D"
    encodeChar c = [c]

-- | Decoded KEL response.
newtype KelResp = KelResp
    { kelRespEventCount :: Int
    }
    deriving stock (Show)

instance FromJSON KelResp where
    parseJSON = withObject "KelResp" $ \o ->
        KelResp <$> o .: "eventCount"
