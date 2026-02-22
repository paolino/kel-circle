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

import Data.Aeson (encode)
import E2E.TestHelpers
import KelCircle.Events (Resolution (..))
import KelCircle.Server.JSON (Submission (..))
import KelCircle.Types (Role (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types
    ( status200
    , status401
    , status422
    )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

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
    let sub = signSubmission (bootstrapAdmin admin1)
        sub' = sub{subPassphrase = Nothing}
    resp <- httpPost te "/events" (encode sub')
    HC.responseStatus resp @?= status401

testBootstrapWrongPass :: IO ()
testBootstrapWrongPass = withTestEnv $ \te -> do
    admin1 <- newTestId
    let sub = signSubmission (bootstrapAdmin admin1)
        sub' =
            sub{subPassphrase = Just "wrong"}
    resp <- httpPost te "/events" (encode sub')
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
    let seqId =
            TestId{tidKey = "server-sequencer"}
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

    -- Read third event
    e2 <- httpGet te "/events?after=1"
    HC.responseStatus e2 @?= status200
