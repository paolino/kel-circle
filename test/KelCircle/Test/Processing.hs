{- |
Module      : KelCircle.Test.Processing
Description : Properties for event processing pipeline
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

QuickCheck properties mirroring the Lean theorems in
@KelCircle.Processing@: sequence number increments,
circle preservation, proposal registry preservation,
gate/sequencer interaction.
-}
module KelCircle.Test.Processing
    ( tests
    ) where

import KelCircle.Events
    ( BaseDecision (..)
    , Resolution (..)
    )
import KelCircle.Processing
    ( FullState (..)
    , applyAppDecision
    , applyBase
    , applyProposal
    , applyResolve
    , applyResponse
    , gateProposal
    , gateResolve
    , gateResponse
    , initFullState
    )
import KelCircle.State (Circle (..), isMember)
import KelCircle.Test.Generators ()
import KelCircle.Types (MemberId (..), Role (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
    ( assertBool
    , assertEqual
    , testCase
    )
import Test.Tasty.QuickCheck (testProperty, (==>))

tests :: TestTree
tests =
    testGroup
        "Processing"
        [ testSeqIncrements
        , testCirclePreservation
        , testProposalPreservation
        , testGateSequencer
        , testGenesisState
        , testResponseInvariants
        , testProposalInvariants
        ]

sid :: MemberId
sid = MemberId "sequencer-0"

aid :: MemberId
aid = MemberId "admin-0"

s0 :: FullState () () ()
s0 = initFullState sid ()

mid :: MemberId
mid = MemberId "member-0"

s1 :: FullState () () ()
s1 = applyBase s0 (IntroduceMember aid "" Admin)

-- s1 with a non-admin member introduced
s2 :: FullState () () ()
s2 = applyBase s1 (IntroduceMember mid "" Member)

-- ---------------------------------------------------------
-- Sequence increments (mirrors apply_*_increments_seq)
-- ---------------------------------------------------------

testSeqIncrements :: TestTree
testSeqIncrements =
    testGroup
        "sequence increments"
        [ testCase "base increments" $
            assertEqual
                ""
                (fsNextSeq s0 + 1)
                ( fsNextSeq $
                    applyBase
                        s0
                        (IntroduceMember aid "" Admin)
                )
        , testCase "app decision increments" $
            assertEqual
                ""
                (fsNextSeq s1 + 1)
                ( fsNextSeq $
                    applyAppDecision
                        s1
                        ()
                        (\_ _ -> ())
                )
        , testCase "proposal increments" $
            assertEqual
                ""
                (fsNextSeq s1 + 1)
                ( fsNextSeq $
                    applyProposal
                        s1
                        ()
                        aid
                        1000
                )
        , testCase "response increments" $
            assertEqual
                ""
                (fsNextSeq s1 + 1)
                ( fsNextSeq $
                    applyResponse
                        s1
                        ()
                        aid
                        0
                )
        , testCase "resolve increments" $
            assertEqual
                ""
                (fsNextSeq s1 + 1)
                ( fsNextSeq $
                    applyResolve
                        s1
                        0
                        ThresholdReached
                )
        ]

-- ---------------------------------------------------------
-- Circle preservation (mirrors
-- app_decision_preserves_circle, proposal_preserves_circle,
-- response_preserves_circle, resolve_preserves_circle)
-- ---------------------------------------------------------

testCirclePreservation :: TestTree
testCirclePreservation =
    testGroup
        "circle preservation"
        [ testCase "app decision preserves circle" $
            assertEqual
                ""
                (fsCircle s1)
                ( fsCircle $
                    applyAppDecision
                        s1
                        ()
                        (\_ _ -> ())
                )
        , testCase "proposal preserves circle" $
            assertEqual
                ""
                (fsCircle s1)
                ( fsCircle $
                    applyProposal
                        s1
                        ()
                        aid
                        1000
                )
        , testCase "response preserves circle" $
            assertEqual
                ""
                (fsCircle s1)
                ( fsCircle $
                    applyResponse
                        s1
                        ()
                        aid
                        0
                )
        , testCase "resolve preserves circle" $
            assertEqual
                ""
                (fsCircle s1)
                ( fsCircle $
                    applyResolve
                        s1
                        0
                        ThresholdReached
                )
        ]

-- ---------------------------------------------------------
-- Proposal registry preservation (mirrors
-- base_preserves_proposals,
-- app_decision_preserves_proposals)
-- ---------------------------------------------------------

testProposalPreservation :: TestTree
testProposalPreservation =
    testGroup
        "proposal preservation"
        [ testCase "base preserves proposals" $
            assertEqual
                ""
                (fsProposals s1)
                ( fsProposals $
                    applyBase
                        s1
                        (IntroduceMember (MemberId "new") "" Member)
                )
        , testCase "app decision preserves proposals" $
            assertEqual
                ""
                (fsProposals s1)
                ( fsProposals $
                    applyAppDecision
                        s1
                        ()
                        (\_ _ -> ())
                )
        ]

-- ---------------------------------------------------------
-- Gate/sequencer interaction (mirrors
-- non_sequencer_cannot_resolve, sequencer_can_resolve)
-- ---------------------------------------------------------

testGateSequencer :: TestTree
testGateSequencer =
    testGroup
        "gate/sequencer"
        [ testProperty
            "non-sequencer cannot resolve"
            $ \signer ->
                signer /= sid ==>
                    not (gateResolve s1 signer)
        , testCase "sequencer can resolve" $
            assertBool
                ""
                (gateResolve s1 sid)
        ]

-- ---------------------------------------------------------
-- Genesis state (mirrors init_seq_is_one,
-- init_no_proposals, init_sequencer_is_member)
-- ---------------------------------------------------------

testGenesisState :: TestTree
testGenesisState =
    testGroup
        "genesis state"
        [ testCase "initial seq is 1" $
            assertEqual "" 1 (fsNextSeq s0)
        , testCase "initial has no proposals" $
            assertEqual "" [] (fsProposals s0)
        , testProperty
            "initial sequencer is member"
            $ \sid' ->
                let s = initFullState sid' () :: FullState () () ()
                in  isMember
                        (circleState (fsCircle s))
                        sid'
        ]

-- ---------------------------------------------------------
-- Response invariants (mirrors gateResponse_requires_admin)
-- ---------------------------------------------------------

testResponseInvariants :: TestTree
testResponseInvariants =
    testGroup
        "response invariants"
        [ testCase "non-admin response rejected" $ do
            -- Open a proposal so there's something to
            -- respond to
            let sWithProp =
                    applyProposal s2 () aid 9999
                pid = fsNextSeq s2
            assertBool
                "non-admin should be rejected"
                (not (gateResponse sWithProp mid pid))
        , testCase "admin response accepted" $ do
            let sWithProp =
                    applyProposal s2 () aid 9999
                pid = fsNextSeq s2
            assertBool
                "admin should be accepted"
                (gateResponse sWithProp aid pid)
        ]

-- ---------------------------------------------------------
-- Proposal invariants (mirrors
-- gateProposal_rejects_duplicate)
-- ---------------------------------------------------------

testProposalInvariants :: TestTree
testProposalInvariants =
    testGroup
        "proposal invariants"
        [ testCase "duplicate proposal rejected" $ do
            let sWithProp =
                    applyProposal s1 () aid 9999
            assertBool
                "duplicate should be rejected"
                ( not
                    ( gateProposal
                        sWithProp
                        aid
                        ()
                        (\_ _ -> True)
                    )
                )
        , testCase "different content accepted" $ do
            -- Use Int proposals to test different
            -- content
            let s :: FullState () Int ()
                s = initFullState sid ()
                s' =
                    applyBase
                        s
                        (IntroduceMember aid "" Admin)
                sWithProp =
                    applyProposal s' (1 :: Int) aid 9999
            assertBool
                "different content should pass"
                ( gateProposal
                    sWithProp
                    aid
                    (2 :: Int)
                    (\_ _ -> True)
                )
        ]
