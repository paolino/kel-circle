{- |
Module      : KelCircle.Test.Proposals
Description : Properties for proposal lifecycle
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

QuickCheck properties mirroring the Lean theorems in
@KelCircle.Proposals@: open, respond, resolve, timeout
obligation, resolution dichotomy.
-}
module KelCircle.Test.Proposals
    ( tests
    ) where

import KelCircle.Events
    ( Resolution (..)
    , isPositive
    )
import KelCircle.Proposals
    ( ProposalStatus (..)
    , TrackedProposal (..)
    , addResponse
    , canRespond
    , findProposal
    , isOpen
    , openProposal
    , resolveProposal
    )
import KelCircle.Test.Generators ()
import KelCircle.Types (MemberId (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
    ( assertBool
    , assertEqual
    , testCase
    )
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "Proposals"
        [ testOpenProposal
        , testResolve
        , testResolution
        , testResponseValidation
        ]

proposer :: MemberId
proposer = MemberId "proposer-0"

-- ---------------------------------------------------------
-- Open proposal (mirrors open_proposal_is_open,
-- open_proposal_no_responses)
-- ---------------------------------------------------------

testOpenProposal :: TestTree
testOpenProposal =
    testGroup
        "open"
        [ testCase "newly opened proposal is open" $
            do
                let reg = openProposal [] 0 () proposer 1000
                case reg of
                    (tp : _) ->
                        assertBool "" (isOpen (tpStatus tp))
                    [] -> assertBool "should have proposal" False
        , testCase "newly opened has no responses" $
            do
                let reg = openProposal [] 0 () proposer 1000
                case reg of
                    (tp : _) ->
                        assertEqual "" 0 (length (tpResponses tp))
                    [] -> assertBool "should have proposal" False
        ]

-- ---------------------------------------------------------
-- Resolve (mirrors resolve_resolved_noop,
-- resolved_satisfies_obligation)
-- ---------------------------------------------------------

testResolve :: TestTree
testResolve =
    testGroup
        "resolve"
        [ testCase "resolving already-resolved is noop" $
            do
                let reg0 = openProposal [] 0 () proposer 1000
                    reg1 =
                        resolveProposal
                            reg0
                            0
                            ThresholdReached
                    reg2 = resolveProposal reg1 0 Timeout
                -- Should still be ThresholdReached, not Timeout
                case findProposal reg2 0 of
                    Just tp ->
                        assertEqual
                            ""
                            (Resolved ThresholdReached)
                            (tpStatus tp)
                    Nothing ->
                        assertBool "should find proposal" False
        , testCase
            "resolved proposal satisfies timeout obligation"
            $ do
                let reg0 = openProposal [] 0 () proposer 1000
                    reg1 =
                        resolveProposal
                            reg0
                            0
                            Timeout
                case findProposal reg1 0 of
                    Just tp -> do
                        -- Resolved means isOpen is False
                        assertBool
                            "should not be open"
                            (not $ isOpen (tpStatus tp))
                    Nothing ->
                        assertBool "should find proposal" False
        ]

-- ---------------------------------------------------------
-- Resolution dichotomy (mirrors resolution_dichotomy,
-- threshold_is_positive, etc.)
-- ---------------------------------------------------------

testResolution :: TestTree
testResolution =
    testGroup
        "resolution"
        [ testProperty
            "every resolution is positive or negative"
            $ \r ->
                isPositive r || not (isPositive r)
        , testCase "threshold is positive" $
            assertBool "" (isPositive ThresholdReached)
        , testCase "proposer-positive is positive" $
            assertBool "" (isPositive ProposerPositive)
        , testCase "proposer-negative is negative" $
            assertBool "" (not $ isPositive ProposerNegative)
        , testCase "timeout is negative" $
            assertBool "" (not $ isPositive Timeout)
        ]

-- ---------------------------------------------------------
-- Response validation (mirrors
-- fresh_proposal_accepts_response, canRespond)
-- ---------------------------------------------------------

testResponseValidation :: TestTree
testResponseValidation =
    testGroup
        "response validation"
        [ testProperty
            "fresh proposal accepts any member"
            $ \m ->
                let tp =
                        TrackedProposal
                            { tpProposalId = 0
                            , tpContent = ()
                            , tpProposer = proposer
                            , tpDeadline = 1000
                            , tpResponses = [] :: [()]
                            , tpRespondents = []
                            , tpStatus = Open
                            }
                in  canRespond tp m
        , testCase
            "member cannot respond twice"
            $ do
                let responder = MemberId "resp-0"
                    reg0 = openProposal [] 0 () proposer 1000
                    reg1 =
                        addResponse
                            reg0
                            0
                            responder
                            ()
                case findProposal reg1 0 of
                    Just tp ->
                        assertBool
                            "should not accept second"
                            (not $ canRespond tp responder)
                    Nothing ->
                        assertBool "should find" False
        , testCase
            "resolved proposal rejects responses"
            $ do
                let reg0 = openProposal [] 0 () proposer 1000
                    reg1 =
                        resolveProposal
                            reg0
                            0
                            ThresholdReached
                case findProposal reg1 0 of
                    Just tp ->
                        assertBool
                            "should reject"
                            ( not $
                                canRespond
                                    tp
                                    (MemberId "any")
                            )
                    Nothing ->
                        assertBool "should find" False
        ]
