{- |
Module      : KelCircle.Test.Gate
Description : Properties for the two-level gate
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

QuickCheck properties mirroring the Lean theorems in
@KelCircle.BaseDecisions@ (gate section): bootstrap gate
behavior, full gate composition, admin majority.
-}
module KelCircle.Test.Gate
    ( tests
    ) where

import KelCircle.Events (BaseDecision (..))
import KelCircle.Gate
    ( baseGate
    , fullGate
    , hasAdminMajority
    , hasUniqueName
    )
import KelCircle.Processing
    ( FullState (..)
    , applyBase
    , initFullState
    )
import KelCircle.State
    ( Circle (..)
    , CircleState
    , applyBaseDecision
    , majority
    )
import KelCircle.Test.Generators ()
import KelCircle.Types (MemberId (..), Role (..))
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
        "Gate"
        [ testBootstrapGate
        , testFullGateComposition
        , testAdminMajority
        , testRotationGate
        , testNameUniqueness
        ]

sid :: MemberId
sid = MemberId "sequencer-0"

aid :: MemberId
aid = MemberId "admin-0"

bootstrapState :: CircleState
bootstrapState =
    circleState
        (fsCircle (initFullState sid () :: FullState () () ()))

normalState :: FullState () () ()
normalState =
    let s0 = initFullState sid ()
    in  applyBase s0 (IntroduceMember aid "" Admin)

-- ---------------------------------------------------------
-- Bootstrap gate (mirrors bootstrap_accepts_admin_intro,
-- bootstrap_rejects_member_intro, bootstrap_rejects_remove)
-- ---------------------------------------------------------

testBootstrapGate :: TestTree
testBootstrapGate =
    testGroup
        "bootstrap"
        [ testProperty
            "accepts admin introduction"
            $ \mid' ->
                baseGate
                    bootstrapState
                    sid
                    sid
                    (IntroduceMember mid' "" Admin)
        , testProperty
            "rejects member introduction"
            $ \mid' ->
                not $
                    baseGate
                        bootstrapState
                        sid
                        sid
                        (IntroduceMember mid' "" Member)
        , testCase
            "rejects removal"
            $ assertBool ""
            $ not
            $ baseGate
                bootstrapState
                sid
                sid
                (RemoveMember aid)
        ]

-- ---------------------------------------------------------
-- Full gate composition (mirrors full_gate_base_rejects,
-- full_gate_app_rejects)
-- ---------------------------------------------------------

testFullGateComposition :: TestTree
testFullGateComposition =
    testGroup
        "full gate"
        [ testCase
            "base rejection overrides app gate"
            $ do
                let s = circleState (fsCircle normalState)
                    -- RemoveMember of sequencer: base gate rejects
                    result =
                        fullGate
                            s
                            aid
                            sid
                            (RemoveMember sid)
                            ()
                            (\_ _ -> True)
                assertBool
                    "should reject"
                    (not result)
        , testCase
            "app rejection overrides base gate"
            $ do
                let s = circleState (fsCircle normalState)
                    -- valid base gate, but app rejects
                    result =
                        fullGate
                            s
                            aid
                            sid
                            (IntroduceMember (MemberId "new") "" Member)
                            ()
                            (\_ _ -> False)
                assertBool
                    "should reject"
                    (not result)
        ]

-- ---------------------------------------------------------
-- Admin majority (mirrors single_admin_majority,
-- single_admin_majority_self)
-- ---------------------------------------------------------

testAdminMajority :: TestTree
testAdminMajority =
    testGroup
        "admin majority"
        [ testCase "majority of 1 is 1" $
            assertEqual
                ""
                1
                (majority 1)
        , testCase
            "single admin meets majority with self"
            $ do
                let s = circleState (fsCircle normalState)
                assertBool "" $
                    hasAdminMajority s [aid]
        , testCase
            "empty respondents don't meet majority"
            $ do
                let s = circleState (fsCircle normalState)
                assertBool "" $
                    not (hasAdminMajority s [])
        ]

-- ---------------------------------------------------------
-- Rotation gate (mirrors rotate_accepted_by_admin)
-- ---------------------------------------------------------

testRotationGate :: TestTree
testRotationGate =
    testGroup
        "rotation"
        [ testProperty
            "admin can rotate sequencer"
            $ \newSid ->
                let s = circleState (fsCircle normalState)
                in  baseGate
                        s
                        aid
                        sid
                        (RotateSequencer newSid)
        ]

-- ---------------------------------------------------------
-- Name uniqueness (hasUniqueName enforced at gate level)
-- ---------------------------------------------------------

testNameUniqueness :: TestTree
testNameUniqueness =
    testGroup
        "name uniqueness"
        [ testCase
            "duplicate name rejected in bootstrap"
            $ do
                let s = bootstrapState
                    -- introduce first admin with name "alice"
                    s1 =
                        circleState $
                            applyBaseDecision
                                (Circle s sid)
                                (IntroduceMember (MemberId "alice-id") "alice" Admin)
                    -- second introduction with same name must be rejected
                    result =
                        hasUniqueName
                            s1
                            (IntroduceMember (MemberId "alice-id-2") "alice" Admin)
                assertBool "should reject duplicate name" (not result)
        , testCase
            "unique name accepted"
            $ do
                let s = bootstrapState
                    s1 =
                        circleState $
                            applyBaseDecision
                                (Circle s sid)
                                (IntroduceMember (MemberId "alice-id") "alice" Admin)
                    result =
                        hasUniqueName
                            s1
                            (IntroduceMember (MemberId "bob-id") "bob" Admin)
                assertBool "should accept unique name" result
        ]
