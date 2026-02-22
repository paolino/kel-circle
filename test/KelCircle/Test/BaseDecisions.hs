{- |
Module      : KelCircle.Test.BaseDecisions
Description : Properties for base decisions and genesis
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

QuickCheck properties mirroring the Lean theorems in
@KelCircle.BaseDecisions@: genesis, introduction, admin
exits bootstrap, sequencer protection, demotion.
-}
module KelCircle.Test.BaseDecisions
    ( tests
    ) where

import KelCircle.Events (BaseDecision (..))
import KelCircle.Gate (baseGate)
import KelCircle.Processing
    ( FullState (..)
    , applyBase
    , initFullState
    )
import KelCircle.State
    ( Circle (..)
    , applyBaseDecision
    , emptyCircle
    , isAdmin
    , isBootstrap
    , isMember
    )
import KelCircle.Test.Generators ()
import KelCircle.Types (MemberId (..), Role (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
    ( assertBool
    , testCase
    )
import Test.Tasty.QuickCheck (testProperty)

tests :: TestTree
tests =
    testGroup
        "BaseDecisions"
        [ testGenesis
        , testIntroduction
        , testBootstrapExit
        , testSequencerProtection
        , testDemotion
        ]

-- Genesis helpers
sid :: MemberId
sid = MemberId "sequencer-0"

aid :: MemberId
aid = MemberId "admin-0"

genesisCircle :: Circle
genesisCircle =
    applyBaseDecision
        ( Circle
            { circleState = emptyCircle
            , sequencerId = sid
            }
        )
        (IntroduceMember sid "sequencer" Member)

-- ---------------------------------------------------------
-- Genesis (mirrors Lean genesis_* theorems)
-- ---------------------------------------------------------

testGenesis :: TestTree
testGenesis =
    testGroup
        "genesis"
        [ testCase
            "sequencer is member after genesis"
            $ assertBool "" (isMember (circleState genesisCircle) sid)
        , testCase
            "sequencer is NOT admin after genesis"
            $ assertBool
                ""
                (not $ isAdmin (circleState genesisCircle) sid)
        , testCase
            "circle is in bootstrap after genesis"
            $ assertBool
                ""
                (isBootstrap (circleState genesisCircle))
        , testCase
            "sequencer id preserved"
            $ assertBool
                ""
                (sequencerId genesisCircle == sid)
        ]

-- ---------------------------------------------------------
-- Introduction (mirrors introduce_* theorems)
-- ---------------------------------------------------------

testIntroduction :: TestTree
testIntroduction =
    testGroup
        "introduce"
        [ testProperty
            "introduces member to circle"
            $ \mid' ->
                let c' =
                        applyBaseDecision
                            genesisCircle
                            (IntroduceMember mid' "" Member)
                in  isMember (circleState c') mid'
        , testProperty
            "preserves existing members"
            $ \mid' ->
                let c' =
                        applyBaseDecision
                            genesisCircle
                            (IntroduceMember mid' "" Member)
                in  isMember (circleState c') sid
        ]

-- ---------------------------------------------------------
-- Bootstrap exit (mirrors introduce_admin_exits_bootstrap)
-- ---------------------------------------------------------

testBootstrapExit :: TestTree
testBootstrapExit =
    testGroup
        "bootstrap exit"
        [ testProperty
            "introducing admin exits bootstrap"
            $ \mid' ->
                let c' =
                        applyBaseDecision
                            genesisCircle
                            (IntroduceMember mid' "" Admin)
                in  not (isBootstrap (circleState c'))
        ]

-- ---------------------------------------------------------
-- Sequencer protection (mirrors sequencer_removal_rejected,
-- sequencer_admin_promotion_rejected)
-- ---------------------------------------------------------

testSequencerProtection :: TestTree
testSequencerProtection =
    testGroup
        "sequencer protection"
        [ testCase
            "sequencer removal is always rejected by base gate"
            $ do
                let s0 = initFullState sid ()
                    s1 = applyBase s0 (IntroduceMember aid "" Admin)
                -- After exiting bootstrap, try to remove sequencer
                -- The base gate should reject this
                let gateResult =
                        baseGate
                            (circleState (fsCircle s1))
                            aid
                            sid
                            (RemoveMember sid)
                assertBool "should reject" (not gateResult)
        , testCase
            "sequencer promotion to admin is rejected"
            $ do
                let s0 = initFullState sid ()
                    s1 = applyBase s0 (IntroduceMember aid "" Admin)
                let gateResult =
                        baseGate
                            (circleState (fsCircle s1))
                            aid
                            sid
                            (ChangeRole sid Admin)
                assertBool "should reject" (not gateResult)
        ]

-- ---------------------------------------------------------
-- Demotion (mirrors demote_sole_admin_enters_bootstrap)
-- ---------------------------------------------------------

testDemotion :: TestTree
testDemotion =
    testGroup
        "demotion"
        [ testCase
            "demoting sole admin re-enters bootstrap"
            $ do
                let c0 = genesisCircle
                    c1 =
                        applyBaseDecision
                            c0
                            (IntroduceMember aid "" Admin)
                    c2 =
                        applyBaseDecision
                            c1
                            (ChangeRole aid Member)
                assertBool
                    "should be bootstrap"
                    (isBootstrap (circleState c2))
        , testCase
            "sequencer survives demotion"
            $ do
                let c0 = genesisCircle
                    c1 =
                        applyBaseDecision
                            c0
                            (IntroduceMember aid "" Admin)
                    c2 =
                        applyBaseDecision
                            c1
                            (ChangeRole aid Member)
                assertBool
                    "sequencer still member"
                    (isMember (circleState c2) sid)
        ]
