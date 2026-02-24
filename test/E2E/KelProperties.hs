{- |
Module      : E2E.KelProperties
Description : Property-based state machine tests
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

QuickCheck properties verifying state machine invariants
after random circle lifecycle sequences. Mirrors theorems
from the Lean formalization:

* KEL event count = 1 + signCount (MemberKel.lean)
* nextSeq = 1 + totalEventsPosted (Processing.lean)
* memberCount = 1 + activeMembers (BaseDecisions.lean)
* authMode = \"normal\" when admin exists (BaseDecisions.lean)
-}
module E2E.KelProperties (tests) where

import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import E2E.TestHelpers
import KelCircle.Events (Resolution (..))
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types (status404)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual)
import Test.Tasty.QuickCheck
    ( Gen
    , Property
    , choose
    , elements
    , forAllShrink
    , frequency
    , ioProperty
    , sublistOf
    , testProperty
    , withMaxSuccess
    )

-- --------------------------------------------------------
-- Scenario types
-- --------------------------------------------------------

-- | A complete test scenario: member count + actions.
data Scenario = Scenario
    { scnMemberCount :: Int
    -- ^ Number of members (index 0 = admin)
    , scnActions :: [Action]
    -- ^ Sequence of lifecycle actions
    }
    deriving stock (Show)

-- | A single lifecycle action.
data Action
    = -- | Full proposal round (submit + respond + resolve)
      ProposeRound ProposalRound
    | -- | Remove member at index (admin signs)
      RemoveAction Int
    deriving stock (Show)

-- | A complete proposal lifecycle.
data ProposalRound = ProposalRound
    { prProposer :: Int
    -- ^ Index of proposer
    , prResponders :: [Int]
    -- ^ Indices of responders
    , prResolution :: Resolution
    -- ^ How the sequencer resolves
    }
    deriving stock (Show)

-- --------------------------------------------------------
-- Generator
-- --------------------------------------------------------

-- | Generate a valid scenario with 2-4 members.
genScenario :: Gen Scenario
genScenario = do
    n <- choose (2, 4)
    actions <- genActions (IntSet.fromList [0 .. n - 1])
    pure
        Scenario
            { scnMemberCount = n
            , scnActions = actions
            }

-- | Generate a list of valid actions, threading active set.
genActions :: IntSet -> Gen [Action]
genActions active = do
    continue <-
        frequency [(3, pure True), (1, pure False)]
    if not continue || IntSet.size active < 2
        then pure []
        else do
            action <- genValidAction active
            let active' = case action of
                    RemoveAction i ->
                        IntSet.delete i active
                    _ -> active
            (action :) <$> genActions active'

{- | Generate one valid action. Proposals are 3x more
likely than removals. Removals require > 2 active
members and never remove the admin (index 0).
-}
genValidAction :: IntSet -> Gen Action
genValidAction active
    | canRemove =
        frequency
            [ (3, ProposeRound <$> genRound active)
            , (1, RemoveAction <$> elements removable)
            ]
    | otherwise =
        ProposeRound <$> genRound active
  where
    removable =
        IntSet.toList (IntSet.delete 0 active)
    canRemove =
        not (null removable)
            && IntSet.size active > 2

-- | Generate a proposal round from the active set.
genRound :: IntSet -> Gen ProposalRound
genRound active = do
    let members = IntSet.toList active
    proposer <- elements members
    let others = filter (/= proposer) members
    responders <- sublistOf others
    resolution <-
        elements
            [ ThresholdReached
            , ProposerPositive
            , ProposerNegative
            , Timeout
            ]
    pure
        ProposalRound
            { prProposer = proposer
            , prResponders = responders
            , prResolution = resolution
            }

-- --------------------------------------------------------
-- Shrinking
-- --------------------------------------------------------

{- | Shrink a scenario by removing trailing actions
or shrinking responder lists within rounds. Does not
shrink member count or remove intermediate removals
(would invalidate indices).
-}
shrinkScenario :: Scenario -> [Scenario]
shrinkScenario s =
    -- Drop trailing actions (always valid)
    [ s{scnActions = take k (scnActions s)}
    | k <- [0 .. length (scnActions s) - 1]
    ]
        -- Shrink responder lists within rounds
        ++ [ s{scnActions = shrinkAt i (scnActions s)}
           | i <- [0 .. length (scnActions s) - 1]
           , canShrinkAction (scnActions s !! i)
           ]
  where
    canShrinkAction (ProposeRound pr) =
        not (null (prResponders pr))
    canShrinkAction _ = False

    shrinkAt _ [] = []
    shrinkAt 0 (ProposeRound pr : rest) =
        ProposeRound
            pr
                { prResponders =
                    drop 1 (prResponders pr)
                }
            : rest
    shrinkAt n (a : rest) = a : shrinkAt (n - 1) rest

-- --------------------------------------------------------
-- Execution
-- --------------------------------------------------------

{- | Execute a scenario against a fresh test server and
verify state machine invariants.
-}
executeScenario :: TestEnv -> Scenario -> IO ()
executeScenario te scenario = do
    let n = scnMemberCount scenario

    -- Create test identities (index 0 = admin)
    admin <- newTestId
    others <- mapM (const newTestId) [1 .. n - 1]
    let tids = admin : others
    seqId <- mkBadTestId "server-sequencer"

    -- Track sign counts per member index
    signCounts <-
        newIORef
            ( IntMap.fromList
                [(i, 0 :: Int) | i <- [0 .. n - 1]]
            )
    let bumpSign i =
            modifyIORef'
                signCounts
                (IntMap.adjust (+ 1) i)

    -- Track total events posted (for nextSeq check)
    totalEventsRef <- newIORef (0 :: Int)
    let bumpTotal =
            modifyIORef' totalEventsRef (+ 1)

    -- Bootstrap admin (index 0)
    _ <- postEvent te (bootstrapAdmin admin)
    bumpSign 0
    bumpTotal

    -- Introduce members 1..n-1
    mapM_
        ( \i -> do
            _ <-
                postEvent
                    te
                    ( introduceMember
                        admin
                        (tids !! i)
                    )
            bumpSign 0
            bumpTotal
        )
        [1 .. n - 1]

    -- Track removed members
    removedRef <- newIORef IntSet.empty

    -- Execute actions
    mapM_
        ( \case
            ProposeRound pr -> do
                let proposer = tids !! prProposer pr
                pid <-
                    postEvent
                        te
                        ( submitProposal
                            proposer
                            9999
                        )
                bumpSign (prProposer pr)
                bumpTotal

                mapM_
                    ( \ri -> do
                        _ <-
                            postEvent
                                te
                                ( respondToProposal
                                    (tids !! ri)
                                    pid
                                )
                        bumpSign ri
                        bumpTotal
                    )
                    (prResponders pr)

                -- Sequencer resolves (exempt from signing)
                _ <-
                    postEvent
                        te
                        ( resolveProposal
                            seqId
                            pid
                            (prResolution pr)
                        )
                bumpTotal
                pure ()
            RemoveAction i -> do
                _ <-
                    postEvent
                        te
                        (removeMember admin (tids !! i))
                bumpSign 0
                bumpTotal
                modifyIORef'
                    removedRef
                    (IntSet.insert i)
        )
        (scnActions scenario)

    -- ---- Verify invariants ----

    finalCounts <- readIORef signCounts
    removedSet <- readIORef removedRef
    totalEvents <- readIORef totalEventsRef
    info <- getInfo te

    -- (1) Processing.lean: nextSeq increments by 1 per event
    --     Server starts at nextSeq=1 (genesis), so after N
    --     events: nextSeq = 1 + N
    assertEqual
        "nextSeq = 1 + totalEvents"
        (1 + totalEvents)
        (irNextSeq info)

    -- (2) BaseDecisions.lean: member count =
    --     1 (sequencer) + active members
    let activeCount = n - IntSet.size removedSet
    assertEqual
        "memberCount = sequencer + active"
        (1 + activeCount)
        (irMemberCount info)

    -- (3) BaseDecisions.lean: admin exists → normal mode
    --     (generator never removes index 0 = admin)
    assertEqual
        "authMode = normal (admin exists)"
        "normal"
        (irAuthMode info)

    -- (4) MemberKel.lean: per-member KEL event count
    mapM_
        ( \i ->
            if IntSet.member i removedSet
                then do
                    -- Removed: KEL should be 404
                    resp <-
                        httpGet
                            te
                            ( "/members/"
                                <> urlEncode
                                    ( tidKey
                                        (tids !! i)
                                    )
                                <> "/kel"
                            )
                    assertEqual
                        ( "removed member "
                            <> show i
                            <> " KEL status"
                        )
                        status404
                        (HC.responseStatus resp)
                else do
                    -- Active: KEL = 1 (icp) + signCount
                    kel <-
                        getMemberKel
                            te
                            (tidKey (tids !! i))
                    let expected =
                            1
                                + IntMap.findWithDefault
                                    0
                                    i
                                    finalCounts
                    assertEqual
                        ( "member "
                            <> show i
                            <> " KEL count"
                        )
                        expected
                        (kelRespEventCount kel)
        )
        [0 .. n - 1]

-- --------------------------------------------------------
-- Property
-- --------------------------------------------------------

-- | State machine invariants property.
stateMachineProperty :: Property
stateMachineProperty =
    withMaxSuccess 30 $
        forAllShrink genScenario shrinkScenario $
            \scenario ->
                ioProperty $
                    withTestEnv $ \te ->
                        executeScenario te scenario

-- | All property-based tests.
tests :: TestTree
tests =
    testGroup
        "State machine properties"
        [ testProperty
            "invariants hold after random lifecycle"
            stateMachineProperty
        ]
