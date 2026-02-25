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
* sequencerKelCount = 1 (icp) + resolveCount
* full cryptographic KEL audit: seqNum contiguity,
  prefix consistency, digest chaining, signature validity
* prefix uniqueness across all active members
* global sequence contiguity + signer membership
-}
module E2E.KelProperties (tests) where

import Data.ByteString (ByteString)
import Data.Either (fromRight)
import Data.IORef
    ( modifyIORef'
    , newIORef
    , readIORef
    )
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import E2E.TestHelpers
import KelCircle.Events (Resolution (..))
import KelCircle.MemberKel
    ( extractDigest
    , extractKeys
    , extractPrefix
    , extractSeqNumHex
    , parseEventJson
    )
import Keri.Cesr.Decode qualified as Cesr
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519 (publicKeyFromBytes, verify)
import Network.HTTP.Client qualified as HC
import Network.HTTP.Types (status200, status404)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
    ( assertBool
    , assertEqual
    , assertFailure
    )
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

{- | Generate a proposal round from the active set.
Only admin members (index 0) can respond to proposals.
-}
genRound :: IntSet -> Gen ProposalRound
genRound active = do
    let ms = IntSet.toList active
    proposer <- elements ms
    let admins =
            filter
                (\i -> i == 0 && i /= proposer)
                ms
    responders <- sublistOf admins
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
        seqId = teSequencer te

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

    -- Track resolve count (for sequencer KEL)
    resolveCountRef <- newIORef (0 :: Int)
    let bumpResolve =
            modifyIORef' resolveCountRef (+ 1)

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

                -- Sequencer resolves
                _ <-
                    postEvent
                        te
                        ( resolveProposal
                            seqId
                            pid
                            (prResolution pr)
                        )
                bumpTotal
                bumpResolve
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
    resolveCount <- readIORef resolveCountRef
    info <- getInfo te

    -- (1) nextSeq = 1 + totalEvents
    assertEqual
        "nextSeq = 1 + totalEvents"
        (1 + totalEvents)
        (irNextSeq info)

    -- (2) memberCount = 1 (sequencer) + active members
    let activeCount = n - IntSet.size removedSet
    assertEqual
        "memberCount = sequencer + active"
        (1 + activeCount)
        (irMemberCount info)

    -- (3) authMode = "normal" (admin exists)
    assertEqual
        "authMode = normal (admin exists)"
        "normal"
        (irAuthMode info)

    -- (4) per-member KEL event count
    mapM_
        ( \i ->
            if IntSet.member i removedSet
                then do
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

    -- (5) sequencer KEL = 1 (icp) + resolveCount
    seqKel <-
        getMemberKel te (tidKey seqId)
    assertEqual
        "sequencer KEL count"
        (1 + resolveCount)
        (kelRespEventCount seqKel)

    -- (6) full cryptographic KEL audit for all active
    --     members including the sequencer
    let activeKeys =
            [ tidKey (tids !! i)
            | i <- [0 .. n - 1]
            , not (IntSet.member i removedSet)
            ]
                ++ [tidKey seqId]
    mapM_ (auditKel te) activeKeys

    -- (7) Prefix uniqueness across all active members
    prefixes <- mapM (getKelPrefix te) activeKeys
    let prefixSet = Set.fromList prefixes
    assertEqual
        "all KERI prefixes unique"
        (length activeKeys)
        (Set.size prefixSet)

    -- (8) Global sequence contiguity + signer membership
    --     Walk the entire log, verify each signer is a
    --     known member (including removed ones).
    let allKnownKeys =
            Set.fromList
                ( [ tidKey (tids !! i)
                  | i <- [0 .. n - 1]
                  ]
                    ++ [tidKey seqId]
                )
    globalEvents <-
        walkGlobalLog te totalEvents
    mapM_
        ( \(idx, ge) ->
            assertBool
                ( "global event "
                    <> show idx
                    <> " signer is known member"
                )
                ( Set.member
                    (erSigner ge)
                    allKnownKeys
                )
        )
        (zip [0 :: Int ..] globalEvents)

-- --------------------------------------------------------
-- Prefix uniqueness helper
-- --------------------------------------------------------

{- | Extract the KERI prefix from a member's inception
event (first event in their KEL).
-}
getKelPrefix :: TestEnv -> T.Text -> IO T.Text
getKelPrefix te midText = do
    kr <- getMemberKel te midText
    case kelRespEvents kr of
        [] -> do
            _ <-
                assertFailure $
                    T.unpack midText
                        <> " KEL is empty"
            pure ""
        (ker : _) ->
            case parseEventJson
                (TE.encodeUtf8 (kerEvent ker)) of
                Left err -> do
                    _ <-
                        assertFailure $
                            T.unpack midText
                                <> " inception parse: "
                                <> T.unpack err
                    pure ""
                Right evtVal ->
                    case extractPrefix evtVal of
                        Left err -> do
                            _ <-
                                assertFailure $
                                    T.unpack midText
                                        <> " prefix: "
                                        <> T.unpack err
                            pure ""
                        Right pfx -> pure pfx

-- --------------------------------------------------------
-- Global sequence walk
-- --------------------------------------------------------

{- | Walk the global event log via GET /events?after=N.
Verifies contiguity (every position returns 200) and
that no extra events exist beyond the expected total.
-}
walkGlobalLog
    :: TestEnv -> Int -> IO [GetEventResp]
walkGlobalLog te total = do
    events <- mapM fetchAt [0 .. total - 1]
    -- Verify no extra events exist
    resp <-
        httpGet
            te
            ( "/events?after="
                <> show (total - 1)
            )
    assertEqual
        "global log ends at expected total"
        status404
        (HC.responseStatus resp)
    pure events
  where
    fetchAt idx = do
        resp <-
            httpGet
                te
                ( "/events?after="
                    <> show (idx - 1)
                )
        assertEqual
            ( "global event "
                <> show idx
                <> " exists"
            )
            status200
            (HC.responseStatus resp)
        decodeOrFail (HC.responseBody resp)

-- --------------------------------------------------------
-- Full cryptographic KEL audit
-- --------------------------------------------------------

{- | Audit a member's KEL for:
(a) seqNum contiguity
(b) prefix consistency
(c) digest chaining
(d) signature validity
-}
auditKel :: TestEnv -> T.Text -> IO ()
auditKel te midText = do
    kr <- getMemberKel te midText
    let evts = kelRespEvents kr
        label = T.unpack midText

    -- Walk events checking invariants
    auditEvents label Nothing 0 evts

{- | Walk KEL events verifying invariants pairwise.
Tracks the expected seqNum, prefix from first event,
and prior digest for chaining.
-}
auditEvents
    :: String
    -> Maybe (T.Text, T.Text)
    -- ^ (prefix, lastDigest) from prior event
    -> Int
    -- ^ Expected seqNum
    -> [KelEventResp]
    -> IO ()
auditEvents _ _ _ [] = pure ()
auditEvents label mPrior expectedSeq (ker : rest) = do
    let evtBytes = TE.encodeUtf8 (kerEvent ker)
    case parseEventJson evtBytes of
        Left err ->
            assertFailure $
                label
                    <> " event parse failed: "
                    <> T.unpack err
        Right evtVal -> do
            -- (a) seqNum contiguity
            case extractSeqNumHex evtVal of
                Left err ->
                    assertFailure $
                        label
                            <> " seqNum extract: "
                            <> T.unpack err
                Right sn ->
                    assertEqual
                        ( label
                            <> " seqNum at "
                            <> show expectedSeq
                        )
                        expectedSeq
                        sn

            -- (b) prefix consistency
            case extractPrefix evtVal of
                Left err ->
                    assertFailure $
                        label
                            <> " prefix extract: "
                            <> T.unpack err
                Right pfx ->
                    case mPrior of
                        Nothing -> pure ()
                        Just (prevPfx, _) ->
                            assertEqual
                                ( label
                                    <> " prefix at "
                                    <> show expectedSeq
                                )
                                prevPfx
                                pfx

            -- (c) digest chaining
            case extractDigest evtVal of
                Left err ->
                    assertFailure $
                        label
                            <> " digest extract: "
                            <> T.unpack err
                Right _digest ->
                    pure ()

            -- (d) signature validity
            verifySigs label evtBytes (kerSignatures ker)

            -- Continue to next
            let pfx =
                    fromRight "?" (extractPrefix evtVal)
                dig =
                    fromRight "?" (extractDigest evtVal)
            auditEvents
                label
                (Just (pfx, dig))
                (expectedSeq + 1)
                rest

{- | Verify that at least one signature in the event
is cryptographically valid. Decodes the CESR signature
and the signer's public key from the event's \"k\" field.
-}
verifySigs
    :: String -> ByteString -> [(Int, T.Text)] -> IO ()
verifySigs label evtBytes sigs =
    case parseEventJson evtBytes of
        Left _ -> pure ()
        Right evtVal ->
            case extractKeys evtVal of
                Left _ -> pure ()
                Right keys ->
                    mapM_ (verifySig label evtBytes keys) sigs

{- | Verify a single indexed signature against the
corresponding key.
-}
verifySig
    :: String
    -> ByteString
    -> [T.Text]
    -> (Int, T.Text)
    -> IO ()
verifySig label evtBytes keys (idx, sigCesr)
    | idx >= length keys = pure ()
    | otherwise = do
        let keyCesr = keys !! idx
        case Cesr.decode sigCesr of
            Left _ -> pure ()
            Right sigPrim ->
                case Cesr.decode keyCesr of
                    Left _ -> pure ()
                    Right keyPrim ->
                        case ( code keyPrim
                             , code sigPrim
                             ) of
                            ( Ed25519PubKey
                                , Ed25519Sig
                                ) ->
                                    case publicKeyFromBytes
                                        (raw keyPrim) of
                                        Left _ ->
                                            pure ()
                                        Right pk ->
                                            assertEqual
                                                ( label
                                                    <> " sig["
                                                    <> show idx
                                                    <> "] valid"
                                                )
                                                True
                                                ( verify
                                                    pk
                                                    evtBytes
                                                    (raw sigPrim)
                                                )
                            _ -> pure ()

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
