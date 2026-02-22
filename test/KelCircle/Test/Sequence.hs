{- |
Module      : KelCircle.Test.Sequence
Description : Properties for global sequence invariants
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

QuickCheck properties mirroring the Lean theorems in
@KelCircle.GlobalSequence@ and @KelCircle.Invariants@.
-}
module KelCircle.Test.Sequence
    ( tests
    ) where

import KelCircle.Sequence
    ( SequencedEvent (..)
    , indicesContiguous
    , isWellFormed
    , noDuplicateIndices
    , timestampsIncreasing
    )
import KelCircle.Test.Generators ()
import KelCircle.Types (MemberId (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import Test.Tasty.QuickCheck
    ( testProperty
    )

dummyMid :: MemberId
dummyMid = MemberId "test"

tests :: TestTree
tests =
    testGroup
        "GlobalSequence"
        [ testEmptyInvariants
        , testSingletonInvariants
        , testAppendPreservesContiguous
        , testAppendPreservesIncreasing
        , testAppendPreservesNoDups
        ]

{- | Mirrors: empty_contiguous, empty_increasing,
empty_no_dups
-}
testEmptyInvariants :: TestTree
testEmptyInvariants =
    testCase "empty sequence is well-formed" $
        assertEqual
            ""
            True
            (isWellFormed ([] :: [SequencedEvent ()]))

{- | Mirrors: singleton_contiguous,
singleton_increasing, singleton_no_dups
-}
testSingletonInvariants :: TestTree
testSingletonInvariants =
    testCase "singleton with index 0 is well-formed" $
        let e =
                SequencedEvent
                    { seqIndex = 0
                    , seqTimestamp = 100
                    , seqMember = dummyMid
                    , seqPayload = ()
                    }
        in  assertEqual "" True (isWellFormed [e])

-- | Mirrors: append_preserves_contiguous
testAppendPreservesContiguous :: TestTree
testAppendPreservesContiguous =
    testProperty
        "append with correct index preserves contiguity"
        $ \(ts :: Int) ->
            let e0 =
                    SequencedEvent
                        0
                        100
                        dummyMid
                        ()
                e1 = SequencedEvent 1 (100 + abs ts + 1) dummyMid ()
                gs = [e1, e0]
            in  indicesContiguous gs

-- | Mirrors: append_preserves_increasing
testAppendPreservesIncreasing :: TestTree
testAppendPreservesIncreasing =
    testProperty
        "append with greater timestamp preserves increasing"
        $ \(delta :: Int) ->
            let d = abs delta + 1
                e0 =
                    SequencedEvent
                        0
                        100
                        dummyMid
                        ()
                e1 =
                    SequencedEvent
                        1
                        (100 + d)
                        dummyMid
                        ()
                gs = [e1, e0]
            in  timestampsIncreasing gs

-- | Mirrors: append_preserves_no_dups
testAppendPreservesNoDups :: TestTree
testAppendPreservesNoDups =
    testProperty
        "append with unique index preserves no-dups"
        $ \(idx :: Int) ->
            let i = abs idx + 1
                e0 =
                    SequencedEvent
                        0
                        100
                        dummyMid
                        ()
                e1 =
                    SequencedEvent
                        i
                        200
                        dummyMid
                        ()
                gs = [e1, e0]
            in  noDuplicateIndices gs
