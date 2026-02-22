{- |
Module      : KelCircle.Sequence
Description : Global sequence and sequenced events
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

The global sequence: a monotonic counter managed by the
sequencer. Each event gets a unique index and a strictly
increasing UTC timestamp. Mirrors Lean
@KelCircle.GlobalSequence@.
-}
module KelCircle.Sequence
    ( -- * Sequenced events
      SequencedEvent (..)

      -- * Invariant checks
    , indicesContiguous
    , timestampsIncreasing
    , noDuplicateIndices
    , isWellFormed
    ) where

import Data.List (nub)
import KelCircle.Types (MemberId, Timestamp)

{- | A sequenced event: an interaction event assigned a
position in the global sequence by the sequencer.
Mirrors Lean @SequencedEvent@.
-}
data SequencedEvent a = SequencedEvent
    { seqIndex :: Int
    -- ^ Position in the global sequence
    , seqTimestamp :: Timestamp
    -- ^ UTC timestamp assigned by sequencer
    , seqMember :: MemberId
    -- ^ Member who submitted the event
    , seqPayload :: a
    -- ^ Event content
    }
    deriving stock (Show)

{- | Check that indices form a contiguous range @[0, n)@.
Events are stored newest-first (head = most recent).
Mirrors Lean @indicesContiguous@.
-}
indicesContiguous :: [SequencedEvent a] -> Bool
indicesContiguous = go
  where
    go [] = True
    go (e : rest) =
        seqIndex e == length rest && go rest

{- | Check that timestamps strictly increase (newest
first in list).
Mirrors Lean @timestampsIncreasing@.
-}
timestampsIncreasing :: [SequencedEvent a] -> Bool
timestampsIncreasing = go
  where
    go [] = True
    go [_] = True
    go (e1 : e2 : rest) =
        seqTimestamp e1 > seqTimestamp e2
            && go (e2 : rest)

{- | Check that no two events share the same index.
Mirrors Lean @noDuplicateIndices@.
-}
noDuplicateIndices :: [SequencedEvent a] -> Bool
noDuplicateIndices es =
    let indices = map seqIndex es
    in  indices == nub indices

{- | A well-formed sequence satisfies all three invariants.
Mirrors Lean @WellFormedSequence@.
-}
isWellFormed :: [SequencedEvent a] -> Bool
isWellFormed gs =
    indicesContiguous gs
        && timestampsIncreasing gs
        && noDuplicateIndices gs
