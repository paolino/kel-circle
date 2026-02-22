{- |
Module      : KelCircle.Fold
Description : State fold over the global sequence
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Generic fold over sequenced events. Every event in the
global sequence contributes to the resulting state.
Mirrors Lean @KelCircle.Fold@.
-}
module KelCircle.Fold
    ( -- * Generic fold
      foldAll

      -- * Two-layer fold
    , TwoLayerState (..)
    , twoLayerFold
    ) where

import KelCircle.Sequence (SequencedEvent (..))

{- | Fold all events in the global sequence, oldest-first.
Every event contributes to the resulting state.
Mirrors Lean @foldAll@.
-}
foldAll
    :: (s -> a -> s)
    -> s
    -> [SequencedEvent a]
    -> s
foldAll f = foldl (\acc e -> f acc (seqPayload e))

{- | Two-layer state: base fold + application fold.
Mirrors Lean @TwoLayerState@.
-}
data TwoLayerState b g = TwoLayerState
    { tlBase :: b
    -- ^ Base-layer state (membership, roles)
    , tlApp :: g
    -- ^ Application-layer state
    }
    deriving stock (Show, Eq)

{- | Compose two fold functions into a two-layer fold.
Both layers see every sequenced event.
Mirrors Lean @twoLayerFold@.
-}
twoLayerFold
    :: (b -> a -> b)
    -> (g -> a -> g)
    -> TwoLayerState b g
    -> [SequencedEvent a]
    -> TwoLayerState b g
twoLayerFold fBase fApp =
    foldAll
        ( \s a ->
            TwoLayerState
                (fBase (tlBase s) a)
                (fApp (tlApp s) a)
        )
