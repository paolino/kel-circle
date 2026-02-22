{- |
Module      : KelCircle.Gate
Description : Two-level semantic gate
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

The two-level gate: base gate (protocol-level access
control) composed with application gate. Mirrors Lean
@KelCircle.BaseDecisions@ gate functions and
@KelCircle.Processing@ gate functions.
-}
module KelCircle.Gate
    ( -- * Sequencer protection
      protectsSequencer
    , requiresMajority

      -- * Name uniqueness
    , hasUniqueName

      -- * Base gate
    , baseGate

      -- * Full gate (base + app)
    , fullGate

      -- * Admin majority
    , hasAdminMajority
    ) where

import KelCircle.Events (BaseDecision (..))
import KelCircle.State
    ( CircleState
    , adminCount
    , isAdmin
    , isBootstrap
    , isMember
    , majority
    , nameExists
    )
import KelCircle.Types (MemberId, Role (..))

{- | The sequencer cannot be removed or promoted to
admin.
Mirrors Lean @protectsSequencer@.
-}
protectsSequencer :: MemberId -> BaseDecision -> Bool
protectsSequencer sid = \case
    RemoveMember mid -> mid /= sid
    ChangeRole mid Admin -> mid /= sid
    _ -> True

{- | Admin role changes require majority (proposal),
not a straight decision.
Mirrors Lean @requiresMajority@.
-}
requiresMajority :: BaseDecision -> Bool
requiresMajority = \case
    IntroduceMember _ _ Admin -> True
    ChangeRole _ Admin -> True
    ChangeRole _ Member -> True
    _ -> False

{- | Reject introduction of a member whose name is
already taken.
-}
hasUniqueName :: CircleState -> BaseDecision -> Bool
hasUniqueName s = \case
    IntroduceMember _ name _ -> not (nameExists s name)
    _ -> True

{- | Base gate for straight decisions.
In bootstrap: only admin introduction.
In normal: signer must be admin, and the decision
must not require majority.
Mirrors Lean @baseGate@.
-}
baseGate
    :: CircleState
    -> MemberId
    -> MemberId
    -> BaseDecision
    -> Bool
baseGate s signer sid d =
    protectsSequencer sid d
<<<<<<< HEAD
        && hasUniqueName s d
        && targetExists s d
        && if isBootstrap s
            then case d of
                IntroduceMember _ _ Admin -> True
                _ -> False
            else isAdmin s signer && not (requiresMajority d)

{- | Check that the target of a decision exists in
the circle.
-}
targetExists :: CircleState -> BaseDecision -> Bool
targetExists s = \case
    RemoveMember mid -> isMember s mid
    ChangeRole mid _ -> isMember s mid
    _ -> True

{- | The full gate composes base gate and application
gate.
Mirrors Lean @fullGate@.
-}
fullGate
    :: CircleState
    -> MemberId
    -> MemberId
    -> BaseDecision
    -> g
    -> (g -> BaseDecision -> Bool)
    -> Bool
fullGate s signer sid d appState appGate =
    baseGate s signer sid d
        && appGate appState d

{- | Check whether admin respondents meet the majority
threshold.
Mirrors Lean @adminMajorityMet@.
-}
hasAdminMajority
    :: CircleState -> [MemberId] -> Bool
hasAdminMajority s respondents =
    let adminRespondents =
            filter (isAdmin s) respondents
    in  length adminRespondents
            >= majority (adminCount s)
