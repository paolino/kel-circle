-- | Event types mirroring Haskell KelCircle.Events.
module KelCircle.Client.Events
  ( BaseDecision(..)
  , Resolution(..)
  , isPositive
  , CircleEvent(..)
  ) where

import Prelude

import KelCircle.Client.Types (MemberId, ProposalId, Role, Timestamp)

-- | Base decisions: protocol-level membership operations.
-- | Mirrors Haskell @BaseDecision@.
data BaseDecision
  = IntroduceMember MemberId String Role
  | RemoveMember MemberId
  | ChangeRole MemberId Role
  | RotateSequencer MemberId

derive instance eqBaseDecision :: Eq BaseDecision

instance showBaseDecision :: Show BaseDecision where
  show (IntroduceMember mid _ _) = "IntroduceMember " <> mid
  show (RemoveMember mid) = "RemoveMember " <> mid
  show (ChangeRole mid _) = "ChangeRole " <> mid
  show (RotateSequencer mid) = "RotateSequencer " <> mid

-- | How a proposal was resolved.
-- | Mirrors Haskell @Resolution@.
data Resolution
  = ThresholdReached
  | ProposerPositive
  | ProposerNegative
  | Timeout

derive instance eqResolution :: Eq Resolution

instance showResolution :: Show Resolution where
  show ThresholdReached = "ThresholdReached"
  show ProposerPositive = "ProposerPositive"
  show ProposerNegative = "ProposerNegative"
  show Timeout = "Timeout"

-- | A resolution is positive if it carries responses
-- | into the fold.
isPositive :: Resolution -> Boolean
isPositive ThresholdReached = true
isPositive ProposerPositive = true
isPositive ProposerNegative = false
isPositive Timeout = false

-- | The unified circle event type.
-- | Mirrors Haskell @CircleEvent d p r@.
-- | Type parameters:
-- |   d — application decision content
-- |   p — proposal content
-- |   r — response content
data CircleEvent d p r
  = CEBaseDecision BaseDecision
  | CEAppDecision d
  | CEProposal p Timestamp
  | CEResponse r ProposalId
  | CEResolveProposal ProposalId Resolution

derive instance eqCircleEvent :: (Eq d, Eq p, Eq r) => Eq (CircleEvent d p r)
