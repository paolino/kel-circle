{- |
Module      : KelCircle.Events
Description : Event classes and base decisions
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

The three event classes (decision, proposal, response)
and base decisions (membership operations). Mirrors Lean
@KelCircle.Events@ and @KelCircle.BaseDecisions@.
-}
module KelCircle.Events
    ( -- * Event classes
      EventClass (..)
    , isDecision
    , isProposal
    , isResponse

      -- * Base decisions
    , BaseDecision (..)

      -- * Proposal resolution
    , Resolution (..)
    , isPositive

      -- * Circle events (unified)
    , CircleEvent (..)
    ) where

import KelCircle.Types
    ( MemberId
    , ProposalId
    , Role
    , Timestamp
    )

{- | The three event classes, parameterized by
application content types.
Mirrors Lean @EventClass@.

Type parameters:

* @d@ — decision content
* @p@ — proposal content
* @r@ — response content
-}
data EventClass d p r
    = -- | A straight decision (changes the fold)
      Decision d
    | -- | Opens a coordination round
      Proposal p Timestamp MemberId
    | -- | References an open proposal
      Response r ProposalId MemberId
    deriving stock (Show, Eq)

-- | Is this event a decision?
isDecision :: EventClass d p r -> Bool
isDecision (Decision _) = True
isDecision _ = False

-- | Is this event a proposal?
isProposal :: EventClass d p r -> Bool
isProposal (Proposal{}) = True
isProposal _ = False

-- | Is this event a response?
isResponse :: EventClass d p r -> Bool
isResponse (Response{}) = True
isResponse _ = False

{- | Base decisions: protocol-level membership operations.
Mirrors Lean @BaseDecision@.
-}
data BaseDecision
    = -- | Add a member with a given role
      IntroduceMember MemberId Role
    | -- | Remove a member from the circle
      RemoveMember MemberId
    | -- | Change a member's role
      ChangeRole MemberId Role
    | -- | Replace the sequencer identity
      RotateSequencer MemberId
    deriving stock (Show, Eq)

{- | How a proposal was resolved.
Mirrors Lean @Resolution@.
-}
data Resolution
    = -- | Enough responses met the threshold gate
      ThresholdReached
    | -- | Proposer closed with positive outcome
      ProposerPositive
    | -- | Proposer cancelled
      ProposerNegative
    | -- | Deadline passed without resolution
      Timeout
    deriving stock (Show, Eq)

{- | A resolution is positive if it carries responses
into the fold.
Mirrors Lean @Resolution.isPositive@.
-}
isPositive :: Resolution -> Bool
isPositive ThresholdReached = True
isPositive ProposerPositive = True
isPositive ProposerNegative = False
isPositive Timeout = False

{- | The unified circle event type. Combines base
decisions, application decisions, proposals,
responses, and resolution events.
Mirrors Lean @CircleEvent@.

Type parameters:

* @d@ — application decision content
* @p@ — proposal content
* @r@ — response content
-}
data CircleEvent d p r
    = -- | A base decision (membership operation)
      CEBaseDecision BaseDecision
    | -- | An application decision (domain-specific)
      CEAppDecision d
    | -- | Opens a proposal with deadline
      CEProposal p Timestamp
    | -- | Responds to an open proposal
      CEResponse r ProposalId
    | -- | Server-emitted resolution
      CEResolveProposal ProposalId Resolution
    deriving stock (Show, Eq)
