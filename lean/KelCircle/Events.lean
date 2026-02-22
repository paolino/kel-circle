-- Events: the three event classes and membership model
--
-- The protocol fixes three event classes (decision, proposal, response).
-- The content within each class is parameterized by the application.

import KelCircle.GlobalSequence

namespace KelCircle

-- Role in the circle
inductive Role where
  | admin
  | member
  deriving Repr, BEq, DecidableEq

-- A circle member with their role
structure Member where
  id   : MemberId
  role : Role
  deriving Repr

-- Proposal identifier (index in the global sequence where it was opened)
abbrev ProposalId := Nat

-- The three event classes, parameterized by application content types
inductive EventClass (δ π ρ : Type) where
  | decision (content : δ)
  | proposal (content : π) (timeout : Timestamp) (proposer : MemberId)
  | response (content : ρ) (proposalId : ProposalId) (responder : MemberId)

variable {δ π ρ : Type}

-- Predicate: an event is a decision
def EventClass.isDecision : EventClass δ π ρ → Prop
  | .decision _ => True
  | .proposal _ _ _ => False
  | .response _ _ _ => False

-- Predicate: an event is a proposal
def EventClass.isProposal : EventClass δ π ρ → Prop
  | .decision _ => False
  | .proposal _ _ _ => True
  | .response _ _ _ => False

-- Predicate: an event is a response
def EventClass.isResponse : EventClass δ π ρ → Prop
  | .decision _ => False
  | .proposal _ _ _ => False
  | .response _ _ _ => True

-- Every event is exactly one class
theorem event_trichotomy (e : EventClass δ π ρ) :
    e.isDecision ∨ e.isProposal ∨ e.isResponse := by
  cases e <;> simp [EventClass.isDecision, EventClass.isProposal, EventClass.isResponse]

-- Decision and proposal are mutually exclusive
theorem decision_not_proposal (e : EventClass δ π ρ) :
    e.isDecision → ¬e.isProposal := by
  cases e <;> simp [EventClass.isDecision, EventClass.isProposal]

-- Decision and response are mutually exclusive
theorem decision_not_response (e : EventClass δ π ρ) :
    e.isDecision → ¬e.isResponse := by
  cases e <;> simp [EventClass.isDecision, EventClass.isResponse]

-- Proposal and response are mutually exclusive
theorem proposal_not_response (e : EventClass δ π ρ) :
    e.isProposal → ¬e.isResponse := by
  cases e <;> simp [EventClass.isProposal, EventClass.isResponse]

end KelCircle
