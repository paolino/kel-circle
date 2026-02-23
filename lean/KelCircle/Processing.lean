-- Processing: the event processing pipeline
--
-- This module ties together the event classes, two-level gate,
-- two-layer fold, and proposal lifecycle into a single state
-- machine. Each submitted event is validated, applied to the
-- fold, and (if a proposal or response) updates the proposal
-- registry.

import KelCircle.Proposals
import KelCircle.MemberKel

namespace KelCircle

-------------------------------------------------------------------
-- Circle event: the unified event type
-------------------------------------------------------------------

-- A circle event is one of:
-- 1. A base decision (membership operation)
-- 2. An application decision (domain-specific, straight)
-- 3. A proposal (opens coordination round)
-- 4. A response (to an open proposal)
-- 5. A proposal resolution (server-emitted decision)
--
-- Base decisions are separate from app decisions because the
-- base gate handles them differently (admin checks, sequencer
-- protection). Application decisions go through the app gate only.
inductive CircleEvent (δ π ρ : Type) where
  | baseDecision    (d : BaseDecision)
  | appDecision     (content : δ)
  | proposal        (content : π) (deadline : Timestamp)
  | response        (content : ρ) (proposalId : ProposalId)
  | resolveProposal (proposalId : ProposalId) (r : Resolution)
  deriving Repr

-------------------------------------------------------------------
-- Full circle state: base + app + proposals
-------------------------------------------------------------------

-- The complete state maintained by the sequencer
structure FullState (γ π ρ : Type) where
  circle     : Circle              -- base state + sequencer id
  appState   : γ                   -- application fold state
  proposals  : ProposalRegistry π ρ  -- tracked proposals
  nextSeq    : Nat                 -- next sequence number
  memberKels : MemberKels          -- per-member KELs

-- Initial state for a circle
def initFullState {γ π ρ : Type}
    (sid : MemberId) (initApp : γ) : FullState γ π ρ :=
  { circle     := genesis sid
  , appState   := initApp
  , proposals  := []
  , nextSeq    := 1  -- event 0 was genesis
  , memberKels := []
  }

-------------------------------------------------------------------
-- Gate: who can submit what
-------------------------------------------------------------------

-- Gate for base decisions: uses the two-level gate
def gateBaseDecision {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (d : BaseDecision) (appGate : γ → BaseDecision → Bool) : Bool :=
  fullGate s.circle.state signer s.circle.sequencerId d s.appState appGate

-- Gate for application decisions: signer must be a member
def gateAppDecision {γ δ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (content : δ) (appGate : γ → δ → Bool) : Bool :=
  isMemberB s.circle.state signer && appGate s.appState content

-- Gate for proposals: signer must be a member
def gateProposal {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (content : π) (appGate : γ → π → Bool) : Bool :=
  isMemberB s.circle.state signer && appGate s.appState content

-- Gate for responses: signer must be a member, proposal must be
-- open, signer must not have already responded
def gateResponse {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (proposalId : ProposalId) : Bool :=
  isMemberB s.circle.state signer &&
  match findProposal s.proposals proposalId with
  | some p => canRespond p signer
  | none => false

-- Gate for resolve: only the sequencer can resolve proposals
def gateResolve {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId) : Bool :=
  signer == s.circle.sequencerId

-------------------------------------------------------------------
-- Apply: update state after a gated event
-------------------------------------------------------------------

-- Apply a base decision to the full state
def applyBase {γ π ρ : Type}
    (s : FullState γ π ρ) (d : BaseDecision) : FullState γ π ρ :=
  { s with
    circle := applyBaseDecision s.circle d
    nextSeq := s.nextSeq + 1 }

-- Apply an application decision (fold update only)
def applyAppDecision {γ δ π ρ : Type}
    (s : FullState γ π ρ) (content : δ)
    (fApp : γ → δ → γ) : FullState γ π ρ :=
  { s with
    appState := fApp s.appState content
    nextSeq := s.nextSeq + 1 }

-- Apply a proposal: register it as open
def applyProposal {γ π ρ : Type}
    (s : FullState γ π ρ) (content : π)
    (proposer : MemberId) (deadline : Timestamp)
    : FullState γ π ρ :=
  { s with
    proposals := openProposal s.proposals s.nextSeq content proposer deadline
    nextSeq := s.nextSeq + 1 }

-- Apply a response: add to proposal
def applyResponse {γ π ρ : Type}
    (s : FullState γ π ρ) (content : ρ)
    (responder : MemberId) (proposalId : ProposalId)
    : FullState γ π ρ :=
  { s with
    proposals := addResponse s.proposals proposalId responder content
    nextSeq := s.nextSeq + 1 }

-- Apply a resolution: close the proposal
def applyResolve {γ π ρ : Type}
    (s : FullState γ π ρ) (proposalId : ProposalId)
    (r : Resolution) : FullState γ π ρ :=
  { s with
    proposals := resolveProposal s.proposals proposalId r
    nextSeq := s.nextSeq + 1 }

-------------------------------------------------------------------
-- Sequence number invariant
-------------------------------------------------------------------

-- Every apply increments the sequence number
theorem apply_base_increments_seq {γ π ρ : Type}
    (s : FullState γ π ρ) (d : BaseDecision) :
    (applyBase s d).nextSeq = s.nextSeq + 1 := by
  simp [applyBase]

theorem apply_app_decision_increments_seq {γ δ π ρ : Type}
    (s : FullState γ π ρ) (content : δ) (fApp : γ → δ → γ) :
    (applyAppDecision s content fApp).nextSeq = s.nextSeq + 1 := by
  simp [applyAppDecision]

theorem apply_proposal_increments_seq {γ π ρ : Type}
    (s : FullState γ π ρ) (content : π)
    (proposer : MemberId) (deadline : Timestamp) :
    (applyProposal s content proposer deadline).nextSeq = s.nextSeq + 1 := by
  simp [applyProposal]

theorem apply_response_increments_seq {γ π ρ : Type}
    (s : FullState γ π ρ) (content : ρ)
    (responder : MemberId) (proposalId : ProposalId) :
    (applyResponse s content responder proposalId).nextSeq = s.nextSeq + 1 := by
  simp [applyResponse]

theorem apply_resolve_increments_seq {γ π ρ : Type}
    (s : FullState γ π ρ) (proposalId : ProposalId)
    (r : Resolution) :
    (applyResolve s proposalId r).nextSeq = s.nextSeq + 1 := by
  simp [applyResolve]

-------------------------------------------------------------------
-- Base state preservation
-------------------------------------------------------------------

-- Application decisions don't change the circle
theorem app_decision_preserves_circle {γ δ π ρ : Type}
    (s : FullState γ π ρ) (content : δ) (fApp : γ → δ → γ) :
    (applyAppDecision s content fApp).circle = s.circle := by
  simp [applyAppDecision]

-- Proposals don't change the circle
theorem proposal_preserves_circle {γ π ρ : Type}
    (s : FullState γ π ρ) (content : π)
    (proposer : MemberId) (deadline : Timestamp) :
    (applyProposal s content proposer deadline).circle = s.circle := by
  simp [applyProposal]

-- Responses don't change the circle
theorem response_preserves_circle {γ π ρ : Type}
    (s : FullState γ π ρ) (content : ρ)
    (responder : MemberId) (proposalId : ProposalId) :
    (applyResponse s content responder proposalId).circle = s.circle := by
  simp [applyResponse]

-- Resolutions don't change the circle
theorem resolve_preserves_circle {γ π ρ : Type}
    (s : FullState γ π ρ) (proposalId : ProposalId)
    (r : Resolution) :
    (applyResolve s proposalId r).circle = s.circle := by
  simp [applyResolve]

-------------------------------------------------------------------
-- Proposal registry preservation
-------------------------------------------------------------------

-- Base decisions don't change the proposal registry
theorem base_preserves_proposals {γ π ρ : Type}
    (s : FullState γ π ρ) (d : BaseDecision) :
    (applyBase s d).proposals = s.proposals := by
  simp [applyBase]

-- Application decisions don't change the proposal registry
theorem app_decision_preserves_proposals {γ δ π ρ : Type}
    (s : FullState γ π ρ) (content : δ) (fApp : γ → δ → γ) :
    (applyAppDecision s content fApp).proposals = s.proposals := by
  simp [applyAppDecision]

-------------------------------------------------------------------
-- Gate/sequencer interaction
-------------------------------------------------------------------

-- Only the sequencer can resolve proposals
theorem non_sequencer_cannot_resolve {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (hne : signer ≠ s.circle.sequencerId) :
    gateResolve s signer = false := by
  simp [gateResolve, hne]

-- The sequencer can always resolve (it's always the sequencer)
theorem sequencer_can_resolve {γ π ρ : Type}
    (s : FullState γ π ρ) :
    gateResolve s s.circle.sequencerId = true := by
  simp [gateResolve]

-------------------------------------------------------------------
-- Genesis state
-------------------------------------------------------------------

-- The initial state has sequence number 1 (event 0 was genesis)
theorem init_seq_is_one {γ π ρ : Type}
    (sid : MemberId) (initApp : γ) :
    (initFullState sid initApp : FullState γ π ρ).nextSeq = 1 := by
  simp [initFullState]

-- The initial state has no proposals
theorem init_no_proposals {γ π ρ : Type}
    (sid : MemberId) (initApp : γ) :
    (initFullState sid initApp : FullState γ π ρ).proposals = [] := by
  simp [initFullState]

-- The initial state's sequencer is a member
theorem init_sequencer_is_member {γ π ρ : Type}
    (sid : MemberId) (initApp : γ) :
    isMember (initFullState sid initApp : FullState γ π ρ).circle.state sid := by
  exact genesis_sequencer_is_member sid

-------------------------------------------------------------------
-- Member KEL preservation
-------------------------------------------------------------------

-- All apply functions preserve the memberKels field because
-- they only modify circle, appState, proposals, and nextSeq.

-- Base decisions preserve memberKels
theorem applyBase_preserves_kels {γ π ρ : Type}
    (s : FullState γ π ρ) (d : BaseDecision) :
    (applyBase s d).memberKels = s.memberKels := by
  simp [applyBase]

-- Application decisions preserve memberKels
theorem applyAppDecision_preserves_kels {γ δ π ρ : Type}
    (s : FullState γ π ρ) (content : δ) (fApp : γ → δ → γ) :
    (applyAppDecision s content fApp).memberKels = s.memberKels := by
  simp [applyAppDecision]

-- Proposals preserve memberKels
theorem applyProposal_preserves_kels {γ π ρ : Type}
    (s : FullState γ π ρ) (content : π)
    (proposer : MemberId) (deadline : Timestamp) :
    (applyProposal s content proposer deadline).memberKels = s.memberKels := by
  simp [applyProposal]

-- Responses preserve memberKels
theorem applyResponse_preserves_kels {γ π ρ : Type}
    (s : FullState γ π ρ) (content : ρ)
    (responder : MemberId) (proposalId : ProposalId) :
    (applyResponse s content responder proposalId).memberKels = s.memberKels := by
  simp [applyResponse]

-- Resolutions preserve memberKels
theorem applyResolve_preserves_kels {γ π ρ : Type}
    (s : FullState γ π ρ) (proposalId : ProposalId)
    (r : Resolution) :
    (applyResolve s proposalId r).memberKels = s.memberKels := by
  simp [applyResolve]

-- The initial state has empty memberKels
theorem init_no_kels {γ π ρ : Type}
    (sid : MemberId) (initApp : γ) :
    (initFullState sid initApp : FullState γ π ρ).memberKels = [] := by
  simp [initFullState]

end KelCircle
