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

-- Initial state for a circle (sequencer has inception KEL)
def initFullState {γ π ρ : Type}
    (sid : MemberId) (initApp : γ) : FullState γ π ρ :=
  { circle     := genesis sid
  , appState   := initApp
  , proposals  := []
  , nextSeq    := 1  -- event 0 was genesis
  , memberKels := insertKel [] sid
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

-- Gate for proposals: signer must be a member, no duplicate open
-- proposal with the same content
def gateProposal {γ π ρ : Type} [BEq π]
    (s : FullState γ π ρ) (signer : MemberId)
    (content : π) (appGate : γ → π → Bool) : Bool :=
  isMemberB s.circle.state signer
    && appGate s.appState content
    && !hasOpenProposalWithContent s.proposals content

-- Gate for responses: signer must be an admin, proposal must be
-- open, signer must not have already responded
def gateResponse {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (proposalId : ProposalId) : Bool :=
  isAdminB s.circle.state signer &&
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

-- Apply a base decision to the full state.
-- The signer's KEL gets an interaction event appended, then
-- membership changes update memberKels accordingly.
def applyBase {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (d : BaseDecision) : FullState γ π ρ :=
  let kels := appendKelEvent s.memberKels signer
  { s with
    circle := applyBaseDecision s.circle d
    nextSeq := s.nextSeq + 1
    memberKels := match d with
      | .introduceMember mid _ => insertKel kels mid
      | .removeMember mid => removeKel kels mid
      | _ => kels }

-- Apply an application decision (fold update + signer KEL append)
def applyAppDecision {γ δ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId) (content : δ)
    (fApp : γ → δ → γ) : FullState γ π ρ :=
  { s with
    appState := fApp s.appState content
    nextSeq := s.nextSeq + 1
    memberKels := appendKelEvent s.memberKels signer }

-- Apply a proposal: register it as open + signer KEL append
def applyProposal {γ π ρ : Type}
    (s : FullState γ π ρ) (content : π)
    (proposer : MemberId) (deadline : Timestamp)
    : FullState γ π ρ :=
  { s with
    proposals := openProposal s.proposals s.nextSeq content proposer deadline
    nextSeq := s.nextSeq + 1
    memberKels := appendKelEvent s.memberKels proposer }

-- Apply a response: add to proposal + signer KEL append
def applyResponse {γ π ρ : Type}
    (s : FullState γ π ρ) (content : ρ)
    (responder : MemberId) (proposalId : ProposalId)
    : FullState γ π ρ :=
  { s with
    proposals := addResponse s.proposals proposalId responder content
    nextSeq := s.nextSeq + 1
    memberKels := appendKelEvent s.memberKels responder }

-- Apply a resolution: close the proposal + signer KEL append
def applyResolve {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (proposalId : ProposalId)
    (r : Resolution) : FullState γ π ρ :=
  { s with
    proposals := resolveProposal s.proposals proposalId r
    nextSeq := s.nextSeq + 1
    memberKels := appendKelEvent s.memberKels signer }

-------------------------------------------------------------------
-- Sequence number invariant
-------------------------------------------------------------------

-- Every apply increments the sequence number
theorem apply_base_increments_seq {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId) (d : BaseDecision) :
    (applyBase s signer d).nextSeq = s.nextSeq + 1 := by
  simp [applyBase]

theorem apply_app_decision_increments_seq {γ δ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (content : δ) (fApp : γ → δ → γ) :
    (applyAppDecision s signer content fApp).nextSeq = s.nextSeq + 1 := by
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
    (s : FullState γ π ρ) (signer : MemberId)
    (proposalId : ProposalId) (r : Resolution) :
    (applyResolve s signer proposalId r).nextSeq = s.nextSeq + 1 := by
  simp [applyResolve]

-------------------------------------------------------------------
-- Base state preservation
-------------------------------------------------------------------

-- Application decisions don't change the circle
theorem app_decision_preserves_circle {γ δ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (content : δ) (fApp : γ → δ → γ) :
    (applyAppDecision s signer content fApp).circle = s.circle := by
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
    (s : FullState γ π ρ) (signer : MemberId)
    (proposalId : ProposalId) (r : Resolution) :
    (applyResolve s signer proposalId r).circle = s.circle := by
  simp [applyResolve]

-------------------------------------------------------------------
-- Proposal registry preservation
-------------------------------------------------------------------

-- Base decisions don't change the proposal registry
theorem base_preserves_proposals {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId) (d : BaseDecision) :
    (applyBase s signer d).proposals = s.proposals := by
  simp [applyBase]

-- Application decisions don't change the proposal registry
theorem app_decision_preserves_proposals {γ δ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (content : δ) (fApp : γ → δ → γ) :
    (applyAppDecision s signer content fApp).proposals = s.proposals := by
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
-- Member KEL updates
-------------------------------------------------------------------

-- Apply functions now update memberKels (append signer's KEL event,
-- plus membership changes for base decisions).

-- applyAppDecision appends to signer's KEL
theorem applyAppDecision_updates_kels {γ δ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (content : δ) (fApp : γ → δ → γ) :
    (applyAppDecision s signer content fApp).memberKels =
      appendKelEvent s.memberKels signer := by
  simp [applyAppDecision]

-- applyProposal appends to proposer's KEL
theorem applyProposal_updates_kels {γ π ρ : Type}
    (s : FullState γ π ρ) (content : π)
    (proposer : MemberId) (deadline : Timestamp) :
    (applyProposal s content proposer deadline).memberKels =
      appendKelEvent s.memberKels proposer := by
  simp [applyProposal]

-- applyResponse appends to responder's KEL
theorem applyResponse_updates_kels {γ π ρ : Type}
    (s : FullState γ π ρ) (content : ρ)
    (responder : MemberId) (proposalId : ProposalId) :
    (applyResponse s content responder proposalId).memberKels =
      appendKelEvent s.memberKels responder := by
  simp [applyResponse]

-- applyResolve appends to signer's KEL
theorem applyResolve_updates_kels {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (proposalId : ProposalId) (r : Resolution) :
    (applyResolve s signer proposalId r).memberKels =
      appendKelEvent s.memberKels signer := by
  simp [applyResolve]

-------------------------------------------------------------------
-- KEL invariant preservation for apply functions
-------------------------------------------------------------------

-- applyAppDecision preserves allMembersHaveKel
theorem applyAppDecision_preserves_allMembersHaveKel {γ δ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (content : δ) (fApp : γ → δ → γ)
    (h : allMembersHaveKel s.circle.state.members s.memberKels) :
    allMembersHaveKel
      (applyAppDecision s signer content fApp).circle.state.members
      (applyAppDecision s signer content fApp).memberKels := by
  simp [applyAppDecision]
  exact appendKelEvent_preserves_allMembersHaveKel
    s.circle.state.members s.memberKels signer h

-- applyProposal preserves allMembersHaveKel
theorem applyProposal_preserves_allMembersHaveKel {γ π ρ : Type}
    (s : FullState γ π ρ) (content : π)
    (proposer : MemberId) (deadline : Timestamp)
    (h : allMembersHaveKel s.circle.state.members s.memberKels) :
    allMembersHaveKel
      (applyProposal s content proposer deadline).circle.state.members
      (applyProposal s content proposer deadline).memberKels := by
  simp [applyProposal]
  exact appendKelEvent_preserves_allMembersHaveKel
    s.circle.state.members s.memberKels proposer h

-- applyResponse preserves allMembersHaveKel
theorem applyResponse_preserves_allMembersHaveKel {γ π ρ : Type}
    (s : FullState γ π ρ) (content : ρ)
    (responder : MemberId) (proposalId : ProposalId)
    (h : allMembersHaveKel s.circle.state.members s.memberKels) :
    allMembersHaveKel
      (applyResponse s content responder proposalId).circle.state.members
      (applyResponse s content responder proposalId).memberKels := by
  simp [applyResponse]
  exact appendKelEvent_preserves_allMembersHaveKel
    s.circle.state.members s.memberKels responder h

-- applyResolve preserves allMembersHaveKel
theorem applyResolve_preserves_allMembersHaveKel {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (proposalId : ProposalId) (r : Resolution)
    (h : allMembersHaveKel s.circle.state.members s.memberKels) :
    allMembersHaveKel
      (applyResolve s signer proposalId r).circle.state.members
      (applyResolve s signer proposalId r).memberKels := by
  simp [applyResolve]
  exact appendKelEvent_preserves_allMembersHaveKel
    s.circle.state.members s.memberKels signer h

-------------------------------------------------------------------
-- Genesis KEL invariant
-------------------------------------------------------------------

-- The initial state has the sequencer's KEL
theorem init_sequencer_has_kel {γ π ρ : Type}
    (sid : MemberId) (initApp : γ) :
    ∃ k, k ∈ (initFullState sid initApp : FullState γ π ρ).memberKels
      ∧ k.1 = sid := by
  exact ⟨(sid, ⟨sid, 1, 0⟩), .head _, rfl⟩

-------------------------------------------------------------------
-- Proposal invariant: response requires admin
-------------------------------------------------------------------

-- A non-admin member cannot pass the response gate
theorem gateResponse_requires_admin {γ π ρ : Type}
    (s : FullState γ π ρ) (signer : MemberId)
    (pid : ProposalId)
    (hnotadmin : isAdminB s.circle.state signer = false) :
    gateResponse s signer pid = false := by
  simp [gateResponse, hnotadmin]

-------------------------------------------------------------------
-- Proposal invariant: no duplicate proposals
-------------------------------------------------------------------

-- If an open proposal with the same content exists, the gate rejects
theorem gateProposal_rejects_duplicate {γ π ρ : Type} [BEq π]
    (s : FullState γ π ρ) (signer : MemberId)
    (content : π) (appGate : γ → π → Bool)
    (hdup : hasOpenProposalWithContent s.proposals content = true) :
    gateProposal s signer content appGate = false := by
  simp [gateProposal, hdup]

end KelCircle
