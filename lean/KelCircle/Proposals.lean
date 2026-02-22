-- Proposals: lifecycle, resolution, and invariants
--
-- A proposal opens a coordination round. It carries a mandatory
-- timeout and an optional threshold gate. Responses accumulate
-- until one of four resolution conditions is met. The sequencer
-- emits a decision event to close the proposal.

import KelCircle.BaseDecisions

namespace KelCircle

-- How a proposal was resolved
inductive Resolution where
  | thresholdReached    -- enough responses satisfied the threshold gate
  | proposerPositive    -- proposer closed with a positive outcome
  | proposerNegative    -- proposer cancelled
  | timeout             -- deadline passed without resolution
  deriving Repr, BEq, DecidableEq

-- A resolution is either positive (fold advances with responses)
-- or negative (fold records rejection, no payload applied)
def Resolution.isPositive : Resolution → Bool
  | .thresholdReached  => true
  | .proposerPositive  => true
  | .proposerNegative  => false
  | .timeout           => false

-- Proposal status in the lifecycle
inductive ProposalStatus where
  | open               -- accepting responses
  | resolved (r : Resolution)  -- closed with a resolution
  deriving Repr, BEq, DecidableEq

-- A tracked proposal: the protocol-level bookkeeping
-- Content types are abstract (application-defined)
structure TrackedProposal (π ρ : Type) where
  proposalId   : ProposalId
  content      : π
  proposer     : MemberId
  deadline     : Timestamp
  responses    : List ρ
  respondents  : List MemberId
  status       : ProposalStatus

-- The proposal registry: all tracked proposals
abbrev ProposalRegistry (π ρ : Type) := List (TrackedProposal π ρ)

-- Lookup a proposal by id
def findProposal {π ρ : Type}
    (reg : ProposalRegistry π ρ) (pid : ProposalId)
    : Option (TrackedProposal π ρ) :=
  reg.find? (fun p => p.proposalId == pid)

-- A proposal is open (pattern matching for proof-friendliness)
def TrackedProposal.isOpen {π ρ : Type}
    (p : TrackedProposal π ρ) : Bool :=
  match p.status with
  | .open => true
  | .resolved _ => false

-- A proposal is resolved
def TrackedProposal.isResolved {π ρ : Type}
    (p : TrackedProposal π ρ) : Bool :=
  match p.status with
  | .resolved _ => true
  | .open => false

-- Open a new proposal in the registry
def openProposal {π ρ : Type}
    (reg : ProposalRegistry π ρ) (pid : ProposalId)
    (content : π) (proposer : MemberId)
    (deadline : Timestamp) : ProposalRegistry π ρ :=
  { proposalId := pid
  , content := content
  , proposer := proposer
  , deadline := deadline
  , responses := []
  , respondents := []
  , status := .open
  } :: reg

-- Add a response to an open proposal
def addResponse {π ρ : Type}
    (reg : ProposalRegistry π ρ) (pid : ProposalId)
    (responder : MemberId) (response : ρ)
    : ProposalRegistry π ρ :=
  reg.map (fun p =>
    if p.proposalId == pid && p.isOpen then
      { p with
        responses := response :: p.responses
        respondents := responder :: p.respondents }
    else p)

-- Resolve a proposal with a given resolution
def resolveProposal {π ρ : Type}
    (reg : ProposalRegistry π ρ) (pid : ProposalId)
    (r : Resolution) : ProposalRegistry π ρ :=
  reg.map (fun p =>
    if p.proposalId == pid && p.isOpen then
      { p with status := .resolved r }
    else p)

-------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------

-- A newly opened proposal is open
theorem open_proposal_is_open {π ρ : Type}
    (reg : ProposalRegistry π ρ) (pid : ProposalId)
    (content : π) (proposer : MemberId) (deadline : Timestamp) :
    (openProposal reg pid content proposer deadline).head?.map
      TrackedProposal.isOpen = some true := by
  simp [openProposal, TrackedProposal.isOpen]

-- A newly opened proposal has no responses
theorem open_proposal_no_responses {π ρ : Type}
    (reg : ProposalRegistry π ρ) (pid : ProposalId)
    (content : π) (proposer : MemberId) (deadline : Timestamp) :
    (openProposal reg pid content proposer deadline).head?.map
      (fun p => p.responses.length) = some 0 := by
  simp [openProposal]

-- Resolution is idempotent: resolving an already-resolved proposal
-- does not change it (the isOpen check prevents it)
theorem resolve_resolved_noop {π ρ : Type}
    (p : TrackedProposal π ρ) (r1 r2 : Resolution)
    (hr : p.status = .resolved r1) :
    (if p.proposalId == p.proposalId && p.isOpen then
       { p with status := .resolved r2 }
     else p) = p := by
  simp [TrackedProposal.isOpen, hr]

-- Every resolution is either positive or negative
theorem resolution_dichotomy (r : Resolution) :
    r.isPositive = true ∨ r.isPositive = false := by
  cases r <;> simp [Resolution.isPositive]

-- Threshold and proposer-positive are positive
theorem threshold_is_positive :
    Resolution.thresholdReached.isPositive = true := rfl

theorem proposer_positive_is_positive :
    Resolution.proposerPositive.isPositive = true := rfl

-- Proposer-negative and timeout are negative
theorem proposer_negative_is_negative :
    Resolution.proposerNegative.isPositive = false := rfl

theorem timeout_is_negative :
    Resolution.timeout.isPositive = false := rfl

-------------------------------------------------------------------
-- Timeout guarantee: every proposal with a deadline will resolve
-------------------------------------------------------------------

-- If the current time exceeds the deadline and the proposal is
-- still open, it must be resolved as timeout. This is an
-- obligation on the sequencer, not a theorem about data — we
-- express it as a predicate that the sequencer must maintain.
def timeoutObligation {π ρ : Type}
    (p : TrackedProposal π ρ) (now : Timestamp) : Prop :=
  now > p.deadline → p.isOpen = true → False

-- Any resolved proposal satisfies the timeout obligation
theorem resolved_satisfies_obligation {π ρ : Type}
    (p : TrackedProposal π ρ) (now : Timestamp)
    (r : Resolution) (hr : p.status = .resolved r) :
    timeoutObligation p now := by
  intro _ hopen
  simp [TrackedProposal.isOpen, hr] at hopen

-------------------------------------------------------------------
-- Admin majority proposals (role changes)
-------------------------------------------------------------------

-- Admin role changes use the proposal mechanism with admin
-- majority as the threshold. This connects base decisions
-- (changeRole) to the proposal lifecycle.

-- An admin majority proposal: the threshold gate checks whether
-- the number of admin respondents meets the majority threshold.
def adminMajorityMet (s : CircleState) (respondents : List MemberId) : Bool :=
  let adminRespondents := respondents.filter (fun m => isAdminB s m)
  adminRespondents.length >= majority (adminCount s)

-- Single admin trivially meets majority with their own response
theorem single_admin_majority_self
    (s : CircleState) (aid : MemberId)
    (hadmin : isAdminB s aid = true)
    (hcount : adminCount s = 1) :
    adminMajorityMet s [aid] = true := by
  simp [adminMajorityMet, hadmin, hcount, majority]

-------------------------------------------------------------------
-- Response validation
-------------------------------------------------------------------

-- A member can only respond once per proposal
def hasNotResponded {π ρ : Type}
    (p : TrackedProposal π ρ) (m : MemberId) : Bool :=
  !(p.respondents.any (fun r => r == m))

-- Only open proposals accept responses
def canRespond {π ρ : Type}
    (p : TrackedProposal π ρ) (m : MemberId) : Bool :=
  p.isOpen && hasNotResponded p m

-- A fresh proposal accepts any member's response
theorem fresh_proposal_accepts_response {π ρ : Type}
    (pid : ProposalId) (content : π)
    (proposer : MemberId) (deadline : Timestamp)
    (m : MemberId) :
    let p : TrackedProposal π ρ :=
      { proposalId := pid
      , content := content
      , proposer := proposer
      , deadline := deadline
      , responses := []
      , respondents := []
      , status := .open }
    canRespond p m = true := by
  simp [canRespond, TrackedProposal.isOpen, hasNotResponded, List.any]

end KelCircle
