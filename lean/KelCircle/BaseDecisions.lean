-- BaseDecisions: membership transitions and their invariants
--
-- Base decisions are straight decisions that modify the circle's
-- membership. They are gated by admin role (except in bootstrap).
-- Event 0 is always the sequencer introducing itself as a non-admin
-- member. The sequencer can never be removed or promoted to admin.

import KelCircle.Invariants

namespace KelCircle

-- Base decisions: the protocol-level operations on membership
-- rotateSequencer replaces the current sequencer with a new identity.
-- The old sequencer stays as a regular member.
inductive BaseDecision where
  | introduceMember   (id : MemberId) (role : Role)
  | removeMember      (id : MemberId)
  | changeRole        (id : MemberId) (newRole : Role)
  | rotateSequencer   (newSequencerId : MemberId)
  deriving Repr

-- Count admins in the circle
def adminCount (s : CircleState) : Nat :=
  s.members.filter (fun m => m.role == Role.admin) |>.length

-- Boolean membership check
def isMemberB (s : CircleState) (m : MemberId) : Bool :=
  s.members.any (fun mem => mem.id == m)

-- Boolean admin check
def isAdminB (s : CircleState) (m : MemberId) : Bool :=
  s.members.any (fun mem => mem.id == m && mem.role == Role.admin)

-- Bootstrap mode as a boolean (computable)
def isBootstrapB (s : CircleState) : Bool :=
  s.members.all (fun m => !(m.role == Role.admin))

-- Full circle state: members + current sequencer identity
structure Circle where
  state       : CircleState
  sequencerId : MemberId

-- Apply a base decision to the circle
def applyBaseDecision (c : Circle) (d : BaseDecision) : Circle :=
  match d with
  | .introduceMember id role =>
    { c with state := ⟨⟨id, role⟩ :: c.state.members⟩ }
  | .removeMember id =>
    { c with state := ⟨c.state.members.filter (fun m => !(m.id == id))⟩ }
  | .changeRole id newRole =>
    { c with state := ⟨c.state.members.map (fun m =>
        if m.id == id then ⟨id, newRole⟩ else m)⟩ }
  | .rotateSequencer newSid =>
    { state := ⟨⟨newSid, .member⟩ :: c.state.members⟩
    , sequencerId := newSid }

-- Introducing a member adds them to the member list
theorem introduce_adds_member (c : Circle) (id : MemberId) (r : Role) :
    isMember (applyBaseDecision c (.introduceMember id r)).state id := by
  exact ⟨⟨id, r⟩, .head _, rfl⟩

-- Introducing an admin exits bootstrap mode
theorem introduce_admin_exits_bootstrap
    (c : Circle) (id : MemberId) :
    ¬isBootstrap (applyBaseDecision c (.introduceMember id .admin)).state := by
  simp [isBootstrap]
  exact ⟨id, ⟨id, .admin⟩, .head _, rfl, rfl⟩

-- Introducing a member preserves existing members
theorem introduce_preserves_existing
    (c : Circle) (id : MemberId) (r : Role)
    (m : MemberId) (hm : isMember c.state m) :
    isMember (applyBaseDecision c (.introduceMember id r)).state m := by
  obtain ⟨mem, hmem, hid⟩ := hm
  exact ⟨mem, .tail _ hmem, hid⟩

-------------------------------------------------------------------
-- Genesis: event 0 is the sequencer introducing itself
-------------------------------------------------------------------

-- The empty circle with a sequencer id
def emptyCircleFor (sid : MemberId) : Circle :=
  { state := emptyCircle, sequencerId := sid }

-- The genesis state: sequencer is a non-admin member
def genesis (sid : MemberId) : Circle :=
  applyBaseDecision (emptyCircleFor sid) (.introduceMember sid .member)

-- After genesis, the sequencer is a member
theorem genesis_sequencer_is_member (sid : MemberId) :
    isMember (genesis sid).state sid := by
  exact ⟨⟨sid, .member⟩, .head _, rfl⟩

-- After genesis, the sequencer is NOT an admin
theorem genesis_sequencer_not_admin (sid : MemberId) :
    ¬isAdmin (genesis sid).state sid := by
  intro ⟨mem, hmem, _, hrole⟩
  simp [genesis, applyBaseDecision, emptyCircleFor, emptyCircle] at hmem
  subst hmem
  simp at hrole

-- After genesis, still in bootstrap mode (no admins)
theorem genesis_is_bootstrap (sid : MemberId) :
    isBootstrap (genesis sid).state := by
  intro ⟨_, mem, hmem, _, hrole⟩
  simp [genesis, applyBaseDecision, emptyCircleFor, emptyCircle] at hmem
  subst hmem
  simp at hrole

-- Genesis preserves the sequencer id
theorem genesis_preserves_sequencer_id (sid : MemberId) :
    (genesis sid).sequencerId = sid := by
  simp [genesis, applyBaseDecision, emptyCircleFor]

-------------------------------------------------------------------
-- Sequencer protection: the base gate enforces these
-------------------------------------------------------------------

-- The sequencer cannot be removed or promoted to admin.
-- Sequencer rotation is not a straight decision — it requires
-- admin majority proposal, so it's rejected by the straight gate.
def protectsSequencer (sid : MemberId) (d : BaseDecision) : Bool :=
  match d with
  | .removeMember id => !(id == sid)
  | .changeRole id .admin => !(id == sid)
  | .rotateSequencer _ => false  -- requires majority, not straight
  | _ => true

-- Base gate for straight decisions (not proposals).
-- Sequencer rotation is always rejected here — it must go
-- through a majority proposal instead.
def baseGate (s : CircleState) (signer : MemberId)
    (sequencerId : MemberId) (d : BaseDecision) : Bool :=
  protectsSequencer sequencerId d &&
  if isBootstrapB s then
    match d with
    | .introduceMember _ .admin => true
    | _ => false
  else
    isAdminB s signer

-- Bootstrap gate accepts admin introduction (when sequencer safe)
theorem bootstrap_accepts_admin_intro (s : CircleState)
    (signer : MemberId) (sid : MemberId) (id : MemberId)
    (hboot : isBootstrapB s = true) :
    baseGate s signer sid (.introduceMember id .admin) = true := by
  simp [baseGate, protectsSequencer, hboot]

-- Sequencer removal is always rejected
theorem sequencer_removal_rejected (s : CircleState)
    (signer : MemberId) (sid : MemberId) :
    baseGate s signer sid (.removeMember sid) = false := by
  simp [baseGate, protectsSequencer]

-- Sequencer promotion to admin is always rejected
theorem sequencer_admin_promotion_rejected (s : CircleState)
    (signer : MemberId) (sid : MemberId) :
    baseGate s signer sid (.changeRole sid .admin) = false := by
  simp [baseGate, protectsSequencer]

-- Bootstrap gate rejects member (non-admin) introduction
theorem bootstrap_rejects_member_intro (s : CircleState)
    (signer : MemberId) (sid : MemberId) (id : MemberId)
    (hboot : isBootstrapB s = true) :
    baseGate s signer sid (.introduceMember id .member) = false := by
  simp [baseGate, protectsSequencer, hboot]

-- Bootstrap gate rejects removal
theorem bootstrap_rejects_remove (s : CircleState)
    (signer : MemberId) (sid : MemberId) (id : MemberId)
    (hboot : isBootstrapB s = true)
    (hne : (id == sid) = false) :
    baseGate s signer sid (.removeMember id) = false := by
  simp [baseGate, protectsSequencer, hboot, hne]

-------------------------------------------------------------------
-- Admin majority for role changes
-------------------------------------------------------------------

-- Majority threshold: strictly more than half
def majority (n : Nat) : Nat := n / 2 + 1

-- A role change proposal has reached majority when the number
-- of admin approvals meets the majority threshold
def hasAdminMajority (s : CircleState) (approvalCount : Nat) : Bool :=
  approvalCount >= majority (adminCount s)

-- Single admin majority is trivially 1
theorem single_admin_majority :
    majority 1 = 1 := by
  simp [majority]

-------------------------------------------------------------------
-- Demotion and bootstrap re-entry
-------------------------------------------------------------------

-- Demoting the sole admin of a single-member-admin state
-- returns to bootstrap mode.
-- We prove: if a state has exactly one admin with id `aid`,
-- then changing that admin to member yields bootstrap.
theorem demote_sole_admin_enters_bootstrap
    (sid aid : MemberId) (hne : sid ≠ aid) :
    let c := applyBaseDecision (genesis sid)
               (.introduceMember aid .admin)
    isBootstrapB (applyBaseDecision c (.changeRole aid .member)).state = true := by
  simp [genesis, applyBaseDecision, emptyCircleFor, emptyCircle,
        isBootstrapB, List.all, List.map, hne]
  decide

-- After demotion of sole admin, bootstrap gate accepts new admin
theorem demote_then_bootstrap_accepts_admin
    (sid aid newAdmin : MemberId)
    (signer : MemberId) (hne : sid ≠ aid) :
    let c := applyBaseDecision (genesis sid)
               (.introduceMember aid .admin)
    let c' := applyBaseDecision c (.changeRole aid .member)
    baseGate c'.state signer sid (.introduceMember newAdmin .admin) = true := by
  simp [genesis, applyBaseDecision, emptyCircleFor, emptyCircle,
        baseGate, protectsSequencer, isBootstrapB,
        List.all, List.map, hne]
  left; decide

-- Sequencer survives demotion: still a member after role change
theorem sequencer_survives_demotion
    (sid aid : MemberId) (hne : sid ≠ aid) :
    let c := applyBaseDecision (genesis sid)
               (.introduceMember aid .admin)
    let c' := applyBaseDecision c (.changeRole aid .member)
    isMemberB c'.state sid = true := by
  simp [genesis, applyBaseDecision, emptyCircleFor, emptyCircle,
        isMemberB, List.any, List.map, hne]

-------------------------------------------------------------------
-- Sequencer rotation (requires admin majority proposal)
-------------------------------------------------------------------

-- Sequencer rotation is rejected by the straight gate
theorem rotate_rejected_by_straight_gate (s : CircleState)
    (signer : MemberId) (sid newSid : MemberId) :
    baseGate s signer sid (.rotateSequencer newSid) = false := by
  simp [baseGate, protectsSequencer]

-- After rotation, the new sequencer is a member
theorem rotate_new_sequencer_is_member
    (c : Circle) (newSid : MemberId) :
    isMember (applyBaseDecision c (.rotateSequencer newSid)).state newSid := by
  exact ⟨⟨newSid, .member⟩, .head _, rfl⟩

-- After rotation, the old sequencer is still a member
theorem rotate_old_sequencer_stays_member
    (c : Circle) (newSid : MemberId)
    (hold : isMember c.state c.sequencerId) :
    isMember (applyBaseDecision c (.rotateSequencer newSid)).state c.sequencerId := by
  obtain ⟨mem, hmem, hid⟩ := hold
  exact ⟨mem, .tail _ hmem, hid⟩

-- After rotation, the sequencer id is updated
theorem rotate_updates_sequencer_id
    (c : Circle) (newSid : MemberId) :
    (applyBaseDecision c (.rotateSequencer newSid)).sequencerId = newSid := by
  simp [applyBaseDecision]

end KelCircle
