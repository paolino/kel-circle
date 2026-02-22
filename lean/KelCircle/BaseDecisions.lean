-- BaseDecisions: membership transitions and their invariants
--
-- Base decisions are straight decisions that modify the circle's
-- membership. They are gated by admin role (except in bootstrap).
-- Event 0 is always the sequencer introducing itself as a non-admin
-- member. The sequencer can never be removed or promoted to admin.

import KelCircle.Invariants

namespace KelCircle

-- Base decisions: the protocol-level operations on membership
inductive BaseDecision where
  | introduceMember (id : MemberId) (role : Role)
  | removeMember    (id : MemberId)
  | changeRole      (id : MemberId) (newRole : Role)
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

-- Apply a base decision to the circle state
def applyBaseDecision (s : CircleState) (d : BaseDecision) : CircleState :=
  match d with
  | .introduceMember id role =>
    ⟨⟨id, role⟩ :: s.members⟩
  | .removeMember id =>
    ⟨s.members.filter (fun m => !(m.id == id))⟩
  | .changeRole id newRole =>
    ⟨s.members.map (fun m =>
      if m.id == id then ⟨id, newRole⟩ else m)⟩

-- Introducing a member adds them to the member list
theorem introduce_adds_member (s : CircleState) (id : MemberId) (r : Role) :
    isMember (applyBaseDecision s (.introduceMember id r)) id := by
  exact ⟨⟨id, r⟩, .head _, rfl⟩

-- Introducing an admin exits bootstrap mode
theorem introduce_admin_exits_bootstrap
    (s : CircleState) (id : MemberId) :
    ¬isBootstrap (applyBaseDecision s (.introduceMember id .admin)) := by
  simp [isBootstrap]
  exact ⟨id, ⟨id, .admin⟩, .head _, rfl, rfl⟩

-- Introducing a member preserves existing members
theorem introduce_preserves_existing
    (s : CircleState) (id : MemberId) (r : Role)
    (m : MemberId) (hm : isMember s m) :
    isMember (applyBaseDecision s (.introduceMember id r)) m := by
  obtain ⟨mem, hmem, hid⟩ := hm
  exact ⟨mem, .tail _ hmem, hid⟩

-------------------------------------------------------------------
-- Genesis: event 0 is the sequencer introducing itself
-------------------------------------------------------------------

-- The genesis state: sequencer is a non-admin member
def genesisState (sequencerId : MemberId) : CircleState :=
  applyBaseDecision emptyCircle (.introduceMember sequencerId .member)

-- After genesis, the sequencer is a member
theorem genesis_sequencer_is_member (sid : MemberId) :
    isMember (genesisState sid) sid := by
  exact ⟨⟨sid, .member⟩, .head _, rfl⟩

-- After genesis, the sequencer is NOT an admin
theorem genesis_sequencer_not_admin (sid : MemberId) :
    ¬isAdmin (genesisState sid) sid := by
  intro ⟨mem, hmem, _, hrole⟩
  simp [genesisState, applyBaseDecision, emptyCircle] at hmem
  subst hmem
  simp at hrole

-- After genesis, still in bootstrap mode (no admins)
theorem genesis_is_bootstrap (sid : MemberId) :
    isBootstrap (genesisState sid) := by
  intro ⟨_, mem, hmem, _, hrole⟩
  simp [genesisState, applyBaseDecision, emptyCircle] at hmem
  subst hmem
  simp at hrole

-------------------------------------------------------------------
-- Sequencer protection: the base gate enforces these
-------------------------------------------------------------------

-- The sequencer cannot be removed
def protectsSequencer (sid : MemberId) (d : BaseDecision) : Bool :=
  match d with
  | .removeMember id => !(id == sid)
  | .changeRole id .admin => !(id == sid)
  | _ => true

-- Base gate with sequencer protection
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
    let s := applyBaseDecision (genesisState sid)
               (.introduceMember aid .admin)
    isBootstrapB (applyBaseDecision s (.changeRole aid .member)) = true := by
  simp [genesisState, applyBaseDecision, emptyCircle,
        isBootstrapB, List.all, List.map, hne]
  decide

-- After demotion of sole admin, bootstrap gate accepts new admin
theorem demote_then_bootstrap_accepts_admin
    (sid aid newAdmin : MemberId)
    (signer : MemberId) (hne : sid ≠ aid) :
    let s := applyBaseDecision (genesisState sid)
               (.introduceMember aid .admin)
    let s' := applyBaseDecision s (.changeRole aid .member)
    baseGate s' signer sid (.introduceMember newAdmin .admin) = true := by
  simp [genesisState, applyBaseDecision, emptyCircle,
        baseGate, protectsSequencer, isBootstrapB,
        List.all, List.map, hne]
  left; decide

-- Sequencer survives demotion: still a member after role change
theorem sequencer_survives_demotion
    (sid aid : MemberId) (hne : sid ≠ aid) :
    let s := applyBaseDecision (genesisState sid)
               (.introduceMember aid .admin)
    let s' := applyBaseDecision s (.changeRole aid .member)
    isMemberB s' sid = true := by
  simp [genesisState, applyBaseDecision, emptyCircle,
        isMemberB, List.any, List.map, hne]

end KelCircle
