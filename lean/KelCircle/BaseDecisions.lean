-- BaseDecisions: membership transitions and their invariants
--
-- Base decisions are straight decisions that modify the circle's
-- membership. They are gated by admin role (except in bootstrap).

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

-- Base gate: checks whether a base decision is allowed
-- In bootstrap mode: only introduceMember with admin role
-- In normal mode: signer must be admin
def baseGate (s : CircleState) (signer : MemberId)
    (d : BaseDecision) : Bool :=
  if isBootstrapB s then
    match d with
    | .introduceMember _ .admin => true
    | _ => false
  else
    isAdminB s signer

-- Bootstrap gate accepts admin introduction
theorem bootstrap_accepts_admin_intro (s : CircleState)
    (signer : MemberId) (id : MemberId)
    (hboot : isBootstrapB s = true) :
    baseGate s signer (.introduceMember id .admin) = true := by
  simp [baseGate, hboot]

-- Bootstrap gate rejects member introduction
theorem bootstrap_rejects_member_intro (s : CircleState)
    (signer : MemberId) (id : MemberId)
    (hboot : isBootstrapB s = true) :
    baseGate s signer (.introduceMember id .member) = false := by
  simp [baseGate, hboot]

-- Bootstrap gate rejects removal
theorem bootstrap_rejects_remove (s : CircleState)
    (signer : MemberId) (id : MemberId)
    (hboot : isBootstrapB s = true) :
    baseGate s signer (.removeMember id) = false := by
  simp [baseGate, hboot]

end KelCircle
