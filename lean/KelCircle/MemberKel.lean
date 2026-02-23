-- MemberKel: per-member KEL storage invariants
--
-- Each introduced member has a Key Event Log (KEL) starting with
-- an inception event. The memberKels map tracks these logs.
-- Key invariants: every circle member has a KEL entry, and
-- KEL owner IDs match member IDs.

import KelCircle.Events

namespace KelCircle

-------------------------------------------------------------------
-- Abstract KEL model
-------------------------------------------------------------------

-- A member's KEL: owner ID + event count (at least 1 for inception)
structure MemberKel where
  ownerId : MemberId
  eventCount : Nat
  deriving Repr, DecidableEq

-- The map from member IDs to their KELs
abbrev MemberKels := List (MemberId × MemberKel)

-------------------------------------------------------------------
-- Core invariants
-------------------------------------------------------------------

-- Every circle member has a KEL entry
def allMembersHaveKel
    (members : List Member) (kels : MemberKels) : Prop :=
  ∀ m, m ∈ members → ∃ k, k ∈ kels ∧ k.1 = m.id

-- KEL owner matches the member ID key
def kelOwnersMatch (kels : MemberKels) : Prop :=
  ∀ k, k ∈ kels → k.2.ownerId = k.1

-- A KEL has at least one event (the inception)
def kelNonEmpty (kels : MemberKels) : Prop :=
  ∀ k, k ∈ kels → k.2.eventCount ≥ 1

-------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------

-- Insert a new KEL entry (inception creates a KEL with 1 event)
def insertKel (kels : MemberKels) (mid : MemberId)
    : MemberKels :=
  (mid, ⟨mid, 1⟩) :: kels

-- Remove a KEL entry
def removeKel (kels : MemberKels) (mid : MemberId)
    : MemberKels :=
  kels.filter (fun k => decide (k.1 ≠ mid))

-- Look up a KEL by member ID
def lookupKel (kels : MemberKels) (mid : MemberId)
    : Option MemberKel :=
  match kels.find? (fun k => decide (k.1 = mid)) with
  | some k => some k.2
  | none => none

-------------------------------------------------------------------
-- Preservation theorems
-------------------------------------------------------------------

-- Inserting a KEL preserves allMembersHaveKel for existing members
theorem insert_preserves_allMembersHaveKel
    (members : List Member) (kels : MemberKels)
    (mid : MemberId)
    (h : allMembersHaveKel members kels) :
    allMembersHaveKel members (insertKel kels mid) := by
  intro m hm
  obtain ⟨k, hk, heq⟩ := h m hm
  exact ⟨k, .tail _ hk, heq⟩

-- After inserting member + KEL, the new member has a KEL
theorem insert_new_member_has_kel
    (kels : MemberKels) (mid : MemberId) :
    ∃ k, k ∈ insertKel kels mid ∧ k.1 = mid := by
  exact ⟨(mid, ⟨mid, 1⟩), .head _, rfl⟩

-- Introducing member + KEL atomically preserves the invariant
theorem introduce_with_kel_preserves_allMembersHaveKel
    (members : List Member) (kels : MemberKels)
    (newMem : Member)
    (h : allMembersHaveKel members kels) :
    allMembersHaveKel (newMem :: members)
      (insertKel kels newMem.id) := by
  intro m hm
  cases hm with
  | head => exact insert_new_member_has_kel kels newMem.id
  | tail _ hm' =>
    obtain ⟨k, hk, heq⟩ := h m hm'
    exact ⟨k, .tail _ hk, heq⟩

-- Removing member + KEL preserves invariant for remaining members
theorem remove_member_preserves_allMembersHaveKel
    (members : List Member) (kels : MemberKels)
    (mid : MemberId)
    (h : allMembersHaveKel members kels)
    (hmem : ∀ m, m ∈ members → m.id ≠ mid) :
    allMembersHaveKel members (removeKel kels mid) := by
  intro m hm_in
  obtain ⟨k, hk, heq⟩ := h m hm_in
  have hne : k.1 ≠ mid := by rw [heq]; exact hmem m hm_in
  exact ⟨k, List.mem_filter.mpr ⟨hk, decide_eq_true hne⟩, heq⟩

-- insertKel preserves kelOwnersMatch
theorem insert_preserves_kelOwnersMatch
    (kels : MemberKels) (mid : MemberId)
    (h : kelOwnersMatch kels) :
    kelOwnersMatch (insertKel kels mid) := by
  intro k hk
  cases hk with
  | head => rfl
  | tail _ hk' => exact h k hk'

-- insertKel preserves kelNonEmpty
theorem insert_preserves_kelNonEmpty
    (kels : MemberKels) (mid : MemberId)
    (h : kelNonEmpty kels) :
    kelNonEmpty (insertKel kels mid) := by
  intro k hk
  cases hk with
  | head => simp
  | tail _ hk' => exact h k hk'

-------------------------------------------------------------------
-- Interaction event append
-------------------------------------------------------------------

-- Append a KEL event (interaction): increment event count
def appendKelEvent (kels : MemberKels) (mid : MemberId)
    : MemberKels :=
  kels.map (fun k =>
    if k.1 = mid
    then (k.1, ⟨k.2.ownerId, k.2.eventCount + 1⟩)
    else k)

-- After append, the member's event count increases by 1
theorem appendKelEvent_grows
    (kels : MemberKels) (mid : MemberId)
    (k : MemberId × MemberKel)
    (hk : k ∈ kels) (heq : k.1 = mid) :
    ∃ k', k' ∈ appendKelEvent kels mid
      ∧ k'.1 = mid
      ∧ k'.2.eventCount = k.2.eventCount + 1 := by
  unfold appendKelEvent
  refine ⟨(k.1, ⟨k.2.ownerId, k.2.eventCount + 1⟩),
    List.mem_map.mpr ⟨k, hk, ?_⟩, heq, rfl⟩
  simp [heq]

-- Other members' KELs are unchanged
theorem appendKelEvent_preserves_others
    (kels : MemberKels) (mid : MemberId)
    (k : MemberId × MemberKel)
    (hk : k ∈ kels) (hne : k.1 ≠ mid) :
    k ∈ appendKelEvent kels mid := by
  unfold appendKelEvent
  exact List.mem_map.mpr ⟨k, hk, if_neg hne⟩

-- appendKelEvent preserves allMembersHaveKel
theorem appendKelEvent_preserves_allMembersHaveKel
    (members : List Member) (kels : MemberKels)
    (mid : MemberId)
    (h : allMembersHaveKel members kels) :
    allMembersHaveKel members
      (appendKelEvent kels mid) := by
  intro m hm
  obtain ⟨k, hk, heq⟩ := h m hm
  by_cases hmid : k.1 = mid
  · obtain ⟨k', hk'mem, hk'eq, _⟩ :=
      appendKelEvent_grows kels mid k hk hmid
    exact ⟨k', hk'mem,
      hk'eq.trans (hmid.symm.trans heq)⟩
  · exact ⟨k, appendKelEvent_preserves_others
      kels mid k hk hmid, heq⟩

-- appendKelEvent preserves kelOwnersMatch
theorem appendKelEvent_preserves_kelOwnersMatch
    (kels : MemberKels) (mid : MemberId)
    (h : kelOwnersMatch kels) :
    kelOwnersMatch (appendKelEvent kels mid) := by
  unfold appendKelEvent kelOwnersMatch
  intro k' hk'
  obtain ⟨k, hk, hfk⟩ := List.mem_map.mp hk'
  have h_own := h k hk
  by_cases hmid : k.1 = mid
  · rw [if_pos hmid] at hfk; rw [← hfk]; exact h_own
  · rw [if_neg hmid] at hfk; rw [← hfk]; exact h_own

-- appendKelEvent preserves kelNonEmpty
theorem appendKelEvent_preserves_kelNonEmpty
    (kels : MemberKels) (mid : MemberId)
    (h : kelNonEmpty kels) :
    kelNonEmpty (appendKelEvent kels mid) := by
  unfold appendKelEvent kelNonEmpty
  intro k' hk'
  obtain ⟨k, hk, hfk⟩ := List.mem_map.mp hk'
  have h_ne := h k hk
  by_cases hmid : k.1 = mid
  · rw [if_pos hmid] at hfk; rw [← hfk]; simp
  · rw [if_neg hmid] at hfk; rw [← hfk]; exact h_ne

-------------------------------------------------------------------
-- Non-membership events don't affect KELs
-------------------------------------------------------------------

-- These theorems are proven in Processing.lean where the
-- apply functions are defined. The key property is that
-- applyBase, applyAppDecision, applyProposal, applyResponse,
-- and applyResolve all preserve the memberKels field.

end KelCircle
