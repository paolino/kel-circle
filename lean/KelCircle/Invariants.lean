-- Invariants: preservation theorems and membership model
--
-- Structural invariants for the global sequence, membership
-- as a base-layer concern, bootstrap mode, and admin roles.

import KelCircle.GlobalSequence
import KelCircle.Events
import KelCircle.Fold

namespace KelCircle

variable {α : Type}

-- Appending an event with the correct next index preserves contiguity
theorem append_preserves_contiguous
    (gs : List (SequencedEvent α))
    (e : SequencedEvent α)
    (hgs : indicesContiguous gs)
    (hidx : e.index = gs.length) :
    indicesContiguous (e :: gs) := by
  simp [indicesContiguous]
  exact ⟨hidx, hgs⟩

-- Appending with a greater timestamp preserves increasing timestamps
theorem append_preserves_increasing
    (e₂ : SequencedEvent α)
    (gs : List (SequencedEvent α))
    (hgs : timestampsIncreasing gs)
    (hgt : ∀ (e' : SequencedEvent α), e' ∈ gs →
      e₂.timestamp > e'.timestamp) :
    timestampsIncreasing (e₂ :: gs) := by
  cases gs with
  | nil => simp [timestampsIncreasing]
  | cons hd tl =>
    simp [timestampsIncreasing]
    constructor
    · apply hgt
      exact .head _
    · exact hgs

-- Appending with a unique index preserves no-duplicates
theorem append_preserves_no_dups
    (e : SequencedEvent α)
    (gs : List (SequencedEvent α))
    (hgs : noDuplicateIndices gs)
    (huniq : ∀ (e' : SequencedEvent α), e' ∈ gs → e.index ≠ e'.index) :
    noDuplicateIndices (e :: gs) := by
  simp [noDuplicateIndices]
  exact ⟨huniq, hgs⟩

-- The membership set is computed by folding decisions.
-- This is a base-layer concern: membership changes are decisions.

structure CircleState where
  members : List Member

-- Initial state: empty circle
def emptyCircle : CircleState := ⟨[]⟩

-- A member is in the circle
def isMember (s : CircleState) (m : MemberId) : Prop :=
  ∃ mem ∈ s.members, mem.id = m

-- A member is an admin
def isAdmin (s : CircleState) (m : MemberId) : Prop :=
  ∃ mem ∈ s.members, mem.id = m ∧ mem.role = Role.admin

-- Bootstrap mode: no admins exist
def isBootstrap (s : CircleState) : Prop :=
  ¬∃ m, isAdmin s m

-- Empty circle is in bootstrap mode
theorem empty_is_bootstrap : isBootstrap emptyCircle := by
  intro ⟨_, mem, hmem, _⟩
  simp [emptyCircle] at hmem

-- An admin is always a member
theorem admin_is_member (s : CircleState) (m : MemberId) :
    isAdmin s m → isMember s m := by
  intro ⟨mem, hmem, hid, _⟩
  exact ⟨mem, hmem, hid⟩

end KelCircle
