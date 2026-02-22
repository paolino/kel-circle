-- Invariants: preservation theorems and membership model
--
-- Structural invariants for the global sequence, membership
-- as a base-layer concern, bootstrap mode, and admin roles.
-- The fold is two-layered: base fold (membership/roles) and
-- application fold (domain-specific, pluggable).

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

-- The membership set is computed by folding all events.
-- This is a base-layer concern: membership changes are tracked
-- by the base fold across all event types.

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

-- Two-layer fold: the full state is (base state, app state).
-- The base fold extracts membership/role changes from any event.
-- The application fold accumulates domain state from any event.
-- Both layers see every sequenced event.

structure TwoLayerState (β γ : Type) where
  base : β
  app  : γ

-- Compose two fold functions into a two-layer fold
def twoLayerFold {β γ α : Type}
    (fBase : β → α → β) (fApp : γ → α → γ)
    (init : TwoLayerState β γ)
    (gs : List (SequencedEvent α)) : TwoLayerState β γ :=
  foldAll (fun s a => ⟨fBase s.base a, fApp s.app a⟩) init gs

-- Two-layer fold on empty sequence returns initial state
theorem two_layer_fold_empty {β γ α : Type}
    (fBase : β → α → β) (fApp : γ → α → γ)
    (init : TwoLayerState β γ) :
    twoLayerFold fBase fApp init
      ([] : List (SequencedEvent α)) = init := by
  simp [twoLayerFold, foldAll]

-- Two-layer fold append decomposes into both layers
theorem two_layer_fold_append {β γ α : Type}
    (fBase : β → α → β) (fApp : γ → α → γ)
    (init : TwoLayerState β γ)
    (gs : List (SequencedEvent α))
    (e : SequencedEvent α) :
    twoLayerFold fBase fApp init (gs ++ [e]) =
      let prev := twoLayerFold fBase fApp init gs
      ⟨fBase prev.base e.payload, fApp prev.app e.payload⟩ := by
  simp [twoLayerFold, foldAll, List.foldl_append]

end KelCircle
