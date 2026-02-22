-- GlobalSequence: the monotonic counter and timestamp invariants
--
-- The sequencer maintains a global sequence of events. Each event gets
-- a unique index (0, 1, 2, ...) and a strictly increasing UTC timestamp.

namespace KelCircle

-- Abstract representation of a member identifier (KERI prefix)
abbrev MemberId := Nat

-- UTC timestamp (abstract, monotonically increasing)
abbrev Timestamp := Nat

-- A sequenced event: an interaction event that has been assigned
-- a position in the global sequence by the sequencer.
structure SequencedEvent (α : Type) where
  index     : Nat
  timestamp : Timestamp
  member    : MemberId
  payload   : α

-- The global sequence is a list of sequenced events, oldest first.
def GlobalSequence (α : Type) := List (SequencedEvent α)

variable {α : Type}

-- Predicate: indices form a contiguous range [0, n) with no gaps
-- Defined recursively: each event's index matches its position
def indicesContiguous : List (SequencedEvent α) → Prop
  | [] => True
  | e :: rest => e.index = rest.length ∧ indicesContiguous rest

-- Predicate: timestamps strictly increase (newest first in list)
def timestampsIncreasing : List (SequencedEvent α) → Prop
  | [] => True
  | [_] => True
  | e₁ :: e₂ :: rest =>
      e₁.timestamp > e₂.timestamp ∧ timestampsIncreasing (e₂ :: rest)

-- Predicate: no duplicate indices
def noDuplicateIndices : List (SequencedEvent α) → Prop
  | [] => True
  | e :: rest =>
      (∀ (e' : SequencedEvent α), e' ∈ rest → e.index ≠ e'.index) ∧
      noDuplicateIndices rest

-- A well-formed global sequence satisfies all three invariants
-- Note: events stored newest-first (head = most recent)
structure WellFormedSequence (α : Type) where
  events     : List (SequencedEvent α)
  contiguous : indicesContiguous events
  increasing : timestampsIncreasing events
  noDups     : noDuplicateIndices events

-- The next expected index for a sequence
def nextIndex (gs : List (SequencedEvent α)) : Nat := gs.length

-- Empty sequence is well-formed
theorem empty_contiguous : indicesContiguous ([] : List (SequencedEvent α)) :=
  trivial

theorem empty_increasing : timestampsIncreasing ([] : List (SequencedEvent α)) :=
  trivial

theorem empty_no_dups : noDuplicateIndices ([] : List (SequencedEvent α)) :=
  trivial

-- Singleton with index 0 is contiguous
theorem singleton_contiguous (e : SequencedEvent α) (h : e.index = 0) :
    indicesContiguous [e] := by
  simp [indicesContiguous]
  exact h

-- Singleton is trivially increasing
theorem singleton_increasing (e : SequencedEvent α) :
    timestampsIncreasing [e] := by
  simp [timestampsIncreasing]

-- Singleton has no duplicate indices
theorem singleton_no_dups (e : SequencedEvent α) :
    noDuplicateIndices [e] := by
  simp [noDuplicateIndices]

end KelCircle
