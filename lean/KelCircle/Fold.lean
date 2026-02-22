-- Fold: computing circle state from the global sequence
--
-- The fold processes only decisions. Proposals and responses are
-- sequenced for auditability but do not change the fold state.

import KelCircle.GlobalSequence
import KelCircle.Events

namespace KelCircle

variable {δ π ρ σ : Type}

-- Boolean version for filtering
def EventClass.isDecisionB : EventClass δ π ρ → Bool
  | .decision _ => true
  | .proposal _ _ _ => false
  | .response _ _ _ => false

-- Extract decision content if present
def EventClass.decisionContent : EventClass δ π ρ → Option δ
  | .decision d => some d
  | .proposal _ _ _ => none
  | .response _ _ _ => none

-- Apply a fold function only to decisions, skipping non-decisions.
-- Input list is oldest-first (natural sequence order).
def foldDecisions (f : σ → δ → σ) (init : σ)
    (gs : List (SequencedEvent (EventClass δ π ρ))) : σ :=
  gs.foldl (fun acc e =>
    match e.payload with
    | .decision d => f acc d
    | .proposal _ _ _ => acc
    | .response _ _ _ => acc
  ) init

-- Empty sequence produces the initial state
theorem fold_empty (f : σ → δ → σ) (init : σ) :
    foldDecisions f init ([] : List (SequencedEvent (EventClass δ π ρ))) = init := by
  simp [foldDecisions]

-- Adding a proposal does not change the fold
theorem fold_proposal (f : σ → δ → σ) (init : σ)
    (gs : List (SequencedEvent (EventClass δ π ρ)))
    (e : SequencedEvent (EventClass δ π ρ))
    (p : π) (t : Timestamp) (m : MemberId)
    (h : e.payload = EventClass.proposal p t m) :
    foldDecisions f init (gs ++ [e]) = foldDecisions f init gs := by
  simp [foldDecisions, List.foldl_append]
  rw [h]

-- Adding a response does not change the fold
theorem fold_response (f : σ → δ → σ) (init : σ)
    (gs : List (SequencedEvent (EventClass δ π ρ)))
    (e : SequencedEvent (EventClass δ π ρ))
    (r : ρ) (pid : ProposalId) (rid : MemberId)
    (h : e.payload = EventClass.response r pid rid) :
    foldDecisions f init (gs ++ [e]) = foldDecisions f init gs := by
  simp [foldDecisions, List.foldl_append]
  rw [h]

-- Adding a decision applies the fold function
theorem fold_decision (f : σ → δ → σ) (init : σ)
    (gs : List (SequencedEvent (EventClass δ π ρ)))
    (e : SequencedEvent (EventClass δ π ρ))
    (d : δ) (h : e.payload = EventClass.decision d) :
    foldDecisions f init (gs ++ [e]) = f (foldDecisions f init gs) d := by
  simp [foldDecisions, List.foldl_append]
  rw [h]

end KelCircle
