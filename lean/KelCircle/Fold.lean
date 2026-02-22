-- Fold: computing state from the global sequence
--
-- Every event in the sequence contributes to the fold.
-- The fold function is generic: σ → α → σ where α is the
-- event payload type.

import KelCircle.GlobalSequence

namespace KelCircle

variable {α σ : Type}

-- Fold all events in the global sequence, oldest-first.
-- Every event contributes to the resulting state.
def foldAll (f : σ → α → σ) (init : σ)
    (gs : List (SequencedEvent α)) : σ :=
  gs.foldl (fun acc e => f acc e.payload) init

-- Empty sequence produces the initial state
theorem fold_empty (f : σ → α → σ) (init : σ) :
    foldAll f init ([] : List (SequencedEvent α)) = init := by
  simp [foldAll]

-- Appending an event applies the fold function
theorem fold_append (f : σ → α → σ) (init : σ)
    (gs : List (SequencedEvent α))
    (e : SequencedEvent α) :
    foldAll f init (gs ++ [e]) = f (foldAll f init gs) e.payload := by
  simp [foldAll, List.foldl_append]

end KelCircle
