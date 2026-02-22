{- |
Module      : KelCircle
Description : Synchronized multi-KEL circle protocol
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Top-level module for the kel-circle library. Re-exports
all public API modules.
-}
module KelCircle
    ( -- * Core types
      module KelCircle.Types

      -- * Global sequence
    , module KelCircle.Sequence

      -- * Event classes and decisions
    , module KelCircle.Events

      -- * State fold
    , module KelCircle.Fold

      -- * Circle state and membership
    , module KelCircle.State

      -- * Two-level gate
    , module KelCircle.Gate

      -- * Proposal lifecycle
    , module KelCircle.Proposals

      -- * Event processing pipeline
    , module KelCircle.Processing
    ) where

import KelCircle.Events
import KelCircle.Fold
import KelCircle.Gate
import KelCircle.Processing
import KelCircle.Proposals
import KelCircle.Sequence
import KelCircle.State
import KelCircle.Types
