{- |
Module      : KelCircle.Types
Description : Core types for the kel-circle protocol
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Base types mirroring the Lean formalization in
@KelCircle.GlobalSequence@ and @KelCircle.Events@:
member identifiers, timestamps, roles, and members.
-}
module KelCircle.Types
    ( -- * Identifiers
      MemberId (..)
    , ProposalId
    , Timestamp

      -- * Roles and members
    , Role (..)
    , Member (..)
    ) where

import Data.Text (Text)

{- | A member identifier (KERI prefix).
Mirrors Lean @MemberId@.
-}
newtype MemberId = MemberId {unMemberId :: Text}
    deriving stock (Show)
    deriving newtype (Eq, Ord)

{- | A proposal identifier: the sequence index where
the proposal was opened.
Mirrors Lean @ProposalId@.
-}
type ProposalId = Int

{- | UTC timestamp (milliseconds since epoch).
Mirrors Lean @Timestamp@ (abstract, monotonically
increasing).
-}
type Timestamp = Int

{- | Role in the circle.
Mirrors Lean @Role@.
-}
data Role
    = -- | Can manage membership and approve proposals
      Admin
    | -- | Regular circle participant
      Member
    deriving stock (Eq, Ord, Show)

{- | A circle member with their identifier and role.
Mirrors Lean @Member@.
-}
data Member = MemberRecord
    { memberId :: MemberId
    -- ^ KERI prefix
    , memberRole :: Role
    -- ^ Current role
    }
    deriving stock (Eq, Show)
