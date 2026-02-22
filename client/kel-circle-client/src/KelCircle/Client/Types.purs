-- | Core types mirroring Haskell KelCircle.Types.
module KelCircle.Client.Types
  ( MemberId
  , ProposalId
  , Timestamp
  , Role(..)
  , Member
  , isAdmin
  ) where

import Prelude

-- | Member identifier (KERI prefix string).
type MemberId = String

-- | Proposal identifier: sequence index where opened.
type ProposalId = Int

-- | UTC timestamp (milliseconds since epoch).
type Timestamp = Int

-- | Role in the circle. Mirrors Haskell @Role@.
data Role
  = Admin
  | MemberRole

derive instance eqRole :: Eq Role
derive instance ordRole :: Ord Role

instance showRole :: Show Role where
  show Admin = "Admin"
  show MemberRole = "Member"

-- | Is this an admin role?
isAdmin :: Role -> Boolean
isAdmin Admin = true
isAdmin _ = false

-- | A circle member. Mirrors Haskell @Member@.
type Member =
  { memberId :: MemberId
  , memberRole :: Role
  , memberName :: String
  }
