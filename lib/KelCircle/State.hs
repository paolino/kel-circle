{- |
Module      : KelCircle.State
Description : Circle state and membership queries
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Circle state derived by folding the global sequence.
Membership and admin queries, bootstrap mode detection.
Mirrors Lean @KelCircle.Invariants@ and
@KelCircle.BaseDecisions@.
-}
module KelCircle.State
    ( -- * Circle state
      CircleState (..)
    , Circle (..)
    , emptyCircle

      -- * Membership queries
    , isMember
    , isAdmin
    , adminCount
    , majority

      -- * Bootstrap mode
    , AuthMode (..)
    , authMode
    , isBootstrap

      -- * State transitions
    , applyBaseDecision
    ) where

import KelCircle.Events (BaseDecision (..))
import KelCircle.Types
    ( Member (..)
    , MemberId
    , Role (..)
    )

{- | The circle's membership state.
Mirrors Lean @CircleState@.
-}
newtype CircleState = CircleState
    { members :: [Member]
    }
    deriving stock (Show, Eq)

{- | Full circle state: members + current sequencer.
Mirrors Lean @Circle@.
-}
data Circle = Circle
    { circleState :: CircleState
    -- ^ Current membership
    , sequencerId :: MemberId
    -- ^ Identity of the sequencer
    }
    deriving stock (Show, Eq)

{- | Empty circle with no members.
Mirrors Lean @emptyCircle@.
-}
emptyCircle :: CircleState
emptyCircle = CircleState []

{- | Is the given member in the circle?
Mirrors Lean @isMemberB@.
-}
isMember :: CircleState -> MemberId -> Bool
isMember s m = any (\mem -> memberId mem == m) (members s)

{- | Is the given member an admin?
Mirrors Lean @isAdminB@.
-}
isAdmin :: CircleState -> MemberId -> Bool
isAdmin s m =
    any
        ( \mem ->
            memberId mem == m
                && memberRole mem == Admin
        )
        (members s)

{- | Count the admins in the circle.
Mirrors Lean @adminCount@.
-}
adminCount :: CircleState -> Int
adminCount s =
    length $ filter (\m -> memberRole m == Admin) (members s)

{- | Majority threshold: strictly more than half.
Mirrors Lean @majority@.
-}
majority :: Int -> Int
majority n = n `div` 2 + 1

-- | Authentication mode.
data AuthMode
    = -- | Zero admins: passphrase-based auth
      Bootstrap
    | -- | Normal operation with admin gating
      Normal
    deriving stock (Eq, Show)

{- | Determine the current authentication mode.
Mirrors Lean @isBootstrapB@.
-}
authMode :: CircleState -> AuthMode
authMode s
    | adminCount s == 0 = Bootstrap
    | otherwise = Normal

-- | Is the circle in bootstrap mode?
isBootstrap :: CircleState -> Bool
isBootstrap s = authMode s == Bootstrap

{- | Apply a base decision to the circle.
Mirrors Lean @applyBaseDecision@.
-}
applyBaseDecision :: Circle -> BaseDecision -> Circle
applyBaseDecision c = \case
    IntroduceMember mid role ->
        c
            { circleState =
                (circleState c)
                    { members =
                        MemberRecord mid role
                            : members (circleState c)
                    }
            }
    RemoveMember mid ->
        c
            { circleState =
                (circleState c)
                    { members =
                        filter
                            (\m -> memberId m /= mid)
                            (members (circleState c))
                    }
            }
    ChangeRole mid newRole ->
        c
            { circleState =
                (circleState c)
                    { members =
                        map
                            ( \m ->
                                if memberId m == mid
                                    then
                                        MemberRecord
                                            mid
                                            newRole
                                    else m
                            )
                            (members (circleState c))
                    }
            }
    RotateSequencer newSid ->
        Circle
            { circleState =
                (circleState c)
                    { members =
                        MemberRecord newSid Member
                            : members (circleState c)
                    }
            , sequencerId = newSid
            }
