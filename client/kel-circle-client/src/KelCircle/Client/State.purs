-- | Circle state mirroring Haskell KelCircle.State.
module KelCircle.Client.State
  ( CircleState
  , Circle
  , emptyCircle
  , emptyCircleState
  , isMember
  , isAdminMember
  , adminCount
  , majority
  , AuthMode(..)
  , authMode
  , isBootstrap
  , applyBaseDecision
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (isJust)
import KelCircle.Client.Events (BaseDecision(..))
import KelCircle.Client.Types (Member, MemberId, Role(..))

-- | The circle's membership state.
-- | Mirrors Haskell @CircleState@.
type CircleState =
  { members :: Array Member
  }

-- | Full circle state: members + current sequencer.
-- | Mirrors Haskell @Circle@.
type Circle =
  { circleState :: CircleState
  , sequencerId :: MemberId
  }

-- | Empty membership state.
emptyCircleState :: CircleState
emptyCircleState = { members: [] }

-- | Empty circle with the given sequencer.
emptyCircle :: MemberId -> Circle
emptyCircle sid =
  { circleState: emptyCircleState
  , sequencerId: sid
  }

-- | Is the given member in the circle?
isMember :: CircleState -> MemberId -> Boolean
isMember cs mid =
  isJust (Array.find (\m -> m.memberId == mid) cs.members)

-- | Is the given member an admin?
isAdminMember :: CircleState -> MemberId -> Boolean
isAdminMember cs mid =
  isJust
    ( Array.find
        (\m -> m.memberId == mid && m.memberRole == Admin)
        cs.members
    )

-- | Count admins in the circle.
adminCount :: CircleState -> Int
adminCount cs =
  Array.length (Array.filter (\m -> m.memberRole == Admin) cs.members)

-- | Majority threshold: strictly more than half.
-- | Mirrors Haskell @majority@.
majority :: Int -> Int
majority n = n / 2 + 1

-- | Authentication mode.
data AuthMode
  = Bootstrap
  | Normal

derive instance eqAuthMode :: Eq AuthMode

-- | Determine the current authentication mode.
authMode :: CircleState -> AuthMode
authMode cs
  | adminCount cs == 0 = Bootstrap
  | otherwise = Normal

-- | Is the circle in bootstrap mode?
isBootstrap :: CircleState -> Boolean
isBootstrap cs = authMode cs == Bootstrap

-- | Apply a base decision to the circle.
-- | Mirrors Haskell @applyBaseDecision@.
applyBaseDecision :: Circle -> BaseDecision -> Circle
applyBaseDecision c = case _ of
  IntroduceMember mid name role ->
    c
      { circleState = c.circleState
          { members = Array.snoc c.circleState.members
              { memberId: mid, memberRole: role, memberName: name }
          }
      }
  RemoveMember mid ->
    c
      { circleState = c.circleState
          { members = Array.filter
              (\m -> m.memberId /= mid)
              c.circleState.members
          }
      }
  ChangeRole mid newRole ->
    c
      { circleState = c.circleState
          { members = map
              ( \m ->
                  if m.memberId == mid then
                    m { memberRole = newRole }
                  else m
              )
              c.circleState.members
          }
      }
  RotateSequencer newSid ->
    let oldSid = c.sequencerId
        renamedMembers = map
          (\m -> if m.memberId == oldSid then m { memberName = oldSid } else m)
          c.circleState.members
    in { circleState: c.circleState
            { members = Array.snoc renamedMembers
                { memberId: newSid, memberRole: MemberRole, memberName: "sequencer" }
            }
       , sequencerId: newSid
       }
