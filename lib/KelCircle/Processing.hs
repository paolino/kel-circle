{- |
Module      : KelCircle.Processing
Description : Event processing pipeline
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

The event processing pipeline: validates events through
the two-level gate and applies them to the full state.
Mirrors Lean @KelCircle.Processing@.
-}
module KelCircle.Processing
    ( -- * Full state
      FullState (..)
    , initFullState

      -- * Gate functions
    , gateBaseDecision
    , gateAppDecision
    , gateProposal
    , gateResponse
    , gateResolve

      -- * Apply functions
    , applyBase
    , applyAppDecision
    , applyProposal
    , applyResponse
    , applyResolve
    ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import KelCircle.Events
    ( BaseDecision (..)
    , Resolution
    )
import KelCircle.Gate (fullGate)
import KelCircle.MemberKel (KelKeyState)
import KelCircle.Proposals qualified as P
import KelCircle.State
    ( Circle (..)
    , applyBaseDecision
    , emptyCircle
    , isMember
    )
import KelCircle.Types
    ( MemberId
    , ProposalId
    , Role (..)
    , Timestamp
    )

{- | The complete state maintained by the sequencer.
Mirrors Lean @FullState@.

Type parameters:

* @g@ — application fold state
* @p@ — proposal content
* @r@ — response content
-}
data FullState g p r = FullState
    { fsCircle :: Circle
    -- ^ Base state + sequencer id
    , fsAppState :: g
    -- ^ Application fold state
    , fsProposals :: P.ProposalRegistry p r
    -- ^ Tracked proposals
    , fsNextSeq :: Int
    -- ^ Next sequence number
    , fsKeyStates :: Map MemberId KelKeyState
    -- ^ Cached key states (updated by Store)
    }
    deriving stock (Show, Eq)

{- | Initial state for a circle after genesis.
The sequencer introduces itself as event 0.
Mirrors Lean @initFullState@.
-}
initFullState
    :: MemberId -> g -> FullState g p r
initFullState sid initApp =
    FullState
        { fsCircle = genesis sid
        , fsAppState = initApp
        , fsProposals = []
        , fsNextSeq = 1
        , fsKeyStates = Map.empty
        }
  where
    genesis sid' =
        applyBaseDecision
            ( Circle
                { circleState = emptyCircle
                , sequencerId = sid'
                }
            )
            (IntroduceMember sid' "sequencer" Member)

-- ---------------------------------------------------------------
-- Gate functions
-- ---------------------------------------------------------------

{- | Gate for base decisions: uses the two-level gate.
Mirrors Lean @gateBaseDecision@.
-}
gateBaseDecision
    :: FullState g p r
    -> MemberId
    -> BaseDecision
    -> (g -> BaseDecision -> Bool)
    -> Bool
gateBaseDecision s signer d =
    fullGate
        (circleState (fsCircle s))
        signer
        (sequencerId (fsCircle s))
        d
        (fsAppState s)

{- | Gate for application decisions: signer must be a
member and pass the app gate.
Mirrors Lean @gateAppDecision@.
-}
gateAppDecision
    :: FullState g p r
    -> MemberId
    -> d
    -> (g -> d -> Bool)
    -> Bool
gateAppDecision s signer content appGate =
    isMember (circleState (fsCircle s)) signer
        && appGate (fsAppState s) content

{- | Gate for proposals: signer must be a member and
pass the app gate.
Mirrors Lean @gateProposal@.
-}
gateProposal
    :: FullState g p r
    -> MemberId
    -> p
    -> (g -> p -> Bool)
    -> Bool
gateProposal s signer content appGate =
    isMember (circleState (fsCircle s)) signer
        && appGate (fsAppState s) content

{- | Gate for responses: signer must be a member,
proposal must be open, signer must not have already
responded.
Mirrors Lean @gateResponse@.
-}
gateResponse
    :: FullState g p r
    -> MemberId
    -> ProposalId
    -> Bool
gateResponse s signer pid =
    isMember (circleState (fsCircle s)) signer
        && case P.findProposal (fsProposals s) pid of
            Just tp -> P.canRespond tp signer
            Nothing -> False

{- | Gate for resolve: only the sequencer can resolve.
Mirrors Lean @gateResolve@.
-}
gateResolve
    :: FullState g p r -> MemberId -> Bool
gateResolve s signer =
    signer == sequencerId (fsCircle s)

-- ---------------------------------------------------------------
-- Apply functions
-- ---------------------------------------------------------------

{- | Apply a base decision to the full state.
Mirrors Lean @applyBase@.
-}
applyBase
    :: FullState g p r
    -> BaseDecision
    -> FullState g p r
applyBase s d =
    s
        { fsCircle =
            applyBaseDecision (fsCircle s) d
        , fsNextSeq = fsNextSeq s + 1
        }

{- | Apply an application decision (fold update only).
Mirrors Lean @applyAppDecision@.
-}
applyAppDecision
    :: FullState g p r
    -> d
    -> (g -> d -> g)
    -> FullState g p r
applyAppDecision s content fApp =
    s
        { fsAppState = fApp (fsAppState s) content
        , fsNextSeq = fsNextSeq s + 1
        }

{- | Apply a proposal: register it as open.
Mirrors Lean @applyProposal@.
-}
applyProposal
    :: FullState g p r
    -> p
    -> MemberId
    -> Timestamp
    -> FullState g p r
applyProposal s content proposer deadline =
    s
        { fsProposals =
            P.openProposal
                (fsProposals s)
                (fsNextSeq s)
                content
                proposer
                deadline
        , fsNextSeq = fsNextSeq s + 1
        }

{- | Apply a response: add to proposal.
Mirrors Lean @applyResponse@.
-}
applyResponse
    :: FullState g p r
    -> r
    -> MemberId
    -> ProposalId
    -> FullState g p r
applyResponse s content responder pid =
    s
        { fsProposals =
            P.addResponse
                (fsProposals s)
                pid
                responder
                content
        , fsNextSeq = fsNextSeq s + 1
        }

{- | Apply a resolution: close the proposal.
Mirrors Lean @applyResolve@.
-}
applyResolve
    :: FullState g p r
    -> ProposalId
    -> Resolution
    -> FullState g p r
applyResolve s pid r =
    s
        { fsProposals =
            P.resolveProposal
                (fsProposals s)
                pid
                r
        , fsNextSeq = fsNextSeq s + 1
        }
