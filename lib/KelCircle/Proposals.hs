{- |
Module      : KelCircle.Proposals
Description : Proposal lifecycle and tracking
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Proposal lifecycle: open, respond, resolve. Tracks
proposal status, responses, and respondents. Mirrors
Lean @KelCircle.Proposals@.
-}
module KelCircle.Proposals
    ( -- * Proposal status
      ProposalStatus (..)
    , isOpen
    , isResolved

      -- * Tracked proposals
    , TrackedProposal (..)
    , ProposalRegistry

      -- * Operations
    , findProposal
    , openProposal
    , addResponse
    , resolveProposal

      -- * Response validation
    , hasNotResponded
    , canRespond

      -- * Duplicate detection
    , hasOpenProposalWithContent
    ) where

import KelCircle.Events (Resolution)
import KelCircle.Types
    ( MemberId
    , ProposalId
    , Timestamp
    )

{- | Proposal status in the lifecycle.
Mirrors Lean @ProposalStatus@.
-}
data ProposalStatus
    = -- | Accepting responses
      Open
    | -- | Closed with a resolution
      Resolved Resolution
    deriving stock (Show, Eq)

-- | Is the proposal open?
isOpen :: ProposalStatus -> Bool
isOpen Open = True
isOpen _ = False

-- | Is the proposal resolved?
isResolved :: ProposalStatus -> Bool
isResolved (Resolved _) = True
isResolved _ = False

{- | A tracked proposal: protocol-level bookkeeping.
Content types are abstract (application-defined).
Mirrors Lean @TrackedProposal@.

Type parameters:

* @p@ — proposal content
* @r@ — response content
-}
data TrackedProposal p r = TrackedProposal
    { tpProposalId :: ProposalId
    -- ^ Sequence index where opened
    , tpContent :: p
    -- ^ Application-defined proposal content
    , tpProposer :: MemberId
    -- ^ Who proposed
    , tpDeadline :: Timestamp
    -- ^ Mandatory timeout deadline
    , tpResponses :: [r]
    -- ^ Accumulated responses
    , tpRespondents :: [MemberId]
    -- ^ Who has responded
    , tpStatus :: ProposalStatus
    -- ^ Current lifecycle status
    }
    deriving stock (Show, Eq)

{- | The proposal registry: all tracked proposals.
Mirrors Lean @ProposalRegistry@.
-}
type ProposalRegistry p r = [TrackedProposal p r]

{- | Lookup a proposal by id.
Mirrors Lean @findProposal@.
-}
findProposal
    :: ProposalRegistry p r
    -> ProposalId
    -> Maybe (TrackedProposal p r)
findProposal reg pid =
    case filter
        (\tp -> tpProposalId tp == pid)
        reg of
        (x : _) -> Just x
        [] -> Nothing

{- | Open a new proposal in the registry.
Mirrors Lean @openProposal@.
-}
openProposal
    :: ProposalRegistry p r
    -> ProposalId
    -> p
    -> MemberId
    -> Timestamp
    -> ProposalRegistry p r
openProposal reg pid content proposer deadline =
    TrackedProposal
        { tpProposalId = pid
        , tpContent = content
        , tpProposer = proposer
        , tpDeadline = deadline
        , tpResponses = []
        , tpRespondents = []
        , tpStatus = Open
        }
        : reg

{- | Add a response to an open proposal.
Mirrors Lean @addResponse@.
-}
addResponse
    :: ProposalRegistry p r
    -> ProposalId
    -> MemberId
    -> r
    -> ProposalRegistry p r
addResponse reg pid responder response =
    map
        ( \tp ->
            if tpProposalId tp == pid
                && isOpen (tpStatus tp)
                then
                    tp
                        { tpResponses =
                            response : tpResponses tp
                        , tpRespondents =
                            responder
                                : tpRespondents tp
                        }
                else tp
        )
        reg

{- | Resolve a proposal with a given resolution.
Mirrors Lean @resolveProposal@.
-}
resolveProposal
    :: ProposalRegistry p r
    -> ProposalId
    -> Resolution
    -> ProposalRegistry p r
resolveProposal reg pid r =
    map
        ( \tp ->
            if tpProposalId tp == pid
                && isOpen (tpStatus tp)
                then tp{tpStatus = Resolved r}
                else tp
        )
        reg

{- | A member can only respond once per proposal.
Mirrors Lean @hasNotResponded@.
-}
hasNotResponded
    :: TrackedProposal p r -> MemberId -> Bool
hasNotResponded tp m =
    m `notElem` tpRespondents tp

{- | Only open proposals accept responses, and only
from members who haven't responded yet.
Mirrors Lean @canRespond@.
-}
canRespond
    :: TrackedProposal p r -> MemberId -> Bool
canRespond tp m =
    isOpen (tpStatus tp) && hasNotResponded tp m

{- | Check if there is already an open proposal with
the same content. Mirrors Lean
@hasOpenProposalWithContent@.
-}
hasOpenProposalWithContent
    :: (Eq p) => ProposalRegistry p r -> p -> Bool
hasOpenProposalWithContent reg content =
    any
        ( \tp ->
            isOpen (tpStatus tp)
                && tpContent tp == content
        )
        reg
