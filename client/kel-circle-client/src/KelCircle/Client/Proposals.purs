-- | Proposal lifecycle mirroring Haskell KelCircle.Proposals.
module KelCircle.Client.Proposals
  ( ProposalStatus(..)
  , isOpen
  , isResolved
  , TrackedProposal
  , ProposalRegistry
  , findProposal
  , openProposal
  , addResponse
  , resolveProposal
  , hasNotResponded
  , canRespond
  , hasOpenProposalWithContent
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe)
import KelCircle.Client.Events (Resolution)
import KelCircle.Client.Types (MemberId, ProposalId, Timestamp)

-- | Proposal status in the lifecycle.
-- | Mirrors Haskell @ProposalStatus@.
data ProposalStatus
  = Open
  | Resolved Resolution

derive instance eqProposalStatus :: Eq ProposalStatus

instance showProposalStatus :: Show ProposalStatus where
  show Open = "Open"
  show (Resolved r) = "Resolved(" <> show r <> ")"

-- | Is the proposal open?
isOpen :: ProposalStatus -> Boolean
isOpen Open = true
isOpen _ = false

-- | Is the proposal resolved?
isResolved :: ProposalStatus -> Boolean
isResolved (Resolved _) = true
isResolved _ = false

-- | A tracked proposal: protocol-level bookkeeping.
-- | Mirrors Haskell @TrackedProposal p r@.
type TrackedProposal p r =
  { proposalId :: ProposalId
  , content :: p
  , proposer :: MemberId
  , deadline :: Timestamp
  , responses :: Array r
  , respondents :: Array MemberId
  , status :: ProposalStatus
  }

-- | The proposal registry.
type ProposalRegistry p r = Array (TrackedProposal p r)

-- | Lookup a proposal by id.
findProposal
  :: forall p r
   . ProposalRegistry p r
  -> ProposalId
  -> Maybe (TrackedProposal p r)
findProposal reg pid =
  Array.find (\tp -> tp.proposalId == pid) reg

-- | Open a new proposal in the registry.
openProposal
  :: forall p r
   . ProposalRegistry p r
  -> ProposalId
  -> p
  -> MemberId
  -> Timestamp
  -> ProposalRegistry p r
openProposal reg pid cont proposer deadline =
  Array.snoc reg
    { proposalId: pid
    , content: cont
    , proposer
    , deadline
    , responses: []
    , respondents: []
    , status: Open
    }

-- | Add a response to an open proposal.
addResponse
  :: forall p r
   . ProposalRegistry p r
  -> ProposalId
  -> MemberId
  -> r
  -> ProposalRegistry p r
addResponse reg pid responder response =
  map
    ( \tp ->
        if tp.proposalId == pid && isOpen tp.status then
          tp
            { responses = Array.snoc tp.responses response
            , respondents = Array.snoc tp.respondents responder
            }
        else tp
    )
    reg

-- | Resolve a proposal with a given resolution.
resolveProposal
  :: forall p r
   . ProposalRegistry p r
  -> ProposalId
  -> Resolution
  -> ProposalRegistry p r
resolveProposal reg pid r =
  map
    ( \tp ->
        if tp.proposalId == pid && isOpen tp.status then
          tp { status = Resolved r }
        else tp
    )
    reg

-- | A member can only respond once per proposal.
hasNotResponded
  :: forall p r
   . TrackedProposal p r
  -> MemberId
  -> Boolean
hasNotResponded tp mid =
  not (Array.elem mid tp.respondents)

-- | Only open proposals accept responses from
-- | members who haven't responded yet.
canRespond
  :: forall p r
   . TrackedProposal p r
  -> MemberId
  -> Boolean
canRespond tp mid =
  isOpen tp.status && hasNotResponded tp mid

-- | Check if there is already an open proposal
-- | with the same content.
hasOpenProposalWithContent
  :: forall p r
   . Eq p
  => ProposalRegistry p r
  -> p
  -> Boolean
hasOpenProposalWithContent reg content =
  Array.any
    (\tp -> isOpen tp.status && tp.content == content)
    reg
