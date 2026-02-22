-- | Client-side fold mirroring Haskell KelCircle.Fold
-- | and KelCircle.Processing.
module KelCircle.Client.Fold
  ( FullState
  , initFullState
  , applyCircleEvent
  , foldCircle
  ) where

import Prelude

import Data.Foldable (foldl)
import Data.Tuple (Tuple(..))
import KelCircle.Client.Events
  ( BaseDecision(..)
  , CircleEvent(..)
  )
import KelCircle.Client.Proposals as P
import KelCircle.Client.State
  ( Circle
  , applyBaseDecision
  , emptyCircle
  )
import KelCircle.Client.Types (MemberId, Role(..))

-- | The complete state maintained by the fold.
-- | Mirrors Haskell @FullState g p r@.
type FullState g p r =
  { circle :: Circle
  , appState :: g
  , proposals :: P.ProposalRegistry p r
  , nextSeq :: Int
  }

-- | Initial state for a circle after genesis.
-- | Mirrors Haskell @initFullState@.
initFullState :: forall g p r. MemberId -> g -> FullState g p r
initFullState sid initApp =
  { circle: applyBaseDecision (emptyCircle sid) (IntroduceMember sid "sequencer" MemberRole)
  , appState: initApp
  , proposals: []
  , nextSeq: 1
  }

-- | Apply a single circle event to the full state.
-- | The signer is the member who submitted the event.
applyCircleEvent
  :: forall g d p r
   . (g -> d -> g)
  -> FullState g p r
  -> Tuple MemberId (CircleEvent d p r)
  -> FullState g p r
applyCircleEvent appFold st (Tuple signer evt) = case evt of
  CEBaseDecision bd ->
    st
      { circle = applyBaseDecision st.circle bd
      , nextSeq = st.nextSeq + 1
      }
  CEAppDecision d ->
    st
      { appState = appFold st.appState d
      , nextSeq = st.nextSeq + 1
      }
  CEProposal content deadline ->
    st
      { proposals = P.openProposal
          st.proposals
          st.nextSeq
          content
          signer
          deadline
      , nextSeq = st.nextSeq + 1
      }
  CEResponse content pid ->
    st
      { proposals = P.addResponse
          st.proposals
          pid
          signer
          content
      , nextSeq = st.nextSeq + 1
      }
  CEResolveProposal pid res ->
    st
      { proposals = P.resolveProposal
          st.proposals
          pid
          res
      , nextSeq = st.nextSeq + 1
      }

-- | Fold a sequence of signed circle events into state.
foldCircle
  :: forall g d p r
   . (g -> d -> g)
  -> FullState g p r
  -> Array (Tuple MemberId (CircleEvent d p r))
  -> FullState g p r
foldCircle appFold =
  foldl (applyCircleEvent appFold)
