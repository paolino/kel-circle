module View.Proposals
  ( proposalsComponent
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import KelCircle.Client.Proposals
  ( TrackedProposal
  , ProposalRegistry
  , canRespond
  , isOpen
  )
import KelCircle.Client.State
  ( CircleState
  , isAdminMember
  )
import KelCircle.Client.Types (MemberId, ProposalId)

data Output = SubmitResponse ProposalId

type Input =
  { proposals :: ProposalRegistry Unit Unit
  , myKey :: Maybe MemberId
  , circleState :: CircleState
  }

type State =
  { proposals :: ProposalRegistry Unit Unit
  , myKey :: Maybe MemberId
  , circleState :: CircleState
  }

data Action
  = Receive Input
  | DoRespond ProposalId

proposalsComponent
  :: forall q m. MonadAff m => H.Component q Input Output m
proposalsComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

initialState :: Input -> State
initialState input =
  { proposals: input.proposals
  , myKey: input.myKey
  , circleState: input.circleState
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  let
    openProps = Array.filter (\tp -> isOpen tp.status) st.proposals
    resolvedProps = Array.filter (\tp -> not (isOpen tp.status)) st.proposals
  in
    HH.div [ HP.class_ (HH.ClassName "proposals") ]
      [ HH.h2_ [ HH.text "Proposals" ]
      , if openProps == [] then
          HH.p_ [ HH.text "No open proposals." ]
        else
          HH.div_ (map (proposalCard st.circleState st.myKey) openProps)
      , if resolvedProps /= [] then
          HH.div_
            [ HH.h3_ [ HH.text "Resolved" ]
            , HH.div_ (map resolvedCard resolvedProps)
            ]
        else HH.text ""
      ]

proposalCard
  :: forall m
   . CircleState
  -> Maybe MemberId
  -> TrackedProposal Unit Unit
  -> H.ComponentHTML Action () m
proposalCard cs myKey tp =
  let
    responseCount = Array.length tp.responses
    -- Only admins who haven't responded can respond
    canI = case myKey of
      Nothing -> false
      Just k ->
        isAdminMember cs k && canRespond tp k
  in
    HH.div [ HP.class_ (HH.ClassName "proposal-card") ]
      [ HH.h3_ [ HH.text ("Proposal #" <> show tp.proposalId) ]
      , HH.p_
          [ HH.text $
              "Proposed by: " <> truncateKey tp.proposer
          ]
      , HH.p_
          [ HH.text $
              "Responses: " <> show responseCount
          ]
      , HH.p_
          [ HH.text $
              "Deadline: " <> show tp.deadline
          ]
      , if canI then HH.button
          [ HE.onClick (const (DoRespond tp.proposalId))
          , HP.class_ (HH.ClassName "btn-primary")
          ]
          [ HH.text "Respond" ]
        else HH.text ""
      ]

resolvedCard
  :: forall m
   . TrackedProposal Unit Unit
  -> H.ComponentHTML Action () m
resolvedCard tp =
  HH.div [ HP.class_ (HH.ClassName "proposal-card resolved") ]
    [ HH.h3_ [ HH.text ("Proposal #" <> show tp.proposalId) ]
    , HH.p_
        [ HH.text $
            "Proposed by: " <> truncateKey tp.proposer
        ]
    , HH.p_
        [ HH.text $ "Status: " <> show tp.status ]
    , HH.p_
        [ HH.text $
            "Responses: " <> show (Array.length tp.responses)
        ]
    ]

handleAction
  :: forall m
   . MonadAff m
  => Action
  -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ _
      { proposals = input.proposals
      , myKey = input.myKey
      , circleState = input.circleState
      }
  DoRespond pid ->
    H.raise (SubmitResponse pid)

-- Helpers

truncateKey :: String -> String
truncateKey s =
  if String.length s > 12 then String.take 12 s <> "..."
  else s
