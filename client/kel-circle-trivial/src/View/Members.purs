module View.Members
  ( membersComponent
  , Output(..)
  , Input
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import KelCircle.Client.Events (BaseDecision(..))
import KelCircle.Client.State (CircleState, isAdminMember, isMember)
import KelCircle.Client.Types (Member, Role(..))

data Output = SubmitDecision BaseDecision

type Input =
  { circleState :: CircleState
  , myKey :: Maybe String
  }

type State =
  { circleState :: CircleState
  , myKey :: Maybe String
  , newMemberKey :: String
  , newMemberName :: String
  , newMemberAdmin :: Boolean
  }

data Action
  = Receive Input
  | SetNewMemberKey String
  | SetNewMemberName String
  | ToggleNewMemberAdmin
  | DoIntroduce
  | DoRemove String

membersComponent
  :: forall q m. MonadAff m => H.Component q Input Output m
membersComponent = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  }

initialState :: Input -> State
initialState input =
  { circleState: input.circleState
  , myKey: input.myKey
  , newMemberKey: ""
  , newMemberName: ""
  , newMemberAdmin: false
  }

render :: forall m. State -> H.ComponentHTML Action () m
render st =
  let
    amIMember = case st.myKey of
      Nothing -> false
      Just k -> isMember st.circleState k
  in
    HH.div [ HP.class_ (HH.ClassName "members") ]
      [ HH.h2_ [ HH.text "Members" ]
      , memberTable st
      , if amIMember then introduceForm st
        else HH.p [ HP.class_ (HH.ClassName "not-member-hint") ]
          [ HH.text
              "You are not a member yet. Share your key with \
              \an admin to be introduced."
          ]
      ]

memberTable :: forall m. State -> H.ComponentHTML Action () m
memberTable st =
  let
    entries = st.circleState.members
    amIAdmin = case st.myKey of
      Nothing -> false
      Just k -> isAdminMember st.circleState k
  in
    HH.table [ HP.class_ (HH.ClassName "member-table") ]
      [ HH.thead_
          [ HH.tr_
              [ HH.th_ [ HH.text "Name" ]
              , HH.th_ [ HH.text "Role" ]
              , if amIAdmin then HH.th_ [ HH.text "Actions" ]
                else HH.text ""
              ]
          ]
      , HH.tbody_ (map (memberRow amIAdmin) entries)
      ]

memberRow :: forall m. Boolean -> Member -> H.ComponentHTML Action () m
memberRow amIAdmin member =
  HH.tr_
    [ HH.td [ HP.class_ (HH.ClassName "name") ]
        [ HH.text member.memberName ]
    , HH.td_ [ HH.text (show member.memberRole) ]
    , if amIAdmin && member.memberName /= "sequencer" then HH.td_
        [ HH.button
            [ HE.onClick (const (DoRemove member.memberId))
            , HP.class_ (HH.ClassName "btn-danger")
            ]
            [ HH.text "Remove" ]
        ]
      else HH.text ""
    ]

introduceForm :: forall m. State -> H.ComponentHTML Action () m
introduceForm st =
  HH.div [ HP.class_ (HH.ClassName "form introduce-form") ]
    [ HH.h3_ [ HH.text "Introduce Member" ]
    , HH.input
        [ HP.placeholder "CESR public key"
        , HP.value st.newMemberKey
        , HE.onValueInput SetNewMemberKey
        ]
    , HH.input
        [ HP.placeholder "Display name"
        , HP.value st.newMemberName
        , HE.onValueInput SetNewMemberName
        ]
    , HH.label_
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked st.newMemberAdmin
            , HE.onChecked (const ToggleNewMemberAdmin)
            ]
        , HH.text " Admin"
        ]
    , HH.button
        [ HE.onClick (const DoIntroduce)
        , HP.class_ (HH.ClassName "btn-primary")
        ]
        [ HH.text "Introduce" ]
    ]

handleAction
  :: forall m
   . MonadAff m
  => Action
  -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Receive input ->
    H.modify_ _
      { circleState = input.circleState
      , myKey = input.myKey
      }

  SetNewMemberKey s ->
    H.modify_ _ { newMemberKey = s }

  SetNewMemberName s ->
    H.modify_ _ { newMemberName = s }

  ToggleNewMemberAdmin ->
    H.modify_ \s -> s { newMemberAdmin = not s.newMemberAdmin }

  DoIntroduce -> do
    st <- H.get
    when (st.newMemberKey /= "") do
      let
        role = if st.newMemberAdmin then Admin else MemberRole
      H.raise (SubmitDecision (IntroduceMember st.newMemberKey st.newMemberName role))
      H.modify_ _ { newMemberKey = "", newMemberName = "", newMemberAdmin = false }

  DoRemove key ->
    H.raise (SubmitDecision (RemoveMember key))
