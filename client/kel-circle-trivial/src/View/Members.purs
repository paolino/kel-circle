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
import KelCircle.Client.State
  ( CircleState
  , isAdminMember
  , isMember
  )
import KelCircle.Client.Types (Member, Role(..))

data Output
  = SubmitDecision BaseDecision
  | SubmitProposal BaseDecision
  | SubmitIntroduceWithInception
      { memberId :: String
      , name :: String
      , role :: Role
      , inceptionJson :: String
      }

type Input =
  { circleState :: CircleState
  , myKey :: Maybe String
  , submitting :: Boolean
  }

type State =
  { circleState :: CircleState
  , myKey :: Maybe String
  , submitting :: Boolean
  , newMemberKey :: String
  , newMemberName :: String
  , newMemberAdmin :: Boolean
  , newMemberInception :: String
  }

data Action
  = Receive Input
  | SetNewMemberKey String
  | SetNewMemberName String
  | SetNewMemberInception String
  | ToggleNewMemberAdmin
  | DoIntroduce
  | DoRemove String
  | DoPropose String Role

membersComponent
  :: forall q m
   . MonadAff m
  => H.Component q Input Output m
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
  , submitting: input.submitting
  , newMemberKey: ""
  , newMemberName: ""
  , newMemberAdmin: false
  , newMemberInception: ""
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
        else
          HH.p
            [ HP.class_
                (HH.ClassName "not-member-hint")
            ]
            [ HH.text
                "You are not a member yet. Share \
                \your key with an admin to be \
                \introduced."
            ]
      ]

memberTable
  :: forall m. State -> H.ComponentHTML Action () m
memberTable st =
  let
    entries = st.circleState.members
    amIAdmin = case st.myKey of
      Nothing -> false
      Just k -> isAdminMember st.circleState k
    myId = case st.myKey of
      Nothing -> ""
      Just k -> k
  in
    HH.table
      [ HP.class_ (HH.ClassName "member-table") ]
      [ HH.thead_
          [ HH.tr_
              [ HH.th_ [ HH.text "Name" ]
              , HH.th_ [ HH.text "Role" ]
              , if amIAdmin then
                  HH.th_ [ HH.text "Actions" ]
                else HH.text ""
              ]
          ]
      , HH.tbody_
          ( map
              (memberRow amIAdmin st.submitting myId)
              entries
          )
      ]

memberRow
  :: forall m
   . Boolean
  -> Boolean
  -> String
  -> Member
  -> H.ComponentHTML Action () m
memberRow amIAdmin submitting myId member =
  let
    isSequencer = member.memberName == "sequencer"
    isMe = member.memberId == myId
    proposeBtn = case member.memberRole of
      MemberRole ->
        HH.button
          [ HE.onClick
              (const (DoPropose member.memberId Admin))
          , HP.class_
              (HH.ClassName "btn-propose")
          , HP.disabled submitting
          ]
          [ HH.text "Propose Admin" ]
      Admin ->
        HH.button
          [ HE.onClick
              ( const
                  ( DoPropose member.memberId
                      MemberRole
                  )
              )
          , HP.class_
              (HH.ClassName "btn-propose")
          , HP.disabled submitting
          ]
          [ HH.text "Propose Demote" ]
  in
    HH.tr
      ( if isMe then
          [ HP.class_ (HH.ClassName "me") ]
        else []
      )
      [ HH.td
          [ HP.class_ (HH.ClassName "name") ]
          [ HH.text
              ( member.memberName
                  <> if isMe then " (you)" else ""
              )
          ]
      , HH.td_ [ HH.text (show member.memberRole) ]
      , if amIAdmin && not isSequencer && not isMe then HH.td_
          [ proposeBtn
          , HH.button
              [ HE.onClick
                  ( const
                      (DoRemove member.memberId)
                  )
              , HP.class_
                  (HH.ClassName "btn-danger")
              , HP.disabled submitting
              ]
              [ HH.text "Remove" ]
          ]
        else HH.text ""
      ]

introduceForm
  :: forall m. State -> H.ComponentHTML Action () m
introduceForm st =
  HH.div
    [ HP.class_
        (HH.ClassName "form introduce-form")
    ]
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
    , HH.label_ [ HH.text "Inception JSON" ]
    , HH.textarea
        [ HP.placeholder
            "Paste member's inception JSON here"
        , HP.value st.newMemberInception
        , HP.rows 4
        , HE.onValueInput SetNewMemberInception
        ]
    , HH.label_
        [ HH.input
            [ HP.type_ HP.InputCheckbox
            , HP.checked st.newMemberAdmin
            , HE.onChecked
                (const ToggleNewMemberAdmin)
            ]
        , HH.text " Admin"
        ]
    , HH.button
        [ HE.onClick (const DoIntroduce)
        , HP.class_ (HH.ClassName "btn-primary")
        , HP.disabled st.submitting
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
      , submitting = input.submitting
      }

  SetNewMemberKey s ->
    H.modify_ _ { newMemberKey = s }

  SetNewMemberName s ->
    H.modify_ _ { newMemberName = s }

  SetNewMemberInception s ->
    H.modify_ _ { newMemberInception = s }

  ToggleNewMemberAdmin ->
    H.modify_ \s ->
      s { newMemberAdmin = not s.newMemberAdmin }

  DoIntroduce -> do
    st <- H.get
    when (st.newMemberKey /= "") do
      let
        role =
          if st.newMemberAdmin then Admin
          else MemberRole
        name =
          if st.newMemberName == "" then
            st.newMemberKey
          else st.newMemberName
      if st.newMemberInception /= "" then
        H.raise
          ( SubmitIntroduceWithInception
              { memberId: st.newMemberKey
              , name
              , role
              , inceptionJson:
                  st.newMemberInception
              }
          )
      else
        H.raise
          ( SubmitDecision
              ( IntroduceMember
                  st.newMemberKey
                  name
                  role
              )
          )
      H.modify_ _
        { newMemberKey = ""
        , newMemberName = ""
        , newMemberAdmin = false
        , newMemberInception = ""
        }

  DoRemove key ->
    H.raise (SubmitDecision (RemoveMember key))

  DoPropose memberId role ->
    H.raise (SubmitProposal (ChangeRole memberId role))
