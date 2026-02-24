module View.App
  ( appComponent
  ) where

import Prelude

import Data.Argonaut.Core (Json, jsonNull, stringify)
import Data.Argonaut.Decode
  ( decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Const (Const)
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff (try)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import FFI.Fetch as Fetch
import FFI.SSE as SSE
import FFI.Storage as Storage
import Foreign.Object as FO
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import KelCircle.Client.Codec
  ( decodeCircleEvent
  , encodeCircleEvent
  , encodeSubmission
  , decodeInfoResponse
  )
import KelCircle.Client.Events
  ( BaseDecision(..)
  , CircleEvent(..)
  )
import KelCircle.Client.Fold
  ( FullState
  , applyCircleEvent
  , initFullState
  )
import KelCircle.Client.Identity as Identity
import KelCircle.Client.KelValidate as KV
import KelCircle.Client.State (isBootstrap)
import KelCircle.Client.Types (MemberId, Role(..))
import Type.Proxy (Proxy(..))
import View.Bootstrap as Bootstrap
import View.Members as Members
import View.Proposals as Proposals

type InfoResponse =
  { adminEmails :: Array String
  , pendingIntroduction :: Boolean
  }

data Screen
  = IdentityScreen
  | UnlockScreen
  | BootstrapScreen
  | NonMemberScreen InfoResponse
  | NormalScreen

-- | Trivial app: no application events.
type AppState = FullState Unit Unit Unit

type State =
  { screen :: Screen
  , identity :: Maybe Identity.Identity
  , inception :: Maybe Identity.InceptionData
  , fullState :: AppState
  , serverSeqNo :: Int
  , sse :: Maybe SSE.EventSource
  , error :: Maybe String
  , passphraseInput :: String
  , submitting :: Boolean
  }

data Action
  = Init
  | SetPassphrase String
  | GenerateIdentity
  | UnlockIdentity
  | HandleBootstrap Bootstrap.Output
  | HandleMembers Members.Output
  | HandleProposals Proposals.Output
  | SSENewMessage String
  | SSEKelMessage String
  | CopyKey
  | CopyInception
  | ResetIdentity
  | Dismiss

type Slots =
  ( bootstrap
      :: H.Slot (Const Void) Bootstrap.Output Unit
  , members
      :: H.Slot (Const Void) Members.Output Unit
  , proposals
      :: H.Slot (Const Void) Proposals.Output Unit
  )

defaultSequencer :: MemberId
defaultSequencer = "server"

appComponent
  :: forall q i o m
   . MonadAff m
  => H.Component q i o m
appComponent = H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
      { initialize = Just Init
      , handleAction = handleAction
      }
  }

initialState :: State
initialState =
  { screen: IdentityScreen
  , identity: Nothing
  , inception: Nothing
  , fullState: initFullState defaultSequencer unit
  , serverSeqNo: 0
  , sse: Nothing
  , error: Nothing
  , passphraseInput: ""
  , submitting: false
  }

-- --------------------------------------------------------
-- Render
-- --------------------------------------------------------

render
  :: forall m
   . MonadAff m
  => State
  -> H.ComponentHTML Action Slots m
render st = HH.div
  [ HP.class_ (HH.ClassName "app") ]
  [ header st
  , case st.error of
      Just err ->
        HH.div
          [ HP.class_ (HH.ClassName "error-bar")
          , HE.onClick (const Dismiss)
          ]
          [ HH.text err ]
      Nothing -> HH.text ""
  , content st
  ]

header
  :: forall m. State -> H.ComponentHTML Action Slots m
header st = HH.nav
  [ HP.class_ (HH.ClassName "header") ]
  [ HH.h1_ [ HH.text "kel-circle" ]
  , case st.identity of
      Just ident -> HH.span
        [ HP.class_ (HH.ClassName "user-id") ]
        [ HH.text (take8 ident.prefix)
        , HH.button
            [ HE.onClick (const CopyKey)
            , HP.class_ (HH.ClassName "btn-copy")
            ]
            [ HH.text "Copy" ]
        , HH.button
            [ HE.onClick (const ResetIdentity)
            , HP.class_ (HH.ClassName "btn-reset")
            ]
            [ HH.text "Reset" ]
        ]
      Nothing -> HH.text ""
  ]
  where
  take8 s =
    if String.length s > 8 then
      String.take 8 s <> "..."
    else s

content
  :: forall m
   . MonadAff m
  => State
  -> H.ComponentHTML Action Slots m
content st = case st.screen of
  IdentityScreen ->
    HH.div
      [ HP.class_ (HH.ClassName "identity-screen") ]
      [ HH.h2_ [ HH.text "Welcome to kel-circle" ]
      , HH.p_
          [ HH.text
              "Generate a KERI identity to get \
              \started. Choose a passphrase to \
              \protect your secret key."
          ]
      , passphraseField st
      , HH.button
          [ HE.onClick (const GenerateIdentity)
          , HP.class_ (HH.ClassName "btn-primary")
          , HP.disabled (st.passphraseInput == "")
          ]
          [ HH.text "Generate Identity" ]
      ]

  UnlockScreen ->
    HH.div
      [ HP.class_ (HH.ClassName "identity-screen") ]
      [ HH.h2_ [ HH.text "Welcome back" ]
      , HH.p_
          [ HH.text
              "Enter your passphrase to unlock \
              \your identity."
          ]
      , passphraseField st
      , HH.button
          [ HE.onClick (const UnlockIdentity)
          , HP.class_ (HH.ClassName "btn-primary")
          , HP.disabled (st.passphraseInput == "")
          ]
          [ HH.text "Unlock" ]
      ]

  BootstrapScreen ->
    HH.slot (Proxy :: _ "bootstrap") unit
      Bootstrap.bootstrapComponent
      (map _.prefix st.identity)
      HandleBootstrap

  NonMemberScreen info ->
    HH.div
      [ HP.class_
          (HH.ClassName "non-member-screen")
      ]
      [ HH.h2_ [ HH.text "Not a member yet" ]
      , if info.pendingIntroduction then
          HH.p
            [ HP.class_
                (HH.ClassName "pending-notice")
            ]
            [ HH.text
                "Your introduction is pending. \
                \Please wait for admin approval."
            ]
        else HH.text ""
      , if info.adminEmails /= [] then
          HH.div_
            [ HH.p_
                [ HH.text
                    "Contact an admin to request \
                    \introduction:"
                ]
            , HH.ul_ $ map
                (\email -> HH.li_ [ HH.text email ])
                info.adminEmails
            ]
        else if not info.pendingIntroduction then
          HH.p_
            [ HH.text
                "No admins found. Ask someone \
                \with access to introduce you."
            ]
        else HH.text ""
      , case st.inception of
          Just _ ->
            HH.div
              [ HP.class_
                  (HH.ClassName "inception-share")
              ]
              [ HH.p_
                  [ HH.text
                      "Share your inception JSON \
                      \with an admin:"
                  ]
              , HH.button
                  [ HE.onClick
                      (const CopyInception)
                  , HP.class_
                      (HH.ClassName "btn-primary")
                  ]
                  [ HH.text "Copy Inception JSON" ]
              ]
          Nothing -> HH.text ""
      ]

  NormalScreen ->
    HH.div_
      [ HH.slot (Proxy :: _ "members") unit
          Members.membersComponent
          { circleState:
              st.fullState.circle.circleState
          , myKey: map _.prefix st.identity
          , submitting: st.submitting
          }
          HandleMembers
      , HH.slot (Proxy :: _ "proposals") unit
          Proposals.proposalsComponent
          { proposals: st.fullState.proposals
          , myKey: map _.prefix st.identity
          }
          HandleProposals
      ]

passphraseField
  :: forall m. State -> H.ComponentHTML Action Slots m
passphraseField st =
  HH.input
    [ HP.type_ HP.InputPassword
    , HP.value st.passphraseInput
    , HP.placeholder "Passphrase"
    , HE.onValueInput SetPassphrase
    , HP.class_ (HH.ClassName "passphrase-input")
    ]

-- --------------------------------------------------------
-- Actions
-- --------------------------------------------------------

baseUrl :: String
baseUrl = ""

handleAction
  :: forall o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Init -> do
    hasIdent <- liftEffect Identity.hasStoredIdentity
    if hasIdent then
      H.modify_ _ { screen = UnlockScreen }
    else
      pure unit

  SetPassphrase s ->
    H.modify_ _ { passphraseInput = s }

  GenerateIdentity -> do
    st <- H.get
    result <- liftEffect Identity.generateIdentity
    case result of
      Left err ->
        H.modify_ _
          { error = Just ("Identity gen failed: " <> err) }
      Right { identity, inception, keyState } -> do
        liftAff $ Identity.storeIdentity
          st.passphraseInput
          identity
          inception
        let
          mks = FO.insert identity.prefix
            keyState
            FO.empty
        liftEffect $ KV.persistKeyStates mks
        H.modify_ _
          { identity = Just identity
          , inception = Just inception
          , fullState = (initFullState defaultSequencer unit)
              { memberKeyStates = mks }
          , error = Nothing
          , passphraseInput = ""
          }
        checkMembershipAndLoad identity.prefix

  UnlockIdentity -> do
    st <- H.get
    result <- liftAff $
      Identity.loadIdentity st.passphraseInput
    case result of
      Left err ->
        H.modify_ _
          { error = Just ("Unlock failed: " <> err) }
      Right identity -> do
        inception <- liftEffect
          Identity.loadInceptionData
        mks <- liftEffect KV.loadKeyStates
        H.modify_ _
          { identity = Just identity
          , inception = inception
          , fullState = (initFullState defaultSequencer unit)
              { memberKeyStates = mks }
          , error = Nothing
          , passphraseInput = ""
          }
        checkMembershipAndLoad identity.prefix

  HandleBootstrap output -> case output of
    Bootstrap.Submit passphrase key name -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEvent
            (Just passphrase)
            ident
            (CEBaseDecision (IntroduceMember key name Admin))
        Nothing ->
          H.modify_ _ { error = Just "No identity" }

  HandleMembers output -> case output of
    Members.SubmitDecision bd -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEvent Nothing ident
            (CEBaseDecision bd)
        Nothing ->
          H.modify_ _ { error = Just "No identity" }
    Members.SubmitIntroduceWithInception intro -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEventWithInception
            Nothing
            ident
            ( CEBaseDecision
                ( IntroduceMember
                    intro.memberId
                    intro.name
                    intro.role
                )
            )
            intro.inceptionJson
        Nothing ->
          H.modify_ _ { error = Just "No identity" }

  HandleProposals output -> case output of
    Proposals.SubmitResponse pid -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEvent Nothing ident
            (CEResponse unit pid)
        Nothing ->
          H.modify_ _ { error = Just "No identity" }

  SSENewMessage msgStr -> do
    case parseSseMessage msgStr of
      Left _ -> pure unit
      Right _ -> fetchNewEvents

  SSEKelMessage msgStr -> do
    case parseKelSseMessage msgStr of
      Left _ -> pure unit
      Right { memberId, eventCount } ->
        handleKelUpdate memberId eventCount

  CopyKey -> do
    st <- H.get
    case st.identity of
      Just ident ->
        liftEffect $ Storage.copyToClipboard
          ident.prefix
      Nothing -> pure unit

  CopyInception -> do
    st <- H.get
    case st.inception of
      Just icp ->
        liftEffect $ Storage.copyToClipboard
          (stringify (Identity.encodeInceptionData icp))
      Nothing -> pure unit

  ResetIdentity -> do
    ok <- liftEffect $ Storage.confirm
      "Reset your identity? This cannot be undone."
    when ok do
      liftEffect Identity.clearIdentity
      H.modify_ _
        { identity = Nothing
        , inception = Nothing
        , screen = IdentityScreen
        }

  Dismiss ->
    H.modify_ _ { error = Nothing }

-- --------------------------------------------------------
-- KEL update handler
-- --------------------------------------------------------

handleKelUpdate
  :: forall o m
   . MonadAff m
  => String
  -> Int
  -> H.HalogenM State Action Slots o m Unit
handleKelUpdate memberId _eventCount = do
  st <- H.get
  let
    currentKs = KV.lookupKeyState memberId
      st.fullState.memberKeyStates
  case currentKs of
    Nothing -> do
      result <- liftAff $
        KV.fetchAndValidateFullKel baseUrl memberId
      case result of
        Left err ->
          H.modify_ _
            { error = Just
                ("KEL fetch error: " <> err)
            }
        Right ks -> do
          let
            mks' = KV.insertMemberKel memberId ks
              st.fullState.memberKeyStates
          H.modify_ _ { fullState = st.fullState
            { memberKeyStates = mks' } }
          liftEffect $ KV.persistKeyStates mks'

    Just ks -> do
      let after = ks.sequenceNumber + 1
      res <- liftAff $ Fetch.fetch
        ( baseUrl <> "/members/" <> memberId
            <> "/kel?after="
            <> show after
        )
        { method: "GET", body: "" }
      case res.status of
        200 ->
          case KV.parseKelResponseAndApply ks res.body of
            Left err ->
              H.modify_ _
                { error = Just
                    ("KEL validate: " <> err)
                }
            Right ks' -> do
              let
                mks' = KV.insertMemberKel memberId ks'
                  st.fullState.memberKeyStates
              H.modify_ _ { fullState = st.fullState
                { memberKeyStates = mks' } }
              liftEffect $ KV.persistKeyStates mks'
        _ -> pure unit

  -- Re-enable submission if this was our own KEL
  st' <- H.get
  case st'.identity of
    Just ident
      | ident.prefix == memberId ->
          H.modify_ _ { submitting = false }
    _ -> pure unit

-- --------------------------------------------------------
-- Submission
-- --------------------------------------------------------

submitEvent
  :: forall o m
   . MonadAff m
  => Maybe String
  -> Identity.Identity
  -> CircleEvent Unit Unit Unit
  -> H.HalogenM State Action Slots o m Unit
submitEvent passphrase ident evt = do
  st <- H.get
  let
    evtJson = encodeCircleEvent
      (const jsonNull)
      (const jsonNull)
      (const jsonNull)
      evt
    mKs = KV.lookupKeyState ident.prefix
      st.fullState.memberKeyStates
  case mKs of
    Nothing ->
      H.modify_ _
        { error = Just "No key state for signing" }
    Just ks ->
      case Identity.signCircleEvent ident ks evtJson of
        Left err ->
          H.modify_ _
            { error = Just ("Signing failed: " <> err) }
        Right signature -> do
          -- Include inception for self-intro
          let
            mInception = case evt of
              CEBaseDecision (IntroduceMember mid _ _)
                | mid == ident.prefix ->
                    map Identity.encodeInceptionData
                      st.inception
              _ -> Nothing
          doPost passphrase ident signature evt
            mInception

submitEventWithInception
  :: forall o m
   . MonadAff m
  => Maybe String
  -> Identity.Identity
  -> CircleEvent Unit Unit Unit
  -> String
  -> H.HalogenM State Action Slots o m Unit
submitEventWithInception passphrase ident evt icpJsonStr = do
  st <- H.get
  let
    evtJson = encodeCircleEvent
      (const jsonNull)
      (const jsonNull)
      (const jsonNull)
      evt
    mKs = KV.lookupKeyState ident.prefix
      st.fullState.memberKeyStates
  case mKs of
    Nothing ->
      H.modify_ _
        { error = Just "No key state for signing" }
    Just ks ->
      case Identity.signCircleEvent ident ks evtJson of
        Left err ->
          H.modify_ _
            { error = Just ("Signing failed: " <> err) }
        Right signature -> do
          let
            mInception = case jsonParser icpJsonStr of
              Right json -> Just json
              Left _ -> Nothing
          doPost passphrase ident signature evt
            mInception

doPost
  :: forall o m
   . MonadAff m
  => Maybe String
  -> Identity.Identity
  -> String
  -> CircleEvent Unit Unit Unit
  -> Maybe Json
  -> H.HalogenM State Action Slots o m Unit
doPost passphrase ident signature evt mInception = do
  H.modify_ _ { submitting = true }
  let
    body = encodeSubmission
      (const jsonNull)
      (const jsonNull)
      (const jsonNull)
      { passphrase
      , signer: ident.prefix
      , signature
      , event: evt
      , inception: mInception
      }
  res <- liftAff $ Fetch.fetch
    (baseUrl <> "/events")
    { method: "POST", body: stringify body }
  if res.status /= 200 then do
    H.modify_ _
      { error = Just ("Submit failed: " <> res.body)
      , submitting = false
      }
  else do
    st <- H.get
    fetchNewEvents
    case st.screen of
      BootstrapScreen ->
        checkMembershipAndLoad ident.prefix
      _ -> pure unit

-- --------------------------------------------------------
-- Membership check + event replay
-- --------------------------------------------------------

checkMembershipAndLoad
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
checkMembershipAndLoad key = do
  res <- liftAff $ Fetch.fetch
    ( baseUrl <> "/events?after=-1&key=" <> key )
    { method: "GET", body: "" }
  case res.status of
    200 -> do
      fetchAndReplay key
      startSSE key
    403 -> do
      infoRes <- liftAff $ Fetch.fetch
        (baseUrl <> "/info?key=" <> key)
        { method: "GET", body: "" }
      case parseInfoResponse infoRes.body of
        Left _ ->
          H.modify_ _ { screen = BootstrapScreen }
        Right info ->
          if info.adminEmails == []
            && not info.pendingIntroduction
            then
              H.modify_ _
                { screen = BootstrapScreen }
          else
            H.modify_ _
              { screen = NonMemberScreen info }
    404 ->
      H.modify_ _ { screen = BootstrapScreen }
    _ ->
      H.modify_ _
        { error = Just
            ("Fetch failed: " <> show res.status)
        }

fetchAndReplay
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
fetchAndReplay key = do
  infoRes <- liftAff $ Fetch.fetch
    (baseUrl <> "/info")
    { method: "GET", body: "" }
  let
    seqId = case parseSequencerId infoRes.body of
      Right sid -> sid
      Left _ -> defaultSequencer
    go seqNo fs = do
      res <- liftAff $ Fetch.fetch
        ( baseUrl <> "/events?after=" <> show seqNo
            <> "&key="
            <> key
        )
        { method: "GET", body: "" }
      case res.status of
        404 -> do
          -- Replay done. Build member key states.
          mks <- buildAllMemberKeyStates fs
          let
            screen =
              if isBootstrap fs.circle.circleState
                then BootstrapScreen
              else NormalScreen
          H.modify_ _
            { fullState = fs
                { memberKeyStates = mks }
            , serverSeqNo = seqNo + 1
            , screen = screen
            }
          liftEffect $ KV.persistKeyStates mks
        200 ->
          case parseEventResponse res.body of
            Left err ->
              H.modify_ _
                { error = Just
                    ("Decode error: " <> err)
                }
            Right { signer, event } ->
              case
                decodeCircleEvent
                  (const (Right unit))
                  (const (Right unit))
                  (const (Right unit))
                  event
                of
                Left err ->
                  H.modify_ _
                    { error = Just
                        ( "Event decode: "
                            <> printJsonDecodeError err
                        )
                    }
                Right evt -> do
                  let
                    fs' = applyCircleEvent
                      trivialFold
                      fs
                      (Tuple signer evt)
                  go (seqNo + 1) fs'
        _ ->
          H.modify_ _
            { error = Just
                ("Fetch failed: " <> show res.status)
            }
  go (-1) (initFullState seqId unit)

buildAllMemberKeyStates
  :: forall o m
   . MonadAff m
  => AppState
  -> H.HalogenM State Action Slots o m
       KV.MemberKeyStates
buildAllMemberKeyStates fs = do
  st <- H.get
  let
    memberIds = map _.memberId
      fs.circle.circleState.members
    -- Start with existing key states (from
    -- localStorage on unlock, or from identity gen)
    existing = st.fullState.memberKeyStates
  foldM
    ( \acc mid -> do
        case KV.lookupKeyState mid acc of
          Just _ -> pure acc
          Nothing -> do
            result <- liftAff $
              try (KV.fetchAndValidateFullKel baseUrl mid)
            case result of
              Left _ -> pure acc
              Right (Left _) -> pure acc
              Right (Right ks) ->
                pure (KV.insertMemberKel mid ks acc)
    )
    existing
    memberIds

fetchNewEvents
  :: forall o m
   . MonadAff m
  => H.HalogenM State Action Slots o m Unit
fetchNewEvents = do
  st <- H.get
  case st.identity of
    Nothing -> pure unit
    Just ident -> do
      let
        key = ident.prefix
        go seqNo fs = do
          res <- liftAff $ Fetch.fetch
            ( baseUrl <> "/events?after="
                <> show seqNo
                <> "&key="
                <> key
            )
            { method: "GET", body: "" }
          case res.status of
            404 -> do
              let
                screen =
                  if isBootstrap
                    fs.circle.circleState
                    then BootstrapScreen
                  else NormalScreen
              H.modify_ _
                { fullState = fs
                , serverSeqNo = seqNo + 1
                , screen = screen
                }
            200 ->
              case parseEventResponse res.body of
                Left _ ->
                  H.modify_ _
                    { serverSeqNo = seqNo + 1 }
                Right { signer, event } ->
                  case
                    decodeCircleEvent
                      (const (Right unit))
                      (const (Right unit))
                      (const (Right unit))
                      event
                    of
                    Left _ ->
                      H.modify_ _
                        { serverSeqNo = seqNo + 1 }
                    Right evt -> do
                      let
                        fs' = applyCircleEvent
                          trivialFold
                          fs
                          (Tuple signer evt)
                      go (seqNo + 1) fs'
            _ ->
              H.modify_ _
                { error = Just
                    ( "Fetch failed: "
                        <> show res.status
                    )
                }
      go (st.serverSeqNo - 1) st.fullState

-- --------------------------------------------------------
-- SSE
-- --------------------------------------------------------

startSSE
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
startSSE key = do
  { emitter, listener } <- liftEffect HS.create
  void $ H.subscribe emitter
  es <- liftEffect do
    es <- SSE.create
      (baseUrl <> "/stream?key=" <> key)
    SSE.onMessage es \msg ->
      HS.notify listener (SSENewMessage msg)
    SSE.onKelMessage es \msg ->
      HS.notify listener (SSEKelMessage msg)
    pure es
  H.modify_ _ { sse = Just es }

-- --------------------------------------------------------
-- Helpers
-- --------------------------------------------------------

trivialFold :: Unit -> Unit -> Unit
trivialFold _ _ = unit

parseSseMessage :: String -> Either String Int
parseSseMessage s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    obj .: "sn"

parseKelSseMessage
  :: String
  -> Either String
       { memberId :: String, eventCount :: Int }
parseKelSseMessage s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    memberId <- obj .: "memberId"
    eventCount <- obj .: "eventCount"
    pure { memberId, eventCount }

parseEventResponse
  :: String
  -> Either String { signer :: String, event :: Json }
parseEventResponse s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    signer <- obj .: "signer"
    event <- obj .: "event"
    pure { signer, event }

parseInfoResponse
  :: String -> Either String InfoResponse
parseInfoResponse s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError (decodeInfoResponse json)

parseSequencerId
  :: String -> Either String MemberId
parseSequencerId s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    obj .: "sequencerId"
