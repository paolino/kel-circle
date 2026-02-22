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
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import FFI.Fetch as Fetch
import FFI.SSE as SSE
import FFI.Storage as Storage
import FFI.TextEncoder (encodeUtf8)
import FFI.TweetNaCl as NaCl
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Subscription as HS
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as Cesr
import Keri.Cesr.Primitive (mkPrimitive)
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
import KelCircle.Client.Fold (FullState, applyCircleEvent, initFullState)
import KelCircle.Client.State (isBootstrap)
import KelCircle.Client.Types (MemberId, Role(..))
import Type.Proxy (Proxy(..))
import View.Bootstrap as Bootstrap
import View.Members as Members
import View.Proposals as Proposals

type Identity =
  { keyPair :: NaCl.KeyPair
  , prefix :: String
  }

type InfoResponse =
  { adminEmails :: Array String
  , pendingIntroduction :: Boolean
  }

data Screen
  = IdentityScreen
  | BootstrapScreen
  | NonMemberScreen InfoResponse
  | NormalScreen

-- | Trivial app: no application events (Unit for d, p, r).
type AppState = FullState Unit Unit Unit

type State =
  { screen :: Screen
  , identity :: Maybe Identity
  , fullState :: AppState
  , serverSeqNo :: Int
  , sse :: Maybe SSE.EventSource
  , error :: Maybe String
  }

data Action
  = Init
  | GenerateIdentity
  | HandleBootstrap Bootstrap.Output
  | HandleMembers Members.Output
  | HandleProposals Proposals.Output
  | SSEMessage String
  | CopyKey
  | ResetIdentity
  | Dismiss

type Slots =
  ( bootstrap :: H.Slot (Const Void) Bootstrap.Output Unit
  , members :: H.Slot (Const Void) Members.Output Unit
  , proposals :: H.Slot (Const Void) Proposals.Output Unit
  )

-- | Default sequencer ID for client-side state initialization.
defaultSequencer :: MemberId
defaultSequencer = "server"

appComponent :: forall q i o m. MonadAff m => H.Component q i o m
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
  , fullState: initFullState defaultSequencer unit
  , serverSeqNo: 0
  , sse: Nothing
  , error: Nothing
  }

render :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
render st = HH.div [ HP.class_ (HH.ClassName "app") ]
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

header :: forall m. State -> H.ComponentHTML Action Slots m
header st = HH.nav [ HP.class_ (HH.ClassName "header") ]
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
    if String.length s > 8 then String.take 8 s <> "..."
    else s

content :: forall m. MonadAff m => State -> H.ComponentHTML Action Slots m
content st = case st.screen of
  IdentityScreen ->
    HH.div [ HP.class_ (HH.ClassName "identity-screen") ]
      [ HH.h2_ [ HH.text "Welcome to kel-circle" ]
      , HH.p_ [ HH.text "Generate a KERI identity to get started." ]
      , HH.button
          [ HE.onClick (const GenerateIdentity)
          , HP.class_ (HH.ClassName "btn-primary")
          ]
          [ HH.text "Generate Identity" ]
      ]

  BootstrapScreen ->
    HH.slot (Proxy :: _ "bootstrap") unit
      Bootstrap.bootstrapComponent
      (map _.prefix st.identity)
      HandleBootstrap

  NonMemberScreen info ->
    HH.div [ HP.class_ (HH.ClassName "non-member-screen") ]
      [ HH.h2_ [ HH.text "Not a member yet" ]
      , if info.pendingIntroduction then
          HH.p [ HP.class_ (HH.ClassName "pending-notice") ]
            [ HH.text
                "Your introduction is pending. \
                \Please wait for admin approval."
            ]
        else HH.text ""
      , if info.adminEmails /= [] then
          HH.div_
            [ HH.p_ [ HH.text "Contact an admin to request introduction:" ]
            , HH.ul_ $ map
                ( \email ->
                    HH.li_ [ HH.text email ]
                )
                info.adminEmails
            ]
        else if not info.pendingIntroduction then
          HH.p_
            [ HH.text
                "No admins found. Ask someone with \
                \access to introduce you."
            ]
        else HH.text ""
      ]

  NormalScreen ->
    HH.div_
      [ HH.slot (Proxy :: _ "members") unit
          Members.membersComponent
          { circleState: st.fullState.circle.circleState
          , myKey: map _.prefix st.identity
          }
          HandleMembers
      , HH.slot (Proxy :: _ "proposals") unit
          Proposals.proposalsComponent
          { proposals: st.fullState.proposals
          , myKey: map _.prefix st.identity
          }
          HandleProposals
      ]

baseUrl :: String
baseUrl = ""

handleAction
  :: forall o m
   . MonadAff m
  => Action
  -> H.HalogenM State Action Slots o m Unit
handleAction = case _ of
  Init -> do
    mPrefix <- liftEffect $ Storage.getItem "kel-circle-prefix"
    case mPrefix of
      Just prefix -> do
        kp <- liftEffect NaCl.generateKeyPair
        let ident = { keyPair: kp, prefix }
        H.modify_ _ { identity = Just ident }
        checkMembershipAndLoad prefix
      Nothing -> pure unit

  GenerateIdentity -> do
    kp <- liftEffect NaCl.generateKeyPair
    case mkPrimitive Ed25519PubKey kp.publicKey of
      Left err ->
        H.modify_ _ { error = Just err }
      Right prim -> do
        let
          prefix = Cesr.encode prim
          ident = { keyPair: kp, prefix }
        liftEffect $ Storage.setItem "kel-circle-prefix" prefix
        H.modify_ _ { identity = Just ident, error = Nothing }
        checkMembershipAndLoad prefix

  HandleBootstrap output -> case output of
    Bootstrap.Submit passphrase key -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEvent
            (Just passphrase)
            ident
            (CEBaseDecision (IntroduceMember key key Admin))
        Nothing ->
          H.modify_ _
            { error = Just "No identity" }

  HandleMembers output -> case output of
    Members.SubmitDecision bd -> do
      st <- H.get
      case st.identity of
        Just ident ->
          submitEvent Nothing ident
            (CEBaseDecision bd)
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

  SSEMessage msgStr -> do
    case parseSseMessage msgStr of
      Left _ -> pure unit
      Right _ -> fetchNewEvents

  CopyKey -> do
    st <- H.get
    case st.identity of
      Just ident -> liftEffect $ Storage.copyToClipboard ident.prefix
      Nothing -> pure unit

  ResetIdentity -> do
    ok <- liftEffect $ Storage.confirm "Reset your identity? This cannot be undone."
    when ok do
      liftEffect $ Storage.removeItem "kel-circle-prefix"
      H.modify_ _ { identity = Nothing, screen = IdentityScreen }

  Dismiss ->
    H.modify_ _ { error = Nothing }

-- | Check membership via /info + /events, decide which screen.
checkMembershipAndLoad
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
checkMembershipAndLoad key = do
  res <- liftAff $ Fetch.fetch
    (baseUrl <> "/events?after=-1&key=" <> key)
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
          if info.adminEmails == [] && not info.pendingIntroduction then
            H.modify_ _ { screen = BootstrapScreen }
          else
            H.modify_ _ { screen = NonMemberScreen info }
    404 ->
      H.modify_ _ { screen = BootstrapScreen }
    _ ->
      H.modify_ _
        { error = Just ("Fetch failed: " <> show res.status) }

-- | Sign a circle event and compute CESR signature.
signEvent
  :: Identity
  -> CircleEvent Unit Unit Unit
  -> Either String String
signEvent ident evt = do
  let
    evtJson = encodeCircleEvent
      (const jsonNull)
      (const jsonNull)
      (const jsonNull)
      evt
    msgBytes = encodeUtf8 (stringify evtJson)
    sigBytes = NaCl.sign msgBytes ident.keyPair.secretKey
  sigPrim <- mkPrimitive Ed25519Sig sigBytes
  pure (Cesr.encode sigPrim)

-- | Submit a circle event to the server.
submitEvent
  :: forall o m
   . MonadAff m
  => Maybe String
  -> Identity
  -> CircleEvent Unit Unit Unit
  -> H.HalogenM State Action Slots o m Unit
submitEvent passphrase ident evt =
  case signEvent ident evt of
    Left err ->
      H.modify_ _
        { error = Just ("Signing failed: " <> err) }
    Right signature -> do
      let
        body = encodeSubmission
          (const jsonNull)
          (const jsonNull)
          (const jsonNull)
          { passphrase
          , signer: ident.prefix
          , signature
          , event: evt
          }
      res <- liftAff $ Fetch.fetch
        (baseUrl <> "/events")
        { method: "POST", body: stringify body }
      if res.status /= 200 then
        H.modify_ _
          { error =
              Just ("Submit failed: " <> res.body)
          }
      else do
        st <- H.get
        fetchNewEvents
        case st.screen of
          BootstrapScreen ->
            checkMembershipAndLoad ident.prefix
          _ -> pure unit

-- | Fetch all events from the beginning and rebuild state.
fetchAndReplay
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
fetchAndReplay key = do
  let
    go seqNo fs = do
      res <- liftAff $ Fetch.fetch
        ( baseUrl <> "/events?after=" <> show seqNo
            <> "&key="
            <> key
        )
        { method: "GET", body: "" }
      case res.status of
        404 -> do
          let
            screen =
              if isBootstrap fs.circle.circleState then BootstrapScreen
              else NormalScreen
          H.modify_ _
            { fullState = fs
            , serverSeqNo = seqNo + 1
            , screen = screen
            }
        200 ->
          case parseEventResponse res.body of
            Left err ->
              H.modify_ _
                { error = Just ("Decode error: " <> err) }
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
                        ("Event decode: " <> printJsonDecodeError err)
                    }
                Right evt -> do
                  let fs' = applyCircleEvent trivialFold fs (Tuple signer evt)
                  go (seqNo + 1) fs'
        _ ->
          H.modify_ _ { error = Just ("Fetch failed: " <> show res.status) }
  go (-1) (initFullState defaultSequencer unit)

-- | Fetch new events since our last known position.
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
            ( baseUrl <> "/events?after=" <> show seqNo
                <> "&key="
                <> key
            )
            { method: "GET", body: "" }
          case res.status of
            404 -> do
              let
                screen =
                  if isBootstrap fs.circle.circleState then BootstrapScreen
                  else NormalScreen
              H.modify_ _
                { fullState = fs
                , serverSeqNo = seqNo + 1
                , screen = screen
                }
            200 ->
              case parseEventResponse res.body of
                Left _ -> H.modify_ _ { serverSeqNo = seqNo + 1 }
                Right { signer, event } ->
                  case
                    decodeCircleEvent
                      (const (Right unit))
                      (const (Right unit))
                      (const (Right unit))
                      event
                    of
                    Left _ -> H.modify_ _ { serverSeqNo = seqNo + 1 }
                    Right evt -> do
                      let fs' = applyCircleEvent trivialFold fs (Tuple signer evt)
                      go (seqNo + 1) fs'
            _ ->
              H.modify_ _ { error = Just ("Fetch failed: " <> show res.status) }
      go (st.serverSeqNo - 1) st.fullState

-- | Start SSE subscription.
startSSE
  :: forall o m
   . MonadAff m
  => String
  -> H.HalogenM State Action Slots o m Unit
startSSE key = do
  { emitter, listener } <- liftEffect HS.create
  void $ H.subscribe emitter
  es <- liftEffect do
    es <- SSE.create (baseUrl <> "/stream?key=" <> key)
    SSE.onMessage es \msg ->
      HS.notify listener (SSEMessage msg)
    pure es
  H.modify_ _ { sse = Just es }

trivialFold :: Unit -> Unit -> Unit
trivialFold _ _ = unit

-- | Parse SSE data: @{"sn":N}@
parseSseMessage :: String -> Either String Int
parseSseMessage s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    obj .: "sn"

-- | Parse a GET /events response body.
parseEventResponse
  :: String -> Either String { signer :: String, event :: Json }
parseEventResponse s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    signer <- obj .: "signer"
    event <- obj .: "event"
    pure { signer, event }

-- | Parse a GET /info response body.
parseInfoResponse
  :: String -> Either String InfoResponse
parseInfoResponse s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError (decodeInfoResponse json)
