-- | Bridge between circle events and KERI interaction events.
module KelCircle.Client.Message
  ( KelCircleMessage
  , mkCircleMessage
  , verifyCircleMessage
  , extractCircleEvent
  , serializeEnvelope
  , deserializeEnvelope
  ) where

import Prelude

import Data.Argonaut.Core
  ( Json
  , fromObject
  , fromString
  , stringify
  )
import Data.Argonaut.Decode
  ( JsonDecodeError
  , decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Foreign.Object as Object
import FFI.TextEncoder (encodeUtf8)
import FFI.TweetNaCl as NaCl
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as CesrEncode
import Keri.Cesr.Primitive (mkPrimitive)
import Keri.Event (eventPrefix)
import Keri.Event.Interaction (InteractionConfig, mkInteraction)
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState (KeyState)
import Keri.KeyState.Verify (verifySignatures)
import Keri.Kel (SignedEvent) as Kel

-- | A KERI-wrapped circle message.
type KelCircleMessage a =
  { keriEvent :: Kel.SignedEvent
  , circleEvent :: a
  }

-- | Create a signed KERI interaction event wrapping a circle event.
mkCircleMessage
  :: forall a
   . (a -> Json)
  -> { prefix :: String
     , sequenceNumber :: Int
     , priorDigest :: String
     , keyPair :: NaCl.KeyPair
     , keyIndex :: Int
     }
  -> a
  -> Either String (KelCircleMessage a)
mkCircleMessage encodeEvt cfg circleEvent = do
  let
    canonical = stringify (encodeEvt circleEvent)
    anchor = fromString canonical

    ixnConfig :: InteractionConfig
    ixnConfig =
      { prefix: cfg.prefix
      , sequenceNumber: cfg.sequenceNumber
      , priorDigest: cfg.priorDigest
      , anchors: [ anchor ]
      }
    ixn = mkInteraction ixnConfig
    serialized = serializeEvent ixn
    msgBytes = encodeUtf8 serialized
    sigBytes = NaCl.sign msgBytes cfg.keyPair.secretKey
  sigPrim <- mkPrimitive Ed25519Sig sigBytes
  let
    sigCesr = CesrEncode.encode sigPrim
    keriEvent =
      { event: ixn
      , signatures:
          [ { index: cfg.keyIndex, signature: sigCesr } ]
      }
  pure { keriEvent, circleEvent }

-- | Verify a circle message's KERI signature against key state.
verifyCircleMessage
  :: forall a. KeyState -> KelCircleMessage a -> Boolean
verifyCircleMessage ks msg =
  let
    serialized = serializeEvent msg.keriEvent.event
  in
    verifySignatures
      ks.keys
      ks.signingThreshold
      serialized
      msg.keriEvent.signatures

-- | Extract signer AID and circle event from a verified message.
extractCircleEvent
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> KelCircleMessage a
  -> Either String (Tuple String a)
extractCircleEvent _ msg =
  let
    prefix = eventPrefix msg.keriEvent.event
  in
    Right (Tuple prefix msg.circleEvent)

-- | Serialize a KERI signed event to JSON string (for POST).
serializeEnvelope :: forall a. KelCircleMessage a -> String
serializeEnvelope msg =
  let
    serialized = serializeEvent msg.keriEvent.event
  in
    stringify $ fromObject $ Object.fromFoldable
      [ Tuple "event" (fromString serialized) ]

-- | Deserialize a KERI envelope payload from server.
deserializeEnvelope
  :: forall a
   . (Json -> Either JsonDecodeError a)
  -> String
  -> Either String { signer :: String, circleEvent :: a }
deserializeEnvelope decodeEvt s = do
  json <- lmap show (jsonParser s)
  lmap printJsonDecodeError do
    obj <- decodeJson json
    signer <- obj .: "signer"
    evtJson <- obj .: "event"
    evt <- decodeEvt evtJson
    pure { signer, circleEvent: evt }
