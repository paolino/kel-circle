-- | Identity lifecycle: generate, store, load, sign.
module KelCircle.Client.Identity
  ( Identity
  , InceptionData
  , generateIdentity
  , storeIdentity
  , loadIdentity
  , signCircleEvent
  , hasStoredIdentity
  , clearIdentity
  , loadInceptionData
  , encodeInceptionData
  ) where

import Prelude

import Data.Argonaut.Core
  ( Json
  , fromArray
  , fromNumber
  , fromObject
  , fromString
  , stringify
  , toArray
  , toNumber
  , toObject
  , toString
  )
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapMaybe)
import Data.Either (Either(..))
import Data.Int (floor, toNumber) as Int
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import FFI.KeyRestore (keyPairFromSecretKey)
import FFI.Storage as Storage
import FFI.TextEncoder (encodeUtf8)
import FFI.TweetNaCl as NaCl
import FFI.WebCrypto as WebCrypto
import Foreign.Object as FO
import Keri.Cesr.DerivationCode (DerivationCode(..))
import Keri.Cesr.Encode as CesrEncode
import Keri.Cesr.Primitive (mkPrimitive)
import Keri.Event (Event(..), eventPrefix)
import Keri.Event.Inception (mkInception)
import Keri.Event.Interaction (mkInteraction)
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState (KeyState, initialState)
import Keri.KeyState.PreRotation (commitKey)

-- | Client identity: keypair + identifiers.
type Identity =
  { keyPair :: NaCl.KeyPair
  , prefix :: String
  -- ^ CESR public key (MemberId/signer)
  , keriPrefix :: String
  -- ^ SAID from inception (KERI AID)
  }

-- | Signed inception event data for submission.
type InceptionData =
  { event :: String
  , signatures :: Array (Tuple Int String)
  }

-- | Generate a new identity with inception event.
generateIdentity
  :: Effect
       ( Either String
           { identity :: Identity
           , inception :: InceptionData
           , keyState :: KeyState
           }
       )
generateIdentity = do
  kp <- NaCl.generateKeyPair
  pure do
    pubPrim <- mkPrimitive Ed25519PubKey kp.publicKey
    let prefix = CesrEncode.encode pubPrim
    nextKeyCommitment <- commitKey prefix
    let
      icp = mkInception
        { keys: [ prefix ]
        , signingThreshold: 1
        , nextKeys: [ nextKeyCommitment ]
        , nextThreshold: 1
        , config: []
        , anchors: []
        }
      serialized = serializeEvent icp
      msgBytes = encodeUtf8 serialized
      sigBytes = NaCl.sign msgBytes kp.secretKey
    sigPrim <- mkPrimitive Ed25519Sig sigBytes
    let
      sigCesr = CesrEncode.encode sigPrim
      keriPrefix = eventPrefix icp
      identity =
        { keyPair: kp, prefix, keriPrefix }
      inceptionData =
        { event: serialized
        , signatures: [ Tuple 0 sigCesr ]
        }
      ks = case icp of
        Inception d -> initialState d
        _ -> initialState
          { version: ""
          , digest: ""
          , prefix: keriPrefix
          , sequenceNumber: 0
          , signingThreshold: 1
          , keys: [ prefix ]
          , nextThreshold: 1
          , nextKeys: [ nextKeyCommitment ]
          , witnessThreshold: 0
          , witnesses: []
          , config: []
          , anchors: []
          }
    pure
      { identity
      , inception: inceptionData
      , keyState: ks
      }

-- | Store identity encrypted in localStorage.
storeIdentity
  :: String
  -> Identity
  -> InceptionData
  -> Aff Unit
storeIdentity passphrase ident inception = do
  encrypted <- WebCrypto.encrypt passphrase
    ident.keyPair.secretKey
  liftEffect do
    Storage.setItem "kel-circle-encrypted"
      encrypted
    Storage.setItem "kel-circle-prefix"
      ident.prefix
    Storage.setItem "kel-circle-keri-prefix"
      ident.keriPrefix
    Storage.setItem "kel-circle-inception"
      (stringify (encodeInceptionData inception))

-- | Load identity from localStorage.
loadIdentity
  :: String -> Aff (Either String Identity)
loadIdentity passphrase = do
  mEnc <- liftEffect $
    Storage.getItem "kel-circle-encrypted"
  mPfx <- liftEffect $
    Storage.getItem "kel-circle-prefix"
  mKpfx <- liftEffect $
    Storage.getItem "kel-circle-keri-prefix"
  case mEnc of
    Nothing -> pure $ Left "no stored identity"
    Just encrypted -> case mPfx of
      Nothing -> pure $ Left "no stored prefix"
      Just prefix -> case mKpfx of
        Nothing ->
          pure $ Left "no stored keri prefix"
        Just keriPrefix -> do
          secretKey <-
            WebCrypto.decrypt passphrase encrypted
          kp <- liftEffect $
            keyPairFromSecretKey secretKey
          pure $ Right
            { keyPair: kp
            , prefix
            , keriPrefix
            }

-- | Sign a circle event as a KERI interaction.
-- | Returns the CESR-encoded Ed25519 signature.
signCircleEvent
  :: Identity
  -> KeyState
  -> Json
  -> Either String String
signCircleEvent ident ks evtJson = do
  let
    ixn = mkInteraction
      { prefix: ident.keriPrefix
      , sequenceNumber: ks.sequenceNumber + 1
      , priorDigest: ks.lastDigest
      , anchors: [ evtJson ]
      }
    serialized = serializeEvent ixn
    msgBytes = encodeUtf8 serialized
    sigBytes = NaCl.sign msgBytes
      ident.keyPair.secretKey
  sigPrim <- mkPrimitive Ed25519Sig sigBytes
  pure (CesrEncode.encode sigPrim)

-- | Check if there's a stored identity.
hasStoredIdentity :: Effect Boolean
hasStoredIdentity = do
  m <- Storage.getItem "kel-circle-encrypted"
  pure (isJust m)

-- | Clear all stored identity data.
clearIdentity :: Effect Unit
clearIdentity = do
  Storage.removeItem "kel-circle-encrypted"
  Storage.removeItem "kel-circle-prefix"
  Storage.removeItem "kel-circle-keri-prefix"
  Storage.removeItem "kel-circle-inception"
  Storage.removeItem "kel-circle-key-states"

-- | Load stored inception data.
loadInceptionData :: Effect (Maybe InceptionData)
loadInceptionData = do
  mStr <- Storage.getItem "kel-circle-inception"
  pure (mStr >>= parseInceptionStr)

-- | Encode inception data as JSON Value for the
-- | server submission. Matches server's
-- | InceptionSubmission format.
encodeInceptionData :: InceptionData -> Json
encodeInceptionData icp =
  fromObject $ FO.fromFoldable
    [ Tuple "event" (fromString icp.event)
    , Tuple "signatures" (fromArray sigsJson)
    ]
  where
  sigsJson = map
    ( \(Tuple idx sig) ->
        fromArray
          [ fromNumber (Int.toNumber idx)
          , fromString sig
          ]
    )
    icp.signatures

-- Helpers

parseInceptionStr :: String -> Maybe InceptionData
parseInceptionStr str = case jsonParser str of
  Left _ -> Nothing
  Right json -> do
    obj <- toObject json
    event <- FO.lookup "event" obj >>= toString
    sigsJson <-
      FO.lookup "signatures" obj >>= toArray
    let sigs = mapMaybe parseSigTuple sigsJson
    pure { event, signatures: sigs }

parseSigTuple :: Json -> Maybe (Tuple Int String)
parseSigTuple j = do
  arr <- toArray j
  case arr of
    [ idxJ, sigJ ] -> do
      idx <- toNumber idxJ
      sig <- toString sigJ
      pure (Tuple (Int.floor idx) sig)
    _ -> Nothing
