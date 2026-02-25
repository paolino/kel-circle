-- | Manages ALL members' key states, mirroring the
-- | server's fsKeyStates :: Map MemberId KelKeyState.
module KelCircle.Client.KelValidate
  ( MemberKeyStates
  , emptyKeyStates
  , validateAndApplyKelEvents
  , fetchAndValidateFullKel
  , parseKelResponseAndApply
  , insertMemberKel
  , removeMemberKel
  , lookupKeyState
  , persistKeyStates
  , loadKeyStates
  ) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (drop, head, mapMaybe)
import Data.Foldable (foldM)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.Common (joinWith) as Str
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import FFI.Fetch as Fetch
import FFI.Storage as Storage
import Foreign.Object as FO
import KelCircle.Client.KeriParse (parseKeriEvent)
import Keri.Event (Event(..))
import Keri.KeyState (KeyState, applyEvent, initialState)
import Keri.KeyState.Verify (verifySignatures)

-- | Map from MemberId to their current KeyState.
type MemberKeyStates = FO.Object KeyState

emptyKeyStates :: MemberKeyStates
emptyKeyStates = FO.empty

insertMemberKel
  :: String -> KeyState -> MemberKeyStates -> MemberKeyStates
insertMemberKel = FO.insert

removeMemberKel
  :: String -> MemberKeyStates -> MemberKeyStates
removeMemberKel = FO.delete

lookupKeyState
  :: String -> MemberKeyStates -> Maybe KeyState
lookupKeyState = FO.lookup

-- | A KEL event response from the server.
type KelEventResponse =
  { event :: String
  , signatures :: Array { index :: Int, signature :: String }
  }

-- | Validate and apply incremental KEL events to a
-- | member's key state.
validateAndApplyKelEvents
  :: KeyState
  -> Array KelEventResponse
  -> Either String KeyState
validateAndApplyKelEvents = foldM applyOne
  where
  applyOne ks evtResp = do
    parsed <- parseKeriEvent evtResp.event
    let
      valid = verifySignatures
        ks.keys
        ks.signingThreshold
        evtResp.event
        evtResp.signatures
    when (not valid) do
      Left "signature verification failed"
    applyEvent ks parsed

-- | Fetch a member's full KEL from the server and
-- | build their KeyState from scratch.
fetchAndValidateFullKel
  :: String -> String -> Aff (Either String KeyState)
fetchAndValidateFullKel baseUrl memberId = do
  res <- Fetch.fetch
    ( baseUrl <> "/members/" <> memberId
        <> "/kel"
    )
    { method: "GET", body: "" }
  pure case res.status of
    200 -> do
      events <- parseKelResponse res.body
      buildKeyState events
    _ ->
      Left
        ( "KEL fetch failed: "
            <> show res.status
        )

-- | Build a KeyState from a full sequence of KEL
-- | events (starting with inception).
buildKeyState
  :: Array KelEventResponse
  -> Either String KeyState
buildKeyState events = do
  first <- maybe (Left "empty KEL") Right
    (head events)
  icpEvent <- parseKeriEvent first.event
  icpData <- case icpEvent of
    Inception d -> Right d
    _ -> Left "first KEL event is not inception"
  let
    valid = verifySignatures
      icpData.keys
      icpData.signingThreshold
      first.event
      first.signatures
  when (not valid) do
    Left "inception signature verification failed"
  let ks0 = initialState icpData
  validateAndApplyKelEvents ks0 (drop 1 events)

-- | Parse the KEL response body into event responses.
parseKelResponse
  :: String
  -> Either String (Array KelEventResponse)
parseKelResponse body = do
  json <- lmap show (jsonParser body)
  obj <-
    maybe (Left "not a JSON object") Right
      (toObject json)
  evtsJson <-
    maybe (Left "missing events field") Right
      (FO.lookup "events" obj >>= toArray)
  Right (mapMaybe parseOneEvent evtsJson)

parseOneEvent :: Json -> Maybe KelEventResponse
parseOneEvent json = do
  obj <- toObject json
  event <- FO.lookup "event" obj >>= toString
  sigsJson <-
    FO.lookup "signatures" obj >>= toArray
  let sigs = mapMaybe parseSig sigsJson
  pure { event, signatures: sigs }

-- | Parse a signature from server format.
-- | Server sends (Int, Text) tuples as 2-element
-- | arrays: [[0, "sig"]].
parseSig
  :: Json
  -> Maybe { index :: Int, signature :: String }
parseSig json = do
  arr <- toArray json
  case arr of
    [ idxJson, sigJson ] -> do
      idx <- toNumber idxJson
      sig <- toString sigJson
      pure { index: floor idx, signature: sig }
    _ -> Nothing

-- | Parse a KEL response and apply events to an
-- | existing key state. Used for incremental updates.
parseKelResponseAndApply
  :: KeyState
  -> String
  -> Either String KeyState
parseKelResponseAndApply ks body = do
  events <- parseKelResponse body
  validateAndApplyKelEvents ks events

-- | localStorage key for persisted key states.
keyStatesStorageKey :: String
keyStatesStorageKey = "kel-circle-key-states"

-- | Persist key states to localStorage as JSON.
persistKeyStates :: MemberKeyStates -> Effect Unit
persistKeyStates mks =
  Storage.setItem keyStatesStorageKey
    (stringifyKeyStates mks)

-- | Load key states from localStorage.
loadKeyStates :: Effect MemberKeyStates
loadKeyStates = do
  mStr <- Storage.getItem keyStatesStorageKey
  pure $ case mStr of
    Nothing -> FO.empty
    Just str ->
      fromMaybe FO.empty (parseKeyStates str)

-- | Serialize MemberKeyStates to a JSON string.
stringifyKeyStates :: MemberKeyStates -> String
stringifyKeyStates mks =
  let
    entries =
      FO.toUnfoldable mks
        :: Array (Tuple String KeyState)
    pairs = map
      ( \(Tuple k ks) ->
          jsonStr k <> ":" <> stringifyKs ks
      )
      entries
  in
    "{" <> Str.joinWith "," pairs <> "}"

stringifyKs :: KeyState -> String
stringifyKs ks =
  "{\"prefix\":"
    <> jsonStr ks.prefix
    <> ",\"sequenceNumber\":"
    <> show ks.sequenceNumber
    <> ",\"lastDigest\":"
    <> jsonStr ks.lastDigest
    <> ",\"signingThreshold\":"
    <> show ks.signingThreshold
    <> ",\"keys\":"
    <> jsonStrArr ks.keys
    <> ",\"nextThreshold\":"
    <> show ks.nextThreshold
    <> ",\"nextKeys\":"
    <> jsonStrArr ks.nextKeys
    <> "}"

-- | Parse key states from a JSON string.
parseKeyStates :: String -> Maybe MemberKeyStates
parseKeyStates str = case jsonParser str of
  Left _ -> Nothing
  Right json -> do
    obj <- toObject json
    let
      entries =
        FO.toUnfoldable obj
          :: Array (Tuple String Json)
      parsed = mapMaybe
        ( \(Tuple k v) -> do
            ks <- parseKs v
            pure (Tuple k ks)
        )
        entries
    pure (FO.fromFoldable parsed)

parseKs :: Json -> Maybe KeyState
parseKs json = do
  obj <- toObject json
  prefix <- FO.lookup "prefix" obj >>= toString
  sequenceNumber <-
    FO.lookup "sequenceNumber" obj
      >>= toNumber
      >>= (\n -> pure (floor n))
  lastDigest <-
    FO.lookup "lastDigest" obj >>= toString
  signingThreshold <-
    FO.lookup "signingThreshold" obj
      >>= toNumber
      >>= (\n -> pure (floor n))
  keysJson <- FO.lookup "keys" obj >>= toArray
  let keys = mapMaybe toString keysJson
  nextThreshold <-
    FO.lookup "nextThreshold" obj
      >>= toNumber
      >>= (\n -> pure (floor n))
  nextKeysJson <-
    FO.lookup "nextKeys" obj >>= toArray
  let nextKeys = mapMaybe toString nextKeysJson
  pure
    { prefix
    , sequenceNumber
    , lastDigest
    , signingThreshold
    , keys
    , nextThreshold
    , nextKeys
    }

-- Helpers

jsonStr :: String -> String
jsonStr s = "\"" <> s <> "\""

jsonStrArr :: Array String -> String
jsonStrArr arr =
  "[" <> Str.joinWith "," (map jsonStr arr) <> "]"
