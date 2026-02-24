-- | Parse canonical KERI JSON strings into keri-purs
-- | Event values.
module KelCircle.Client.KeriParse
  ( parseKeriEvent
  ) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (mapMaybe)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromString, fromStringAs, hexadecimal)
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object as FO
import Keri.Event
  ( Event(..)
  , InceptionData
  , InteractionData
  , RotationData
  )

-- | Parse a canonical KERI JSON string into an Event.
parseKeriEvent :: String -> Either String Event
parseKeriEvent str = do
  json <- lmap show (jsonParser str)
  obj <- maybe (Left "not a JSON object") Right
    (toObject json)
  evtType <- getStr obj "t"
  case evtType of
    "icp" -> Inception <$> parseInception obj
    "ixn" -> Interaction <$> parseInteraction obj
    "rot" -> Rotation <$> parseRotation obj
    _ -> Left ("unknown event type: " <> evtType)

parseInception
  :: FO.Object Json -> Either String InceptionData
parseInception obj = do
  version <- getStr obj "v"
  digest <- getStr obj "d"
  prefix <- getStr obj "i"
  sequenceNumber <- getHexInt obj "s"
  signingThreshold <- getIntStr obj "kt"
  keys <- getStrArr obj "k"
  nextThreshold <- getIntStr obj "nt"
  nextKeys <- getStrArr obj "n"
  witnessThreshold <- getIntStr obj "bt"
  witnesses <- getStrArr obj "b"
  config <- getStrArr obj "c"
  anchors <- getJsonArr obj "a"
  pure
    { version
    , digest
    , prefix
    , sequenceNumber
    , signingThreshold
    , keys
    , nextThreshold
    , nextKeys
    , witnessThreshold
    , witnesses
    , config
    , anchors
    }

parseInteraction
  :: FO.Object Json -> Either String InteractionData
parseInteraction obj = do
  version <- getStr obj "v"
  digest <- getStr obj "d"
  prefix <- getStr obj "i"
  sequenceNumber <- getHexInt obj "s"
  priorDigest <- getStr obj "p"
  anchors <- getJsonArr obj "a"
  pure
    { version
    , digest
    , prefix
    , sequenceNumber
    , priorDigest
    , anchors
    }

parseRotation
  :: FO.Object Json -> Either String RotationData
parseRotation obj = do
  version <- getStr obj "v"
  digest <- getStr obj "d"
  prefix <- getStr obj "i"
  sequenceNumber <- getHexInt obj "s"
  priorDigest <- getStr obj "p"
  signingThreshold <- getIntStr obj "kt"
  keys <- getStrArr obj "k"
  nextThreshold <- getIntStr obj "nt"
  nextKeys <- getStrArr obj "n"
  witnessThreshold <- getIntStr obj "bt"
  witnessesAdded <- getStrArr obj "ba"
  witnessesRemoved <- getStrArr obj "br"
  config <- getStrArr obj "c"
  anchors <- getJsonArr obj "a"
  pure
    { version
    , digest
    , prefix
    , sequenceNumber
    , priorDigest
    , signingThreshold
    , keys
    , nextThreshold
    , nextKeys
    , witnessThreshold
    , witnessesAdded
    , witnessesRemoved
    , config
    , anchors
    }

-- | Get a string field from a JSON object.
getStr
  :: FO.Object Json -> String -> Either String String
getStr obj key =
  case FO.lookup key obj >>= toString of
    Just s -> Right s
    Nothing ->
      Left ("missing or invalid field: " <> key)

-- | Get a hex-encoded integer from a string field.
getHexInt
  :: FO.Object Json -> String -> Either String Int
getHexInt obj key = do
  s <- getStr obj key
  case fromStringAs hexadecimal s of
    Just n -> Right n
    Nothing ->
      Left ("invalid hex integer in field: " <> key)

-- | Get an integer from a string field (decimal).
getIntStr
  :: FO.Object Json -> String -> Either String Int
getIntStr obj key = do
  s <- getStr obj key
  case fromString s of
    Just n -> Right n
    Nothing ->
      Left ("invalid integer in field: " <> key)

-- | Get an array of strings from a JSON array field.
getStrArr
  :: FO.Object Json
  -> String
  -> Either String (Array String)
getStrArr obj key =
  case FO.lookup key obj >>= toArray of
    Just arr -> Right (mapMaybe toString arr)
    Nothing ->
      Left ("missing or invalid array field: " <> key)

-- | Get a raw Json array from a field.
getJsonArr
  :: FO.Object Json
  -> String
  -> Either String (Array Json)
getJsonArr obj key =
  case FO.lookup key obj >>= toArray of
    Just arr -> Right arr
    Nothing ->
      Left ("missing or invalid array field: " <> key)
