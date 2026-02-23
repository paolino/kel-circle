{- |
Module      : KelCircle.MemberKel
Description : Per-member KEL storage and inception validation
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Validates inception events for new circle members and
manages per-member Key Event Logs. Each introduced member
must provide a signed inception event at introduction time.
-}
module KelCircle.MemberKel
    ( -- * KEL types
      MemberKel (..)
    , KelEvent (..)
    , kelEventCount

      -- * Inception parsing
    , InceptionSubmission (..)
    , parseInceptionValue

      -- * Validation
    , validateInception

      -- * KEL construction
    , kelFromInception
    ) where

import Data.Aeson
    ( FromJSON (..)
    , ToJSON (..)
    , Value (..)
    , decodeStrict
    , object
    , withObject
    , (.:)
    , (.=)
    )
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Vector qualified as V
import KelCircle.Types (MemberId (..))
import Keri.Cesr.Decode qualified as Cesr
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Primitive (Primitive (..))
import Keri.KeyState.Verify (verifySignatures)

-- --------------------------------------------------------
-- KEL types
-- --------------------------------------------------------

-- | A single stored KEL event.
data KelEvent = KelEvent
    { keEventBytes :: ByteString
    -- ^ Canonical KERI JSON serialization
    , keSignatures :: [(Int, Text)]
    -- ^ Indexed CESR-encoded signatures
    }
    deriving stock (Show, Eq)

-- | A member's Key Event Log.
newtype MemberKel = MemberKel
    { mkelEvents :: [KelEvent]
    }
    deriving stock (Show, Eq)

-- | Number of events in a member's KEL.
kelEventCount :: MemberKel -> Int
kelEventCount = length . mkelEvents

-- --------------------------------------------------------
-- Inception submission parsing
-- --------------------------------------------------------

{- | Parsed inception submission from the client.
The event is the canonical KERI JSON as a text string,
and signatures are indexed CESR-encoded Ed25519 signatures.
-}
data InceptionSubmission = InceptionSubmission
    { isEventText :: Text
    -- ^ Canonical KERI event JSON as string
    , isSignatures :: [(Int, Text)]
    -- ^ Indexed CESR signatures
    }
    deriving stock (Show, Eq)

instance FromJSON InceptionSubmission where
    parseJSON = withObject "InceptionSubmission" $ \o ->
        InceptionSubmission
            <$> o .: "event"
            <*> o .: "signatures"

instance ToJSON InceptionSubmission where
    toJSON is =
        object
            [ "event" .= isEventText is
            , "signatures" .= isSignatures is
            ]

{- | Parse an inception 'Value' into an
'InceptionSubmission'.
-}
parseInceptionValue
    :: Value -> Either Text InceptionSubmission
parseInceptionValue val =
    case Aeson.fromJSON val of
        Aeson.Success is -> Right is
        Aeson.Error err -> Left $ T.pack err

-- --------------------------------------------------------
-- Validation
-- --------------------------------------------------------

{- | Validate a parsed inception event for a member.

Checks:

1. Event bytes are valid JSON
2. Event type is @\"icp\"@ (inception)
3. Sequence number (@\"s\"@ field) is @\"0\"@
4. Member key is present in inception signing keys
5. Signatures verify against the event keys

Returns @Right ()@ on success, @Left reason@ on failure.
-}
validateInception
    :: MemberId
    -> InceptionSubmission
    -> Either Text ()
validateInception
    (MemberId memberKey)
    (InceptionSubmission eventText sigs) = do
        let eventBytes = TE.encodeUtf8 eventText
        obj <- parseEventJson eventBytes
        validateEventType obj
        validateSeqNum obj
        keys <- extractKeys obj
        validateKeyPresent keys memberKey
        threshold <- extractThreshold obj
        validateSignatureCesr sigs
        verifySigs keys threshold eventBytes sigs

-- --------------------------------------------------------
-- Internal validation helpers
-- --------------------------------------------------------

-- | Parse event bytes as a JSON object.
parseEventJson :: ByteString -> Either Text Value
parseEventJson bs =
    case decodeStrict bs of
        Just v@(Object _) -> Right v
        Just _ -> Left "event is not a JSON object"
        Nothing -> Left "event is not valid JSON"

-- | Check event type is \"icp\".
validateEventType :: Value -> Either Text ()
validateEventType val =
    case lookupField "t" val of
        Just (String "icp") -> Right ()
        Just (String t) ->
            Left $
                "expected event type icp, got: "
                    <> t
        _ -> Left "missing or invalid event type"

-- | Check member key is in the inception keys.
validateKeyPresent
    :: [Text] -> Text -> Either Text ()
validateKeyPresent keys memberKey
    | memberKey `elem` keys = Right ()
    | otherwise =
        Left "member key not in inception keys"

-- | Check sequence number is \"0\".
validateSeqNum :: Value -> Either Text ()
validateSeqNum val =
    case lookupField "s" val of
        Just (String "0") -> Right ()
        Just (String s) ->
            Left $
                "expected sequence 0, got: " <> s
        _ ->
            Left "missing or invalid sequence number"

-- | Validate that all signatures are valid CESR.
validateSignatureCesr
    :: [(Int, Text)] -> Either Text ()
validateSignatureCesr sigs
    | null sigs = Left "no signatures provided"
    | otherwise = mapM_ checkSig sigs
  where
    checkSig (_, sigText) =
        case Cesr.decode sigText of
            Left err ->
                Left $
                    "invalid CESR signature: "
                        <> T.pack err
            Right prim
                | code prim /= Ed25519Sig ->
                    Left
                        "signature is not Ed25519"
                | BS.length (raw prim) /= 64 ->
                    Left
                        "signature has wrong length"
                | otherwise -> Right ()

-- | Extract signing keys from the event.
extractKeys :: Value -> Either Text [Text]
extractKeys val =
    case lookupField "k" val of
        Just (Array arr) ->
            Right [t | String t <- V.toList arr]
        _ -> Left "missing or invalid keys field"

-- | Extract signing threshold from the event.
extractThreshold :: Value -> Either Text Int
extractThreshold val =
    case lookupField "kt" val of
        Just (String t) ->
            case reads (T.unpack t) of
                [(n, "")] -> Right n
                _ ->
                    Left $
                        "invalid threshold: " <> t
        Just (Number n) -> Right (round n)
        _ ->
            Left "missing or invalid threshold field"

-- | Verify signatures using keri-hs.
verifySigs
    :: [Text]
    -> Int
    -> ByteString
    -> [(Int, Text)]
    -> Either Text ()
verifySigs keys threshold eventBytes sigs =
    case verifySignatures
        keys
        threshold
        eventBytes
        sigs of
        Left err ->
            Left $
                "signature verification failed: "
                    <> T.pack err
        Right False ->
            Left "signature threshold not met"
        Right True -> Right ()

-- --------------------------------------------------------
-- KEL construction
-- --------------------------------------------------------

{- | Create a 'MemberKel' from a validated inception.
Call 'validateInception' first.
-}
kelFromInception
    :: InceptionSubmission -> MemberKel
kelFromInception (InceptionSubmission eventText sigs) =
    MemberKel
        { mkelEvents =
            [ KelEvent
                { keEventBytes =
                    TE.encodeUtf8 eventText
                , keSignatures = sigs
                }
            ]
        }

-- --------------------------------------------------------
-- JSON field lookup helper
-- --------------------------------------------------------

-- | Look up a field in a JSON object by text key.
lookupField :: Text -> Value -> Maybe Value
lookupField key (Object o) =
    KM.lookup (Key.fromText key) o
lookupField _ _ = Nothing
