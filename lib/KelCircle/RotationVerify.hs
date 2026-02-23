{- |
Module      : KelCircle.RotationVerify
Description : Rotation event signature verification
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Verifies and constructs KERI rotation events for key
rotation. Rotation events are signed by the OLD keys
and reveal new keys that must match pre-rotation
commitments.
-}
module KelCircle.RotationVerify
    ( -- * Verification
      RotationResult (..)
    , verifyRotation

      -- * Building rotation bytes
    , buildRotationBytes
    , rotationEventDigest
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import KelCircle.MemberKel
    ( KelEvent (..)
    , KelKeyState (..)
    , validateRotationCommitment
    )
import Keri.Event (eventDigest)
import Keri.Event.Rotation
    ( RotationConfig (..)
    , mkRotation
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState.Verify (verifySignatures)

-- | Result of rotation event verification.
data RotationResult
    = -- | Rotation valid; here is the new KEL event
      RotationVerified KelEvent
    | -- | Rotation verification failed
      RotationFailed Text
    deriving stock (Show)

{- | Verify a rotation event against the signer's
current key state.

Checks:
1. New keys match pre-rotation commitments
2. Signatures (by OLD keys) verify over rotation bytes
3. Signature threshold is met

On success, returns 'RotationVerified' with the new
'KelEvent'. On failure, returns 'RotationFailed'.
-}
verifyRotation
    :: KelKeyState
    -- ^ Current key state
    -> [Text]
    -- ^ New signing keys (CESR-encoded)
    -> Int
    -- ^ New signing threshold
    -> [Text]
    -- ^ New next-key commitments
    -> Int
    -- ^ New next threshold
    -> [(Int, Text)]
    -- ^ Signatures (by OLD keys over rotation bytes)
    -> Either Text RotationResult
verifyRotation
    kks
    newKeys
    newThreshold
    newNextKeys
    newNextThreshold
    sigs = do
        -- 1. Validate pre-rotation commitments
        case validateRotationCommitment
            (kksNextKeys kks)
            newKeys of
            Left err ->
                Right $
                    RotationFailed $
                        "commitment check: " <> err
            Right () -> do
                -- 2. Build rotation event bytes
                let rotBytes =
                        buildRotationBytes
                            (kksPrefix kks)
                            (kksSeqNum kks + 1)
                            (kksLastDigest kks)
                            newKeys
                            newThreshold
                            newNextKeys
                            newNextThreshold
                -- 3. Verify signatures (OLD keys sign)
                case verifySignatures
                    (kksKeys kks)
                    (kksThreshold kks)
                    rotBytes
                    sigs of
                    Left err ->
                        Right . RotationFailed $
                            "signature verification: "
                                <> T.pack err
                    Right False ->
                        Right $
                            RotationFailed
                                "signature threshold \
                                \not met"
                    Right True ->
                        Right $
                            RotationVerified
                                KelEvent
                                    { keEventBytes =
                                        rotBytes
                                    , keSignatures = sigs
                                    }

{- | Build the canonical serialized bytes for a
rotation event. Exported for test helpers that need
to construct bytes to sign.
-}
buildRotationBytes
    :: Text
    -- ^ KERI AID prefix
    -> Int
    -- ^ Sequence number
    -> Text
    -- ^ Prior event digest
    -> [Text]
    -- ^ New signing keys
    -> Int
    -- ^ New signing threshold
    -> [Text]
    -- ^ New next-key commitments
    -> Int
    -- ^ New next threshold
    -> ByteString
buildRotationBytes
    prefix'
    seqNum
    priorDigest
    newKeys
    newThreshold
    newNextKeys
    newNextThreshold =
        serializeEvent $
            mkRotation
                RotationConfig
                    { rcPrefix = prefix'
                    , rcSequenceNumber = seqNum
                    , rcPriorDigest = priorDigest
                    , rcKeys = newKeys
                    , rcSigningThreshold =
                        newThreshold
                    , rcNextKeys = newNextKeys
                    , rcNextThreshold =
                        newNextThreshold
                    , rcConfig = []
                    , rcAnchors = []
                    }

{- | Extract the digest from the rotation event
built with given parameters. Used internally by
test helpers to track KEL state after rotation.
-}
rotationEventDigest
    :: Text
    -> Int
    -> Text
    -> [Text]
    -> Int
    -> [Text]
    -> Int
    -> Text
rotationEventDigest
    prefix'
    seqNum
    priorDigest
    newKeys
    newThreshold
    newNextKeys
    newNextThreshold =
        eventDigest $
            mkRotation
                RotationConfig
                    { rcPrefix = prefix'
                    , rcSequenceNumber = seqNum
                    , rcPriorDigest = priorDigest
                    , rcKeys = newKeys
                    , rcSigningThreshold =
                        newThreshold
                    , rcNextKeys = newNextKeys
                    , rcNextThreshold =
                        newNextThreshold
                    , rcConfig = []
                    , rcAnchors = []
                    }
