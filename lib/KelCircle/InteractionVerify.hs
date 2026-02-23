{- |
Module      : KelCircle.InteractionVerify
Description : Interaction event signature verification
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Verifies that circle event submissions carry valid
KERI interaction event signatures. Each non-sequencer
submission must be signed as an interaction event
appended to the signer's KEL.
-}
module KelCircle.InteractionVerify
    ( -- * Verification
      VerifyResult (..)
    , verifyInteraction

      -- * Building interaction bytes
    , buildInteractionBytes
    ) where

import Data.Aeson (Value)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import KelCircle.MemberKel
    ( KelEvent (..)
    , KelKeyState (..)
    )
import Keri.Event.Interaction
    ( InteractionConfig (..)
    , mkInteraction
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState.Verify (verifySignatures)

-- | Result of interaction event verification.
data VerifyResult
    = -- | Signature valid; here is the new KEL event
      Verified KelEvent
    | -- | Signature verification failed
      VerifyFailed Text
    deriving stock (Show)

{- | Build interaction event bytes and verify the
signature against the signer's current key state.

On success, returns 'Verified' with the new 'KelEvent'
ready to append to the signer's KEL. On failure,
returns 'VerifyFailed' with a reason.
-}
verifyInteraction
    :: KelKeyState
    -- ^ Signer's current key state
    -> Text
    -- ^ CESR-encoded signature (subSignature)
    -> Value
    -- ^ Circle event as JSON anchor
    -> Either Text VerifyResult
verifyInteraction kks sig anchor = do
    let evtBytes =
            buildInteractionBytes
                (kksPrefix kks)
                (kksSeqNum kks + 1)
                (kksLastDigest kks)
                anchor
        sigs = [(0, sig)]
    case verifySignatures
        (kksKeys kks)
        (kksThreshold kks)
        evtBytes
        sigs of
        Left err ->
            Right . VerifyFailed $
                "signature verification error: "
                    <> T.pack err
        Right False ->
            Right $
                VerifyFailed
                    "signature threshold not met"
        Right True ->
            Right $
                Verified
                    KelEvent
                        { keEventBytes = evtBytes
                        , keSignatures = sigs
                        }

{- | Build the canonical serialized bytes for an
interaction event. Exported for test helpers that need
to construct bytes to sign.
-}
buildInteractionBytes
    :: Text
    -- ^ KERI AID prefix
    -> Int
    -- ^ Sequence number
    -> Text
    -- ^ Prior event digest
    -> Value
    -- ^ Anchor data (circle event JSON)
    -> ByteString
buildInteractionBytes prefix' seqNum priorDigest anchor =
    serializeEvent $
        mkInteraction
            InteractionConfig
                { ixPrefix = prefix'
                , ixSequenceNumber = seqNum
                , ixPriorDigest = priorDigest
                , ixAnchors = [anchor]
                }
