{- |
Module      : KelCircle.Test.InteractionVerify
Description : Unit tests for interaction event verification
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Tests for key state extraction, interaction event
signature verification, and deterministic byte output.
-}
module KelCircle.Test.InteractionVerify (tests) where

import Data.Aeson (Value, object, toJSON, (.=))
import Data.Either (isLeft)
import Data.Text (Text)
import Data.Text qualified as T
import KelCircle.InteractionVerify
    ( VerifyResult (..)
    , buildInteractionBytes
    , verifyInteraction
    )
import KelCircle.MemberKel
    ( KelEvent (..)
    , KelKeyState (..)
    , MemberKel (..)
    , kelKeyState
    )
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519
    ( KeyPair (..)
    , generateKeyPair
    , publicKeyBytes
    , sign
    )
import Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    )
import Keri.Event.Interaction
    ( InteractionConfig (..)
    , mkInteraction
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState.PreRotation (commitKey)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
    ( assertBool
    , testCase
    , (@?=)
    )

-- | All interaction verify unit tests.
tests :: TestTree
tests =
    testGroup
        "InteractionVerify"
        [ testGroup
            "kelKeyState"
            [ testCase
                "extracts from inception-only KEL"
                testKelKeyStateInception
            , testCase
                "extracts from multi-event KEL"
                testKelKeyStateMultiEvent
            , testCase
                "fails on empty KEL"
                testKelKeyStateEmpty
            ]
        , testGroup
            "verifyInteraction"
            [ testCase
                "succeeds with valid sig"
                testVerifyValid
            , testCase
                "fails with wrong sig"
                testVerifyWrongSig
            ]
        , testGroup
            "buildInteractionBytes"
            [ testCase
                "deterministic output"
                testDeterministic
            ]
        ]

-- --------------------------------------------------------
-- Test helpers
-- --------------------------------------------------------

-- | Create a test inception KEL for a keypair.
mkTestKel :: IO (MemberKel, Text, KeyPair)
mkTestKel = do
    kp <- generateKeyPair
    let cesrKey =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes (publicKey kp)
                    }
    nextKp <- generateKeyPair
    let nextCesr =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes
                            (publicKey nextKp)
                    }
    case commitKey nextCesr of
        Left err -> error $ "commitKey: " <> err
        Right commitment -> do
            let cfg =
                    InceptionConfig
                        { icKeys = [cesrKey]
                        , icSigningThreshold = 1
                        , icNextKeys = [commitment]
                        , icNextThreshold = 1
                        , icConfig = []
                        , icAnchors = []
                        }
                evt = mkInception cfg
                evtBytes = serializeEvent evt
                sigBytes = sign kp evtBytes
                sigCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519Sig
                            , raw = sigBytes
                            }
                kel =
                    MemberKel
                        [ KelEvent
                            { keEventBytes = evtBytes
                            , keSignatures =
                                [(0, sigCesr)]
                            }
                        ]
            pure (kel, cesrKey, kp)

-- --------------------------------------------------------
-- kelKeyState tests
-- --------------------------------------------------------

testKelKeyStateInception :: IO ()
testKelKeyStateInception = do
    (kel, cesrKey, _kp) <- mkTestKel
    case kelKeyState kel of
        Left err ->
            error $ "kelKeyState failed: " <> T.unpack err
        Right kks -> do
            -- Prefix should be a SAID (starts with E)
            assertBool
                "prefix is not empty"
                (T.length (kksPrefix kks) > 0)
            kksSeqNum kks @?= 0
            assertBool
                "digest is not empty"
                (T.length (kksLastDigest kks) > 0)
            kksKeys kks @?= [cesrKey]
            kksThreshold kks @?= 1

testKelKeyStateMultiEvent :: IO ()
testKelKeyStateMultiEvent = do
    (kel@(MemberKel [icpEvt]), _cesrKey, kp) <-
        mkTestKel
    case kelKeyState kel of
        Left err ->
            error $ "kelKeyState failed: " <> T.unpack err
        Right kks0 -> do
            -- Build an interaction event
            let anchor :: Value
                anchor = object ["test" .= ("data" :: Text)]
                ixnEvt =
                    mkInteraction
                        InteractionConfig
                            { ixPrefix = kksPrefix kks0
                            , ixSequenceNumber = 1
                            , ixPriorDigest =
                                kksLastDigest kks0
                            , ixAnchors = [anchor]
                            }
                ixnBytes = serializeEvent ixnEvt
                sigBytes = sign kp ixnBytes
                sigCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519Sig
                            , raw = sigBytes
                            }
                ixnKe =
                    KelEvent
                        { keEventBytes = ixnBytes
                        , keSignatures =
                            [(0, sigCesr)]
                        }
                multiKel = MemberKel [icpEvt, ixnKe]
            case kelKeyState multiKel of
                Left err ->
                    error $
                        "kelKeyState multi failed: "
                            <> T.unpack err
                Right kks1 -> do
                    kksSeqNum kks1 @?= 1
                    -- Digest should differ from icp
                    assertBool
                        "digest changed"
                        ( kksLastDigest kks1
                            /= kksLastDigest kks0
                        )
                    -- Keys unchanged
                    kksKeys kks1 @?= kksKeys kks0

testKelKeyStateEmpty :: IO ()
testKelKeyStateEmpty =
    assertBool
        "empty KEL fails"
        (isLeft $ kelKeyState (MemberKel []))

-- --------------------------------------------------------
-- verifyInteraction tests
-- --------------------------------------------------------

testVerifyValid :: IO ()
testVerifyValid = do
    (kel, _cesrKey, kp) <- mkTestKel
    case kelKeyState kel of
        Left err ->
            error $
                "kelKeyState failed: "
                    <> T.unpack err
        Right kks -> do
            let anchor = toJSON ("hello" :: Text)
                ixnBytes =
                    buildInteractionBytes
                        (kksPrefix kks)
                        (kksSeqNum kks + 1)
                        (kksLastDigest kks)
                        anchor
                sigBytes = sign kp ixnBytes
                sigCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519Sig
                            , raw = sigBytes
                            }
            case verifyInteraction kks sigCesr anchor of
                Right (Verified _) -> pure ()
                Right (VerifyFailed reason) ->
                    error $
                        "expected Verified, got: "
                            <> T.unpack reason
                Left err ->
                    error $
                        "verifyInteraction error: "
                            <> T.unpack err

testVerifyWrongSig :: IO ()
testVerifyWrongSig = do
    (kel, _cesrKey, _kp) <- mkTestKel
    case kelKeyState kel of
        Left err ->
            error $
                "kelKeyState failed: "
                    <> T.unpack err
        Right kks -> do
            let anchor = toJSON ("hello" :: Text)
                fakeSig = T.replicate 88 "A"
            case verifyInteraction kks fakeSig anchor of
                Right (VerifyFailed _) -> pure ()
                Right (Verified _) ->
                    error "expected VerifyFailed"
                Left _ ->
                    -- Error in CESR decode is also fine
                    pure ()

-- --------------------------------------------------------
-- buildInteractionBytes tests
-- --------------------------------------------------------

testDeterministic :: IO ()
testDeterministic = do
    let prefix' = "Etest-prefix-1234"
        seqNum = 5
        digest = "Etest-digest-5678"
        anchor = toJSON ("deterministic" :: Text)
        bytes1 =
            buildInteractionBytes
                prefix'
                seqNum
                digest
                anchor
        bytes2 =
            buildInteractionBytes
                prefix'
                seqNum
                digest
                anchor
    bytes1 @?= bytes2
