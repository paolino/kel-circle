{- |
Module      : KelCircle.Test.RotationVerify
Description : Unit tests for rotation event verification
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Tests for rotation event construction and signature
verification.
-}
module KelCircle.Test.RotationVerify (tests) where

import Data.Text (Text)
import Data.Text qualified as T
import KelCircle.MemberKel
    ( KelEvent (..)
    , KelKeyState (..)
    , MemberKel (..)
    , kelKeyState
    )
import KelCircle.RotationVerify
    ( RotationResult (..)
    , buildRotationBytes
    , verifyRotation
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
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState.PreRotation (commitKey)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- | All rotation verify unit tests.
tests :: TestTree
tests =
    testGroup
        "RotationVerify"
        [ testGroup
            "verifyRotation"
            [ testCase
                "succeeds with valid rotation"
                testVerifyValid
            , testCase
                "fails with wrong commitment"
                testVerifyWrongCommitment
            , testCase
                "fails with wrong signature"
                testVerifyWrongSig
            ]
        , testGroup
            "buildRotationBytes"
            [ testCase
                "deterministic output"
                testDeterministic
            ]
        ]

-- --------------------------------------------------------
-- Test helpers
-- --------------------------------------------------------

-- | Create inception KEL for rotation tests.
mkTestKel :: IO (MemberKel, Text, KeyPair, KeyPair)
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
            pure (kel, cesrKey, kp, nextKp)

-- --------------------------------------------------------
-- verifyRotation tests
-- --------------------------------------------------------

testVerifyValid :: IO ()
testVerifyValid = do
    (kel, _cesrKey, kp, nextKp) <- mkTestKel
    case kelKeyState kel of
        Left err ->
            error $
                "kelKeyState failed: "
                    <> T.unpack err
        Right kks -> do
            -- New key is the pre-committed one
            let newCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519PubKey
                            , raw =
                                publicKeyBytes
                                    (publicKey nextKp)
                            }
            -- Generate next-next commitment
            nextNextKp <- generateKeyPair
            let nextNextCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519PubKey
                            , raw =
                                publicKeyBytes
                                    ( publicKey
                                        nextNextKp
                                    )
                            }
            case commitKey nextNextCesr of
                Left err ->
                    error $
                        "commitKey: " <> err
                Right commitment2 -> do
                    -- Build rotation bytes and sign
                    let rotBytes =
                            buildRotationBytes
                                (kksPrefix kks)
                                (kksSeqNum kks + 1)
                                (kksLastDigest kks)
                                [newCesr]
                                1
                                [commitment2]
                                1
                        sigBytes =
                            sign kp rotBytes
                        sigCesr =
                            Cesr.encode $
                                Primitive
                                    { code = Ed25519Sig
                                    , raw = sigBytes
                                    }
                    case verifyRotation
                        kks
                        [newCesr]
                        1
                        [commitment2]
                        1
                        [(0, sigCesr)] of
                        Right (RotationVerified _) ->
                            pure ()
                        Right (RotationFailed reason) ->
                            error $
                                "expected Verified: "
                                    <> T.unpack reason
                        Left err ->
                            error $
                                "verifyRotation err: "
                                    <> T.unpack err

testVerifyWrongCommitment :: IO ()
testVerifyWrongCommitment = do
    (kel, _cesrKey, kp, _nextKp) <- mkTestKel
    case kelKeyState kel of
        Left err ->
            error $
                "kelKeyState failed: "
                    <> T.unpack err
        Right kks -> do
            -- Use a random key (not the committed one)
            wrongKp <- generateKeyPair
            let wrongCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519PubKey
                            , raw =
                                publicKeyBytes
                                    (publicKey wrongKp)
                            }
            nextNextKp <- generateKeyPair
            let nextNextCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519PubKey
                            , raw =
                                publicKeyBytes
                                    ( publicKey
                                        nextNextKp
                                    )
                            }
            case commitKey nextNextCesr of
                Left err ->
                    error $
                        "commitKey: " <> err
                Right commitment2 -> do
                    let rotBytes =
                            buildRotationBytes
                                (kksPrefix kks)
                                (kksSeqNum kks + 1)
                                (kksLastDigest kks)
                                [wrongCesr]
                                1
                                [commitment2]
                                1
                        sigBytes =
                            sign kp rotBytes
                        sigCesr =
                            Cesr.encode $
                                Primitive
                                    { code = Ed25519Sig
                                    , raw = sigBytes
                                    }
                    case verifyRotation
                        kks
                        [wrongCesr]
                        1
                        [commitment2]
                        1
                        [(0, sigCesr)] of
                        Right (RotationFailed _) ->
                            pure ()
                        other ->
                            error $
                                "expected RotationFailed"
                                    <> ", got: "
                                    <> show other

testVerifyWrongSig :: IO ()
testVerifyWrongSig = do
    (kel, _cesrKey, _kp, nextKp) <- mkTestKel
    case kelKeyState kel of
        Left err ->
            error $
                "kelKeyState failed: "
                    <> T.unpack err
        Right kks -> do
            let newCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519PubKey
                            , raw =
                                publicKeyBytes
                                    (publicKey nextKp)
                            }
            nextNextKp <- generateKeyPair
            let nextNextCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519PubKey
                            , raw =
                                publicKeyBytes
                                    ( publicKey
                                        nextNextKp
                                    )
                            }
            case commitKey nextNextCesr of
                Left err ->
                    error $
                        "commitKey: " <> err
                Right commitment2 -> do
                    -- Sign with WRONG key (nextKp)
                    let rotBytes =
                            buildRotationBytes
                                (kksPrefix kks)
                                (kksSeqNum kks + 1)
                                (kksLastDigest kks)
                                [newCesr]
                                1
                                [commitment2]
                                1
                        sigBytes =
                            sign nextKp rotBytes
                        sigCesr =
                            Cesr.encode $
                                Primitive
                                    { code = Ed25519Sig
                                    , raw = sigBytes
                                    }
                    case verifyRotation
                        kks
                        [newCesr]
                        1
                        [commitment2]
                        1
                        [(0, sigCesr)] of
                        Right (RotationFailed _) ->
                            pure ()
                        other ->
                            error $
                                "expected RotationFailed"
                                    <> ", got: "
                                    <> show other

-- --------------------------------------------------------
-- buildRotationBytes tests
-- --------------------------------------------------------

testDeterministic :: IO ()
testDeterministic = do
    let prefix' = "Etest-prefix-1234"
        seqNum = 5
        digest = "Etest-digest-5678"
        keys = ["Dkey-abc"]
        threshold = 1
        nextKeys = ["Ecommit-xyz"]
        nextThreshold = 1
        bytes1 =
            buildRotationBytes
                prefix'
                seqNum
                digest
                keys
                threshold
                nextKeys
                nextThreshold
        bytes2 =
            buildRotationBytes
                prefix'
                seqNum
                digest
                keys
                threshold
                nextKeys
                nextThreshold
    bytes1 @?= bytes2
