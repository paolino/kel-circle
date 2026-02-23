{- |
Module      : KelCircle.Test.MemberKel
Description : Unit tests for inception validation
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Tests for CESR inception validation, including
valid inception, wrong prefix, bad signature, and
non-inception event rejection.
-}
module KelCircle.Test.MemberKel (tests) where

import Data.Aeson (Value, object, (.=))
import Data.Either (isLeft, isRight)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import KelCircle.MemberKel
    ( InceptionSubmission (..)
    , KelEvent (..)
    , KelKeyState (..)
    , MemberKel (..)
    , kelEventCount
    , kelFromInception
    , kelKeyState
    , parseInceptionValue
    , validateInception
    , validateRotationCommitment
    )
import KelCircle.Types (MemberId (..))
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
import Keri.Event.Rotation
    ( RotationConfig (..)
    , mkRotation
    )
import Keri.Event.Serialize (serializeEvent)
import Keri.KeyState.PreRotation (commitKey)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase, (@?=))

-- | All member KEL unit tests.
tests :: TestTree
tests =
    testGroup
        "MemberKel"
        [ testValidateInception
        , testKelFromInception
        , testRotatedKeyState
        , testRotationCommitment
        ]

-- --------------------------------------------------------
-- Test helpers
-- --------------------------------------------------------

-- | Create a proper inception submission for a keypair.
mkTestInception
    :: KeyPair -> IO (InceptionSubmission, Text)
mkTestInception kp = do
    let cesrKey =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes (publicKey kp)
                    }
    -- Next key for pre-rotation
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
            pure
                ( InceptionSubmission
                    { isEventText =
                        TE.decodeUtf8 evtBytes
                    , isSignatures = [(0, sigCesr)]
                    }
                , cesrKey
                )

-- --------------------------------------------------------
-- validateInception tests
-- --------------------------------------------------------

testValidateInception :: TestTree
testValidateInception =
    testGroup
        "validateInception"
        [ testCase
            "accepts valid inception"
            testValidInception
        , testCase
            "rejects wrong prefix"
            testWrongPrefix
        , testCase
            "rejects bad signature"
            testBadSignature
        , testCase
            "parseInceptionValue roundtrip"
            testParseInceptionValue
        ]

testValidInception :: IO ()
testValidInception = do
    kp <- generateKeyPair
    (inception, prefix) <- mkTestInception kp
    let mid = MemberId prefix
    assertBool
        "valid inception should be accepted"
        ( isRight $
            validateInception mid inception
        )

testWrongPrefix :: IO ()
testWrongPrefix = do
    kp <- generateKeyPair
    (inception, _prefix) <- mkTestInception kp
    -- Use a different prefix
    other <- generateKeyPair
    let otherCesr =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes
                            (publicKey other)
                    }
        wrongMid = MemberId otherCesr
    assertBool
        "wrong prefix should be rejected"
        ( isLeft $
            validateInception wrongMid inception
        )

testBadSignature :: IO ()
testBadSignature = do
    kp <- generateKeyPair
    (inception, prefix) <- mkTestInception kp
    -- Corrupt the signature
    let badSigs =
            [(0, T.replicate 88 "A")]
        badInception =
            inception{isSignatures = badSigs}
        mid = MemberId prefix
    assertBool
        "bad signature should be rejected"
        ( isLeft $
            validateInception mid badInception
        )

testParseInceptionValue :: IO ()
testParseInceptionValue = do
    kp <- generateKeyPair
    (inception, _prefix) <- mkTestInception kp
    let val :: Value
        val =
            object
                [ "event"
                    .= isEventText inception
                , "signatures"
                    .= isSignatures inception
                ]
    assertBool
        "parseInceptionValue should succeed"
        (isRight $ parseInceptionValue val)

-- --------------------------------------------------------
-- kelFromInception tests
-- --------------------------------------------------------

testKelFromInception :: TestTree
testKelFromInception =
    testGroup
        "kelFromInception"
        [ testCase
            "creates KEL with 1 event"
            testKelCreation
        ]

testKelCreation :: IO ()
testKelCreation = do
    kp <- generateKeyPair
    (inception, _prefix) <- mkTestInception kp
    let kel = kelFromInception inception
    kelEventCount kel @?= 1

-- --------------------------------------------------------
-- Key state after rotation tests
-- --------------------------------------------------------

testRotatedKeyState :: TestTree
testRotatedKeyState =
    testGroup
        "kelKeyState with rotation"
        [ testCase
            "returns new keys after rotation"
            testKeyStateAfterRotation
        , testCase
            "returns rotated keys after ixn"
            testKeyStateAfterRotationAndIxn
        ]

{- | Helper: create inception KEL and return everything
needed for rotation tests.
-}
mkTestKelForRotation
    :: IO (MemberKel, Text, KeyPair, KeyPair, Text)
    -- ^ (kel, cesrKey, kp, nextKp, nextCommitment)
mkTestKelForRotation = do
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
            pure
                (kel, cesrKey, kp, nextKp, nextCesr)

{- | Build a rotation KEL event. The rotation is
signed by the OLD keypair over the canonical bytes.
-}
mkRotationKelEvent
    :: KeyPair
    -- ^ OLD keypair (signs the rotation)
    -> KelKeyState
    -- ^ Current key state
    -> Text
    -- ^ New key (CESR-encoded)
    -> Text
    -- ^ New next-key commitment
    -> KelEvent
mkRotationKelEvent oldKp kks newKeyCesr commitment =
    let rotEvt =
            mkRotation
                RotationConfig
                    { rcPrefix = kksPrefix kks
                    , rcSequenceNumber =
                        kksSeqNum kks + 1
                    , rcPriorDigest =
                        kksLastDigest kks
                    , rcKeys = [newKeyCesr]
                    , rcSigningThreshold = 1
                    , rcNextKeys = [commitment]
                    , rcNextThreshold = 1
                    , rcConfig = []
                    , rcAnchors = []
                    }
        rotBytes = serializeEvent rotEvt
        sigBytes = sign oldKp rotBytes
        sigCesr =
            Cesr.encode $
                Primitive
                    { code = Ed25519Sig
                    , raw = sigBytes
                    }
    in  KelEvent
            { keEventBytes = rotBytes
            , keSignatures = [(0, sigCesr)]
            }

testKeyStateAfterRotation :: IO ()
testKeyStateAfterRotation = do
    ( kel@(MemberKel [icpEvt])
        , _cesrKey
        , kp
        , _nextKp
        , nextCesr
        ) <-
        mkTestKelForRotation
    case kelKeyState kel of
        Left err ->
            error $
                "kelKeyState failed: "
                    <> T.unpack err
        Right kks0 -> do
            -- Build next-next commitment
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
                    let rotEvt =
                            mkRotationKelEvent
                                kp
                                kks0
                                nextCesr
                                commitment2
                        rotKel =
                            MemberKel
                                [icpEvt, rotEvt]
                    case kelKeyState rotKel of
                        Left err ->
                            error $
                                "kelKeyState after rot: "
                                    <> T.unpack err
                        Right kks1 -> do
                            kksKeys kks1
                                @?= [nextCesr]
                            kksThreshold kks1 @?= 1
                            kksSeqNum kks1 @?= 1

testKeyStateAfterRotationAndIxn :: IO ()
testKeyStateAfterRotationAndIxn = do
    ( kel@(MemberKel [icpEvt])
        , _cesrKey
        , kp
        , nextKp
        , nextCesr
        ) <-
        mkTestKelForRotation
    case kelKeyState kel of
        Left err ->
            error $
                "kelKeyState failed: "
                    <> T.unpack err
        Right kks0 -> do
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
                    let rotEvt =
                            mkRotationKelEvent
                                kp
                                kks0
                                nextCesr
                                commitment2
                        rotKel =
                            MemberKel
                                [icpEvt, rotEvt]
                    case kelKeyState rotKel of
                        Left err ->
                            error $
                                "after rot: "
                                    <> T.unpack err
                        Right kks1 -> do
                            -- Append an ixn with new key
                            let anchor =
                                    object
                                        [ "test"
                                            .= ( "data"
                                                    :: Text
                                               )
                                        ]
                                ixnEvt =
                                    mkInteraction
                                        InteractionConfig
                                            { ixPrefix =
                                                kksPrefix
                                                    kks1
                                            , ixSequenceNumber =
                                                kksSeqNum kks1
                                                    + 1
                                            , ixPriorDigest =
                                                kksLastDigest
                                                    kks1
                                            , ixAnchors =
                                                [anchor]
                                            }
                                ixnBytes =
                                    serializeEvent ixnEvt
                                sigBytes =
                                    sign nextKp ixnBytes
                                sigCesr =
                                    Cesr.encode $
                                        Primitive
                                            { code =
                                                Ed25519Sig
                                            , raw = sigBytes
                                            }
                                ixnKe =
                                    KelEvent
                                        { keEventBytes =
                                            ixnBytes
                                        , keSignatures =
                                            [(0, sigCesr)]
                                        }
                                fullKel =
                                    MemberKel
                                        [ icpEvt
                                        , rotEvt
                                        , ixnKe
                                        ]
                            case kelKeyState fullKel of
                                Left err ->
                                    error $
                                        "after ixn: "
                                            <> T.unpack err
                                Right kks2 -> do
                                    -- Keys still rotated
                                    kksKeys kks2
                                        @?= [nextCesr]
                                    kksSeqNum kks2
                                        @?= 2

-- --------------------------------------------------------
-- Rotation commitment tests
-- --------------------------------------------------------

testRotationCommitment :: TestTree
testRotationCommitment =
    testGroup
        "validateRotationCommitment"
        [ testCase
            "matching commitment succeeds"
            testCommitmentMatch
        , testCase
            "mismatched commitment fails"
            testCommitmentMismatch
        ]

testCommitmentMatch :: IO ()
testCommitmentMatch = do
    kp <- generateKeyPair
    let cesrKey =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes (publicKey kp)
                    }
    case commitKey cesrKey of
        Left err ->
            error $ "commitKey: " <> err
        Right commitment ->
            assertBool
                "matching commitment should succeed"
                ( isRight $
                    validateRotationCommitment
                        [commitment]
                        [cesrKey]
                )

testCommitmentMismatch :: IO ()
testCommitmentMismatch = do
    kp1 <- generateKeyPair
    kp2 <- generateKeyPair
    let key1 =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes (publicKey kp1)
                    }
        key2 =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes (publicKey kp2)
                    }
    case commitKey key1 of
        Left err ->
            error $ "commitKey: " <> err
        Right commitment1 ->
            -- commitment1 is for key1, but we provide key2
            assertBool
                "mismatched commitment should fail"
                ( isLeft $
                    validateRotationCommitment
                        [commitment1]
                        [key2]
                )
