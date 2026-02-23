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
    , kelEventCount
    , kelFromInception
    , parseInceptionValue
    , validateInception
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
