{- |
Module      : KelCircle.Test.Crypto
Description : Unit tests for cryptographic validation
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Tests for CESR prefix validation and future
Phase 2 cryptographic integrity functions.
-}
module KelCircle.Test.Crypto (tests) where

import Data.Either (isLeft, isRight)
import Data.Text qualified as T
import KelCircle.Crypto (validateCesrPrefix)
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Blake3 qualified as Blake3
import Keri.Crypto.Ed25519
    ( KeyPair (..)
    , generateKeyPair
    , publicKeyBytes
    )
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, testCase)

-- | All crypto unit tests.
tests :: TestTree
tests =
    testGroup
        "Crypto"
        [ cesrPrefixTests
        ]

-- --------------------------------------------------------
-- CESR prefix validation
-- --------------------------------------------------------

cesrPrefixTests :: TestTree
cesrPrefixTests =
    testGroup
        "validateCesrPrefix"
        [ testCase
            "valid Ed25519 prefix accepted"
            testValidEd25519Prefix
        , testCase
            "wrong derivation code rejected"
            testWrongDerivationCode
        , testCase
            "garbage text rejected"
            testGarbageText
        , testCase
            "empty string rejected"
            testEmptyString
        , testCase
            "truncated prefix rejected"
            testTruncatedPrefix
        ]

testValidEd25519Prefix :: IO ()
testValidEd25519Prefix = do
    kp <- generateKeyPair
    let prefix =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes
                            (publicKey kp)
                    }
    assertBool
        "valid Ed25519 CESR prefix"
        (isRight $ validateCesrPrefix prefix)

testWrongDerivationCode :: IO ()
testWrongDerivationCode = do
    -- Blake2b digest is valid CESR but not Ed25519
    let digest = Blake3.hash "some data"
        prefix =
            Cesr.encode $
                Primitive
                    { code = Blake2bDigest
                    , raw = digest
                    }
    assertBool
        "Blake2b digest rejected as member ID"
        (isLeft $ validateCesrPrefix prefix)

testGarbageText :: IO ()
testGarbageText =
    assertBool
        "garbage text rejected"
        (isLeft $ validateCesrPrefix "not-cesr")

testEmptyString :: IO ()
testEmptyString =
    assertBool
        "empty string rejected"
        (isLeft $ validateCesrPrefix "")

testTruncatedPrefix :: IO ()
testTruncatedPrefix = do
    kp <- generateKeyPair
    let prefix =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes
                            (publicKey kp)
                    }
        -- Take only half the prefix
        truncated = T.take 20 prefix
    assertBool
        "truncated prefix rejected"
        (isLeft $ validateCesrPrefix truncated)
