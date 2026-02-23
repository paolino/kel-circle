{- |
Module      : Main
Description : Test entry point
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Runs all property tests mirroring the Lean
preservation theorems.
-}
module Main (main) where

import KelCircle.Test.BaseDecisions qualified
import KelCircle.Test.Crypto qualified
import KelCircle.Test.Gate qualified
import KelCircle.Test.InteractionVerify qualified
import KelCircle.Test.MemberKel qualified
import KelCircle.Test.Processing qualified
import KelCircle.Test.Proposals qualified
import KelCircle.Test.Sequence qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "kel-circle"
            [ KelCircle.Test.Sequence.tests
            , KelCircle.Test.BaseDecisions.tests
            , KelCircle.Test.Gate.tests
            , KelCircle.Test.Proposals.tests
            , KelCircle.Test.Processing.tests
            , KelCircle.Test.Crypto.tests
            , KelCircle.Test.MemberKel.tests
            , KelCircle.Test.InteractionVerify.tests
            ]
