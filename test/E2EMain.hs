{- |
Module      : Main
Description : E2E test entry point
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Runs all E2E scenarios through the HTTP API.
-}
module Main (main) where

import E2E.KelProperties qualified
import E2E.Spec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
        testGroup
            "kel-circle-e2e"
            [ E2E.Spec.tests
            , E2E.KelProperties.tests
            ]
