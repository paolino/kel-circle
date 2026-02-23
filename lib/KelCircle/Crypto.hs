{- |
Module      : KelCircle.Crypto
Description : Cryptographic helpers wrapping keri-hs
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

CESR prefix validation and re-exports of the subset of
@keri-hs@ used by kel-circle.
-}
module KelCircle.Crypto
    ( -- * CESR validation
      validateCesrPrefix
    ) where

import Data.Text (Text)
import Data.Text qualified as T
import Keri.Cesr.Decode (decode)
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Primitive (Primitive (..))

{- | Validate that a 'Text' is a well-formed
CESR-encoded Ed25519 public key prefix.

Returns @Right ()@ on success, @Left reason@ on failure.
-}
validateCesrPrefix :: Text -> Either Text ()
validateCesrPrefix txt = case decode txt of
    Left err ->
        Left $
            "CESR decode failed: " <> T.pack err
    Right prim
        | code prim /= Ed25519PubKey ->
            Left $
                "expected Ed25519 public key, got: "
                    <> T.pack (show (code prim))
        | otherwise -> Right ()
