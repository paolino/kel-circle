-- | AES-GCM encrypt/decrypt via Web Crypto API.
module FFI.WebCrypto
  ( encrypt
  , decrypt
  ) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)
import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Exception (Error)

foreign import encryptImpl
  :: String
  -> Uint8Array
  -> (Error -> Effect Unit)
  -> (String -> Effect Unit)
  -> Effect (Effect Unit)

foreign import decryptImpl
  :: String
  -> String
  -> (Error -> Effect Unit)
  -> (Uint8Array -> Effect Unit)
  -> Effect (Effect Unit)

-- | Encrypt bytes with a passphrase. Returns base64
-- | string of salt + iv + ciphertext.
encrypt :: String -> Uint8Array -> Aff String
encrypt passphrase plaintext = makeAff \cb -> do
  _ <- encryptImpl passphrase plaintext
    (cb <<< Left)
    (cb <<< Right)
  pure nonCanceler

-- | Decrypt a base64 string with a passphrase.
-- | Returns the original plaintext bytes.
decrypt :: String -> String -> Aff Uint8Array
decrypt passphrase ciphertext = makeAff \cb -> do
  _ <- decryptImpl passphrase ciphertext
    (cb <<< Left)
    (cb <<< Right)
  pure nonCanceler
