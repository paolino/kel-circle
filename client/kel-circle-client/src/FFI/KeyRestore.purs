-- | Restore a NaCl signing keypair from a secret key.
module FFI.KeyRestore
  ( keyPairFromSecretKey
  ) where

import Data.ArrayBuffer.Types (Uint8Array)
import Effect (Effect)
import FFI.TweetNaCl (KeyPair)

foreign import keyPairFromSecretKeyImpl
  :: Uint8Array -> Effect KeyPair

-- | Reconstruct a full Ed25519 keypair from a 64-byte
-- | secret key (nacl.sign.keyPair.fromSecretKey).
keyPairFromSecretKey :: Uint8Array -> Effect KeyPair
keyPairFromSecretKey = keyPairFromSecretKeyImpl
