{- |
Module      : Main
Description : kel-circle-server executable
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

HTTP server for the kel-circle trivial application.
Uses Unit types for all application parameters. The
sequencer's Ed25519 keypair is loaded from (or generated
to) a key file, and its CESR AID is used as the
sequencer identity.
-}
module Main (main) where

import Control.Concurrent.STM
    ( newBroadcastTChanIO
    )
import Control.Exception (bracket)
import Data.ByteString qualified as BS
import Data.Text (Text, pack)
import Data.Text.IO qualified as TIO
import KelCircle.Events (BaseDecision)
import KelCircle.Gate (requiresMajority)
import KelCircle.MemberKel (KelEvent (..), MemberKel (..))
import KelCircle.Server
    ( SSEMessage
    , ServerConfig (..)
    , mkApp
    )
import KelCircle.Store
    ( CircleStore
    , closeStore
    , insertMemberKel
    , openStore
    , readMemberKel
    )
import KelCircle.Types (MemberId (..))
import Keri.Cesr.DerivationCode (DerivationCode (..))
import Keri.Cesr.Encode qualified as Cesr
import Keri.Cesr.Primitive (Primitive (..))
import Keri.Crypto.Ed25519
    ( KeyPair (..)
    , generateKeyPair
    , publicKeyBytes
    , publicKeyFromBytes
    , secretKeyBytes
    , secretKeyFromBytes
    , sign
    )
import Keri.Event (eventPrefix)
import Keri.Event.Inception
    ( InceptionConfig (..)
    , mkInception
    )
import Keri.Event.Serialize (serializeEvent)
import Network.Wai.Application.Static
    ( defaultFileServerSettings
    , staticApp
    )
import Network.Wai.Handler.Warp qualified as Warp
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.IO (hFlush, stderr)

-- | Entry point: parse args and run server.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [portStr, dbPath, pass, keyFile] ->
            let port = read portStr
            in  runServer
                    port
                    dbPath
                    (pack pass)
                    keyFile
        _ ->
            putStrLn
                "Usage: kel-circle-server \
                \<port> <db> <pass> <keyfile>"

-- | Run the HTTP server with trivial application.
runServer
    :: Int -> FilePath -> Text -> FilePath -> IO ()
runServer port dbPath passphrase keyFile = do
    kp <- loadOrGenerateKeyPair keyFile
    let cesrAid =
            Cesr.encode $
                Primitive
                    { code = Ed25519PubKey
                    , raw =
                        publicKeyBytes (publicKey kp)
                    }
        sid = MemberId cesrAid
    bracket
        ( openStore
            sid
            ()
            trivialAppFold
            Just
            dbPath
        )
        closeStore
        $ \(store :: CircleStore () BaseDecision ()) -> do
            ensureSequencerKel store sid kp cesrAid
            ch <- newBroadcastTChanIO @SSEMessage
            let logger msg =
                    TIO.hPutStrLn stderr msg
                        >> hFlush stderr
                cfg =
                    ServerConfig
                        { scStore = store
                        , scAppFold = trivialAppFold
                        , scExtractDecision = Just
                        , scBaseAppGate =
                            trivialBaseGate
                        , scAppGate = trivialAppGate
                        , scProposalGate =
                            trivialProposalGate
                        , scPassphrase = passphrase
                        , scBroadcast = ch
                        , scLog = logger
                        , scSequencerKeyPair = kp
                        }
                staticDir =
                    "client/kel-circle-trivial/dist"
                fallback =
                    staticApp
                        ( defaultFileServerSettings
                            staticDir
                        )
                app = mkApp cfg (Just fallback)
            putStrLn $
                "Sequencer AID: " <> show cesrAid
            putStrLn $
                "Listening on port " <> show port
            Warp.run port app

{- | Load a keypair from file (64 bytes: 32 secret +
32 public) or generate a new one and write it.
-}
loadOrGenerateKeyPair :: FilePath -> IO KeyPair
loadOrGenerateKeyPair path = do
    exists <- doesFileExist path
    if exists
        then do
            bs <- BS.readFile path
            let (skBytes, pkBytes) =
                    BS.splitAt 32 bs
            case ( secretKeyFromBytes skBytes
                 , publicKeyFromBytes pkBytes
                 ) of
                (Right sk, Right pk) ->
                    pure
                        KeyPair
                            { secretKey = sk
                            , publicKey = pk
                            }
                _ -> error "Bad key file"
        else do
            kp <- generateKeyPair
            BS.writeFile path $
                secretKeyBytes (secretKey kp)
                    <> publicKeyBytes (publicKey kp)
            pure kp

{- | Create the sequencer's inception KEL if it
doesn't exist yet.
-}
ensureSequencerKel
    :: CircleStore () BaseDecision ()
    -> MemberId
    -> KeyPair
    -> Text
    -> IO ()
ensureSequencerKel store sid kp cesrAid = do
    mKel <- readMemberKel store sid
    case mKel of
        Just _ -> pure ()
        Nothing -> do
            let icpCfg =
                    InceptionConfig
                        { icKeys = [cesrAid]
                        , icSigningThreshold = 1
                        , icNextKeys = []
                        , icNextThreshold = 0
                        , icConfig = []
                        , icAnchors = []
                        }
                evt = mkInception icpCfg
                evtBytes = serializeEvent evt
                sigBytes = sign kp evtBytes
                sigCesr =
                    Cesr.encode $
                        Primitive
                            { code = Ed25519Sig
                            , raw = sigBytes
                            }
                kelEvt =
                    KelEvent
                        { keEventBytes = evtBytes
                        , keSignatures =
                            [(0, sigCesr)]
                        }
                kel = MemberKel [kelEvt]
            insertMemberKel store sid kel
            putStrLn $
                "Created sequencer inception KEL"
                    <> " (prefix: "
                    <> show (eventPrefix evt)
                    <> ")"

-- | Trivial app fold: Unit state, no-op.
trivialAppFold :: () -> () -> ()
trivialAppFold _ _ = ()

-- | Trivial base app gate: always passes.
trivialBaseGate :: () -> BaseDecision -> Bool
trivialBaseGate _ _ = True

-- | Trivial app gate: always passes.
trivialAppGate :: () -> () -> Bool
trivialAppGate _ _ = True

{- | Trivial proposal gate: only majority-gated
decisions are valid proposal content.
-}
trivialProposalGate :: () -> BaseDecision -> Bool
trivialProposalGate _ = requiresMajority
