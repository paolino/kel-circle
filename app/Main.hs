{- |
Module      : Main
Description : kel-circle-server executable
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

HTTP server for the kel-circle trivial application.
Uses Unit types for all application parameters.
-}
module Main (main) where

import Control.Concurrent.STM
    ( newBroadcastTChanIO
    )
import Control.Exception (bracket)
import Data.Text (Text, pack)
import Data.Text.IO qualified as TIO
import KelCircle.Events (BaseDecision)
import KelCircle.Server (ServerConfig (..), mkApp)
import KelCircle.Store
    ( CircleStore
    , closeStore
    , openStore
    )
import KelCircle.Types (MemberId (..))
import Network.Wai.Application.Static
    ( defaultFileServerSettings
    , staticApp
    )
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (getArgs)
import System.IO (hFlush, stderr)

-- | Entry point: parse args and run server.
main :: IO ()
main = do
    args <- getArgs
    case args of
        [portStr, dbPath, pass] ->
            let port = read portStr
            in  runServer port dbPath (pack pass)
        _ ->
            putStrLn
                "Usage: kel-circle-server \
                \<port> <db> <pass>"

-- | Run the HTTP server with trivial application.
runServer :: Int -> FilePath -> Text -> IO ()
runServer port dbPath passphrase =
    bracket
        (openStore sid () trivialAppFold dbPath)
        closeStore
        $ \(store :: CircleStore () () ()) -> do
            ch <- newBroadcastTChanIO
            let logger msg =
                    TIO.hPutStrLn stderr msg
                        >> hFlush stderr
                cfg =
                    ServerConfig
                        { scStore = store
                        , scAppFold = trivialAppFold
                        , scBaseAppGate =
                            trivialBaseGate
                        , scAppGate = trivialAppGate
                        , scProposalGate =
                            trivialProposalGate
                        , scPassphrase = passphrase
                        , scBroadcast = ch
                        , scLog = logger
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
                "Listening on port " <> show port
            Warp.run port app
  where
    sid = MemberId "server-sequencer"

-- | Trivial app fold: Unit state, no-op.
trivialAppFold :: () -> () -> ()
trivialAppFold _ _ = ()

-- | Trivial base app gate: always passes.
trivialBaseGate :: () -> BaseDecision -> Bool
trivialBaseGate _ _ = True

-- | Trivial app gate: always passes.
trivialAppGate :: () -> () -> Bool
trivialAppGate _ _ = True

-- | Trivial proposal gate: always passes.
trivialProposalGate :: () -> () -> Bool
trivialProposalGate _ _ = True
