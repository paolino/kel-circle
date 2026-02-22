{- |
Module      : KelCircle.Store
Description : SQLite-backed global sequence store
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Persistent append-only store for the global sequence,
backed by SQLite. Each event is stored as JSON along
with the signer key and Ed25519 signature. The server
maintains a hot @FullState@ in a @TVar@ for O(1) reads.
-}
module KelCircle.Store
    ( -- * Store handle
      CircleStore (..)

      -- * Lifecycle
    , openStore
    , closeStore

      -- * Operations
    , appendCircleEvent
    , readFullState
    , readEventsFrom
    , storeLength

      -- * Stored event
    , StoredEvent (..)
    ) where

import Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Database.SQLite.Simple
    ( Connection
    , Only (..)
    , close
    , execute
    , execute_
    , open
    , query
    , query_
    )
import KelCircle.Events (CircleEvent (..))
import KelCircle.Processing
    ( FullState (..)
    , applyAppDecision
    , applyBase
    , applyProposal
    , applyResolve
    , applyResponse
    , initFullState
    )
import KelCircle.Server.JSON ()
import KelCircle.Types (MemberId (..))

{- | A stored event as returned by 'readEventsFrom'.
Contains the signer key, event JSON, and signature.
-}
data StoredEvent = StoredEvent
    { seSigner :: Text
    -- ^ Signer public key
    , seEventJson :: LBS.ByteString
    -- ^ JSON-encoded CircleEvent
    , seSignature :: Text
    -- ^ Ed25519 signature (hex or CESR)
    }
    deriving stock (Show, Eq)

-- | A handle to a SQLite-backed circle store.
data CircleStore g p r = CircleStore
    { csConn :: Connection
    -- ^ SQLite connection
    , csState :: TVar (FullState g p r)
    -- ^ Hot state updated incrementally
    , csLength :: TVar Int
    -- ^ Number of events in the store
    }

{- | Open or create a circle store at the given path.
Creates the events table on first open and replays
existing events to rebuild the in-memory state.
-}
openStore
    :: ( FromJSON d
       , FromJSON p
       , FromJSON r
       )
    => MemberId
    -- ^ Sequencer member id
    -> g
    -- ^ Initial application state
    -> (g -> d -> g)
    -- ^ Application fold function
    -> FilePath
    -> IO (CircleStore g p r)
openStore sid initApp appFold path = do
    conn <- open path
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS events \
        \( id INTEGER PRIMARY KEY AUTOINCREMENT \
        \, signer TEXT NOT NULL \
        \, event_json TEXT NOT NULL \
        \, signature TEXT NOT NULL \
        \)"
    -- Replay events to rebuild state
    rows <-
        query_
            conn
            "SELECT signer, event_json FROM events \
            \ORDER BY id"
            :: IO [(Text, LBS.ByteString)]
    let initialState = initFullState sid initApp
        finalState =
            foldl
                (replayRow appFold)
                initialState
                rows
    stVar <- newTVarIO finalState
    [Only n] <-
        query_
            conn
            "SELECT COUNT(*) FROM events"
    lVar <- newTVarIO (n :: Int)
    pure
        CircleStore
            { csConn = conn
            , csState = stVar
            , csLength = lVar
            }

-- | Close the store.
closeStore :: CircleStore g p r -> IO ()
closeStore = close . csConn

{- | Append a verified event. Persists to SQLite and
updates the in-memory state. The caller is responsible
for validation before calling this.
-}
appendCircleEvent
    :: (ToJSON d, ToJSON p, ToJSON r)
    => CircleStore g p r
    -> (g -> d -> g)
    -- ^ Application fold function
    -> Text
    -- ^ Signer key
    -> Text
    -- ^ Signature
    -> CircleEvent d p r
    -- ^ The event to store
    -> IO Int
appendCircleEvent store appFold signer sig evt = do
    let eventJson = encode evt
    execute
        (csConn store)
        "INSERT INTO events \
        \(signer, event_json, signature) \
        \VALUES (?, ?, ?)"
        (signer, eventJson, sig)
    atomically $ do
        st <- readTVar (csState store)
        writeTVar (csState store) $
            applyEvent appFold st (MemberId signer) evt
        n <- readTVar (csLength store)
        let n' = n + 1
        writeTVar (csLength store) n'
        pure n'

-- | Read current full state (from TVar, O(1)).
readFullState :: CircleStore g p r -> IO (FullState g p r)
readFullState = readTVarIO . csState

{- | Read events from index @n@ onward (1-based,
matching SQLite rowid).
-}
readEventsFrom
    :: CircleStore g p r
    -> Int
    -> IO [StoredEvent]
readEventsFrom store n = do
    rows <-
        query
            (csConn store)
            "SELECT signer, event_json, signature \
            \FROM events WHERE id >= ? ORDER BY id"
            (Only n)
            :: IO [(Text, LBS.ByteString, Text)]
    pure $ map toStoredEvent rows
  where
    toStoredEvent (s, ej, sig) =
        StoredEvent
            { seSigner = s
            , seEventJson = ej
            , seSignature = sig
            }

-- | Number of events in the store.
storeLength :: CircleStore g p r -> IO Int
storeLength = readTVarIO . csLength

-- --------------------------------------------------------
-- Internal helpers
-- --------------------------------------------------------

-- | Apply a circle event to the full state.
applyEvent
    :: (g -> d -> g)
    -> FullState g p r
    -> MemberId
    -> CircleEvent d p r
    -> FullState g p r
applyEvent appFold st signer = \case
    CEBaseDecision bd -> applyBase st bd
    CEAppDecision d ->
        applyAppDecision st d appFold
    CEProposal content deadline ->
        applyProposal st content signer deadline
    CEResponse content pid ->
        applyResponse st content signer pid
    CEResolveProposal pid res ->
        applyResolve st pid res

{- | Replay a single row from the database into the
full state.
-}
replayRow
    :: (FromJSON d, FromJSON p, FromJSON r)
    => (g -> d -> g)
    -> FullState g p r
    -> (Text, LBS.ByteString)
    -> FullState g p r
replayRow appFold st (signer, eventJson) =
    case decode eventJson of
        Just evt ->
            applyEvent
                appFold
                st
                (MemberId signer)
                evt
        Nothing -> st
