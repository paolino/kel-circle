{- |
Module      : KelCircle.Store
Description : SQLite-backed global sequence store
Copyright   : (c) 2026 Paolo Veronelli
License     : Apache-2.0

Persistent append-only store for the global sequence,
backed by SQLite. Each event is stored as JSON along
with the signer key and Ed25519 signature. The server
maintains a hot @FullState@ in a @TVar@ for O(1) reads.

Per-member KELs are stored in a separate @member_kels@
table and rebuilt on replay.
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
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
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
import KelCircle.Events
    ( BaseDecision (..)
    , CircleEvent (..)
    )
import KelCircle.MemberKel
    ( KelEvent (..)
    , MemberKel (..)
    )
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
Creates the events and member_kels tables on first open
and replays existing events to rebuild the in-memory state.
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
    -- Create events table
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS events \
        \( id INTEGER PRIMARY KEY AUTOINCREMENT \
        \, signer TEXT NOT NULL \
        \, event_json TEXT NOT NULL \
        \, signature TEXT NOT NULL \
        \)"
    -- Create member KELs table
    execute_
        conn
        "CREATE TABLE IF NOT EXISTS member_kels \
        \( member_id TEXT NOT NULL \
        \, seq_num INTEGER NOT NULL \
        \, event_json TEXT NOT NULL \
        \, signatures_json TEXT NOT NULL \
        \, PRIMARY KEY (member_id, seq_num) \
        \)"
    -- Replay events to rebuild state
    rows <-
        query_
            conn
            "SELECT signer, event_json FROM events \
            \ORDER BY id"
            :: IO [(Text, LBS.ByteString)]
    let initialState = initFullState sid initApp
        circleState =
            foldl
                (replayRow appFold)
                initialState
                rows
    -- Replay member KELs
    kelRows <-
        query_
            conn
            "SELECT member_id, seq_num, event_json \
            \, signatures_json \
            \FROM member_kels ORDER BY member_id \
            \, seq_num"
            :: IO [(Text, Int, Text, LBS.ByteString)]
    let kels = replayKelRows kelRows
        finalState =
            circleState{fsMemberKels = kels}
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

When the event is 'IntroduceMember', the optional
'MemberKel' is inserted into the member_kels table.
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
    -> Maybe (MemberId, MemberKel)
    -- ^ Optional: member KEL for IntroduceMember
    -> IO Int
appendCircleEvent store appFold signer sig evt mKel =
    do
        let eventJson = encode evt
        -- Persist circle event
        execute
            (csConn store)
            "INSERT INTO events \
            \(signer, event_json, signature) \
            \VALUES (?, ?, ?)"
            (signer, eventJson, sig)
        -- Persist member KEL if provided
        case mKel of
            Just (mid, kel) ->
                storeMemberKelEvents
                    (csConn store)
                    mid
                    kel
            Nothing -> pure ()
        -- Delete KEL on member removal
        case evt of
            CEBaseDecision (RemoveMember mid) ->
                deleteMemberKel (csConn store) mid
            _ -> pure ()
        -- Update in-memory state
        atomically $ do
            st <- readTVar (csState store)
            let st' =
                    applyEvent
                        appFold
                        st
                        (MemberId signer)
                        evt
                st'' = updateKels evt mKel st'
            writeTVar (csState store) st''
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
    toStoredEvent (s, ej, sig') =
        StoredEvent
            { seSigner = s
            , seEventJson = ej
            , seSignature = sig'
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

-- | Rebuild member KELs from database rows.
replayKelRows
    :: [(Text, Int, Text, LBS.ByteString)]
    -> Map.Map MemberId MemberKel
replayKelRows = foldl addKelRow Map.empty
  where
    addKelRow acc (midText, _seqNum, evtJson, sigsJson) =
        let mid = MemberId midText
            evtBytes = TE.encodeUtf8 evtJson
            sigs = fromMaybe [] (decode sigsJson)
            ke =
                KelEvent
                    { keEventBytes = evtBytes
                    , keSignatures = sigs
                    }
            existing =
                Map.findWithDefault
                    (MemberKel [])
                    mid
                    acc
            updated =
                MemberKel
                    (mkelEvents existing ++ [ke])
        in  Map.insert mid updated acc

-- | Store all events in a member KEL.
storeMemberKelEvents
    :: Connection -> MemberId -> MemberKel -> IO ()
storeMemberKelEvents conn (MemberId mid) kel =
    mapM_ storeOne (zip [0 ..] (mkelEvents kel))
  where
    storeOne (seqNum, ke) =
        execute
            conn
            "INSERT OR REPLACE INTO member_kels \
            \(member_id, seq_num, event_json \
            \, signatures_json) VALUES (?, ?, ?, ?)"
            ( mid
            , seqNum :: Int
            , TE.decodeUtf8 (keEventBytes ke)
            , encode (keSignatures ke)
            )

-- | Delete all KEL events for a member.
deleteMemberKel
    :: Connection -> MemberId -> IO ()
deleteMemberKel conn (MemberId mid) =
    execute
        conn
        "DELETE FROM member_kels WHERE member_id = ?"
        (Only mid)

-- | Update memberKels in the FullState.
updateKels
    :: CircleEvent d p r
    -> Maybe (MemberId, MemberKel)
    -> FullState g p r
    -> FullState g p r
updateKels evt mKel st = case mKel of
    Just (mid, kel) ->
        st
            { fsMemberKels =
                Map.insert mid kel (fsMemberKels st)
            }
    Nothing -> case evt of
        CEBaseDecision (RemoveMember mid) ->
            st
                { fsMemberKels =
                    Map.delete
                        mid
                        (fsMemberKels st)
                }
        _ -> st
