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
table and served directly from SQLite.
-}
module KelCircle.Store
    ( -- * Store handle
      CircleStore (..)

      -- * Lifecycle
    , openStore
    , closeStore

      -- * Operations
    , appendCircleEvent
    , appendRotationEvent
    , insertMemberKel
    , readFullState
    , readEventsFrom
    , readMemberKel
    , lookupKelKeyState
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
    , KelKeyState
    , MemberKel (..)
    , applyStoredEvent
    , kelKeyState
    )
import KelCircle.Processing
    ( FullState (..)
    , applyAppDecision
    , applyBase
    , applyProposal
    , applyResolveWithEffect
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
    }

{- | Open or create a circle store at the given path.
Creates the events and member_kels tables on first open
and replays existing events to rebuild the in-memory state.
KELs are served directly from SQLite, not replayed into
the TVar.
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
    -> (p -> Maybe BaseDecision)
    -- ^ Extract base decision from proposal content
    -> FilePath
    -> IO (CircleStore g p r)
openStore sid initApp appFold extractDecision path = do
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
        replayedState =
            foldl
                (replayRow appFold extractDecision)
                initialState
                rows
    -- Populate key state cache from SQLite KELs
    memberIds <- queryAllMemberIds conn
    keyStates <-
        foldM'
            ( \acc mid -> do
                mKel <-
                    readMemberKelInternal conn mid
                pure $ case mKel >>= eitherToMaybe . kelKeyState of
                    Just kks ->
                        Map.insert mid kks acc
                    Nothing -> acc
            )
            Map.empty
            memberIds
    let finalState =
            replayedState
                { fsKeyStates = keyStates
                }
    stVar <- newTVarIO finalState
    pure
        CircleStore
            { csConn = conn
            , csState = stVar
            }

-- | Close the store.
closeStore :: CircleStore g p r -> IO ()
closeStore = close . csConn

{- | Append a verified event. Persists to SQLite and
updates the in-memory state. The caller is responsible
for validation before calling this.

When the event is 'IntroduceMember', the optional
'MemberKel' is inserted into the member_kels table.
When an interaction 'KelEvent' is provided, it is
appended to the signer's existing KEL.

Returns (sequenceNumber, kelUpdates) where
kelUpdates lists all (MemberId, eventCount) pairs
for KELs that were modified by this event.
-}
appendCircleEvent
    :: (ToJSON d, ToJSON p, ToJSON r)
    => CircleStore g p r
    -> (g -> d -> g)
    -- ^ Application fold function
    -> (p -> Maybe BaseDecision)
    -- ^ Extract base decision from proposal content
    -> Text
    -- ^ Signer key
    -> Text
    -- ^ Signature
    -> CircleEvent d p r
    -- ^ The event to store
    -> Maybe (MemberId, MemberKel)
    -- ^ Optional: member KEL for IntroduceMember
    -> Maybe (MemberId, KelEvent)
    -- ^ Optional: interaction event to append
    -> IO (Int, [(MemberId, Int)])
appendCircleEvent
    store
    appFold
    extractDecision
    signer
    sig
    evt
    mKel
    mIxn = do
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
        -- Persist interaction KEL event if provided
        case mIxn of
            Just (mid, ke) ->
                appendSingleKelEvent
                    (csConn store)
                    mid
                    ke
            Nothing -> pure ()
        -- Delete KEL on member removal
        case evt of
            CEBaseDecision (RemoveMember mid) ->
                deleteMemberKel (csConn store) mid
            _ -> pure ()
        -- Query KEL counts for all affected members
        kelUpdates <- do
            kelKel <- case mKel of
                Just (mid, _) -> do
                    c <- queryKelCount (csConn store) mid
                    pure [(mid, c)]
                Nothing -> pure []
            ixnKel <- case mIxn of
                Just (mid, _) -> do
                    c <- queryKelCount (csConn store) mid
                    pure [(mid, c)]
                Nothing -> pure []
            pure (kelKel <> ixnKel)
        -- Update in-memory state + key state cache
        sn <- atomically $ do
            st <- readTVar (csState store)
            let st' =
                    applyEvent
                        appFold
                        extractDecision
                        st
                        (MemberId signer)
                        evt
                ks0 = fsKeyStates st'
                -- Update cache for introduced member's KEL
                ks1 = case mKel of
                    Just (mid, kel) ->
                        case kelKeyState kel of
                            Right kks ->
                                Map.insert mid kks ks0
                            Left _ -> ks0
                    Nothing -> ks0
                -- Update cache for signer's interaction
                ks2 = case mIxn of
                    Just (mid, ke) ->
                        case Map.lookup mid ks1 of
                            Just kks ->
                                case applyStoredEvent kks ke of
                                    Right kks' ->
                                        Map.insert mid kks' ks1
                                    Left _ -> ks1
                            Nothing -> ks1
                    Nothing -> ks1
                -- Remove cache entry on member removal
                ks3 = case evt of
                    CEBaseDecision (RemoveMember mid) ->
                        Map.delete mid ks2
                    _ -> ks2
            writeTVar
                (csState store)
                st'{fsKeyStates = ks3}
            pure (fsNextSeq st)
        pure (sn, kelUpdates)

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
storeLength store = do
    [Only n] <-
        query_
            (csConn store)
            "SELECT COUNT(*) FROM events"
    pure (n :: Int)

{- | Read a member's KEL from SQLite.
Returns 'Nothing' if no KEL events exist for the member.
-}
readMemberKel
    :: CircleStore g p r -> MemberId -> IO (Maybe MemberKel)
readMemberKel store (MemberId mid) = do
    rows <-
        query
            (csConn store)
            "SELECT event_json, signatures_json \
            \FROM member_kels \
            \WHERE member_id = ? \
            \ORDER BY seq_num"
            (Only mid)
            :: IO [(Text, LBS.ByteString)]
    case rows of
        [] -> pure Nothing
        _ ->
            pure $
                Just $
                    MemberKel $
                        map toKelEvent rows
  where
    toKelEvent (evtJson, sigsJson) =
        KelEvent
            { keEventBytes = TE.encodeUtf8 evtJson
            , keSignatures =
                fromMaybe [] (decode sigsJson)
            }

{- | Append a rotation event to a member's KEL.
Persists to SQLite. Returns the new KEL event count
for the member.
-}
appendRotationEvent
    :: CircleStore g p r
    -> MemberId
    -> KelEvent
    -> IO Int
appendRotationEvent store mid ke = do
    appendSingleKelEvent (csConn store) mid ke
    atomically $ do
        st <- readTVar (csState store)
        case Map.lookup mid (fsKeyStates st) of
            Just kks ->
                case applyStoredEvent kks ke of
                    Right kks' ->
                        writeTVar
                            (csState store)
                            st
                                { fsKeyStates =
                                    Map.insert
                                        mid
                                        kks'
                                        (fsKeyStates st)
                                }
                    Left _ -> pure ()
            Nothing -> pure ()
    queryKelCount (csConn store) mid

{- | Insert a complete member KEL into the store.
Used to seed the sequencer's inception KEL at genesis.
-}
insertMemberKel
    :: CircleStore g p r -> MemberId -> MemberKel -> IO ()
insertMemberKel store mid kel = do
    storeMemberKelEvents (csConn store) mid kel
    case kelKeyState kel of
        Right kks -> atomically $ do
            st <- readTVar (csState store)
            writeTVar
                (csState store)
                st
                    { fsKeyStates =
                        Map.insert
                            mid
                            kks
                            (fsKeyStates st)
                    }
        Left _ -> pure ()

{- | Look up a cached key state for a member.
Returns 'Nothing' if no KEL exists for the member.
O(log n) via the in-memory 'Map'.
-}
lookupKelKeyState
    :: CircleStore g p r
    -> MemberId
    -> IO (Maybe KelKeyState)
lookupKelKeyState store mid = do
    st <- readTVarIO (csState store)
    pure $ Map.lookup mid (fsKeyStates st)

-- --------------------------------------------------------
-- Internal helpers
-- --------------------------------------------------------

-- | Apply a circle event to the full state.
applyEvent
    :: (g -> d -> g)
    -> (p -> Maybe BaseDecision)
    -> FullState g p r
    -> MemberId
    -> CircleEvent d p r
    -> FullState g p r
applyEvent appFold extractDecision st signer = \case
    CEBaseDecision bd -> applyBase st bd
    CEAppDecision d ->
        applyAppDecision st d appFold
    CEProposal content deadline ->
        applyProposal st content signer deadline
    CEResponse content pid ->
        applyResponse st content signer pid
    CEResolveProposal pid res ->
        applyResolveWithEffect
            extractDecision
            st
            pid
            res

{- | Replay a single row from the database into the
full state.
-}
replayRow
    :: (FromJSON d, FromJSON p, FromJSON r)
    => (g -> d -> g)
    -> (p -> Maybe BaseDecision)
    -> FullState g p r
    -> (Text, LBS.ByteString)
    -> FullState g p r
replayRow appFold extractDecision st (signer, eventJson) =
    case decode eventJson of
        Just evt ->
            applyEvent
                appFold
                extractDecision
                st
                (MemberId signer)
                evt
        Nothing -> st

-- | Append a single KEL event to an existing member.
appendSingleKelEvent
    :: Connection -> MemberId -> KelEvent -> IO ()
appendSingleKelEvent conn (MemberId mid) ke = do
    [Only seqNum] <-
        query
            conn
            "SELECT COALESCE(MAX(seq_num), -1) + 1 \
            \FROM member_kels WHERE member_id = ?"
            (Only mid)
    execute
        conn
        "INSERT INTO member_kels \
        \(member_id, seq_num, event_json \
        \, signatures_json) VALUES (?, ?, ?, ?)"
        ( mid
        , seqNum :: Int
        , TE.decodeUtf8 (keEventBytes ke)
        , encode (keSignatures ke)
        )

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

-- | Query KEL event count for a member from SQLite.
queryKelCount :: Connection -> MemberId -> IO Int
queryKelCount conn (MemberId mid) = do
    [Only n] <-
        query
            conn
            "SELECT COUNT(*) FROM member_kels \
            \WHERE member_id = ?"
            (Only mid)
    pure (n :: Int)

-- | Query all distinct member IDs with KELs.
queryAllMemberIds :: Connection -> IO [MemberId]
queryAllMemberIds conn = do
    rows <-
        query_
            conn
            "SELECT DISTINCT member_id \
            \FROM member_kels"
            :: IO [Only Text]
    pure [MemberId mid | Only mid <- rows]

-- | Read a member's KEL directly from SQLite.
readMemberKelInternal
    :: Connection -> MemberId -> IO (Maybe MemberKel)
readMemberKelInternal conn (MemberId mid) = do
    rows <-
        query
            conn
            "SELECT event_json, signatures_json \
            \FROM member_kels \
            \WHERE member_id = ? \
            \ORDER BY seq_num"
            (Only mid)
            :: IO [(Text, LBS.ByteString)]
    case rows of
        [] -> pure Nothing
        _ ->
            pure $
                Just $
                    MemberKel $
                        map toKelEvent rows
  where
    toKelEvent (evtJson, sigsJson) =
        KelEvent
            { keEventBytes = TE.encodeUtf8 evtJson
            , keSignatures =
                fromMaybe [] (decode sigsJson)
            }

-- | Convert Either to Maybe (discarding Left).
eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right x) = Just x
eitherToMaybe (Left _) = Nothing

-- | Strict left fold over a list in IO.
foldM' :: (a -> b -> IO a) -> a -> [b] -> IO a
foldM' _ acc [] = pure acc
foldM' f acc (x : xs) = do
    acc' <- f acc x
    acc' `seq` foldM' f acc' xs
