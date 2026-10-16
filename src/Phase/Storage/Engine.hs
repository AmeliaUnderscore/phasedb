{-# LANGUAGE OverloadedStrings #-}

-- | The core PhaseDB MVCC hierarchical storage engine.
--
-- Manages sparse hierarchical global trees with MUMPS node duality,
-- multi-version concurrency control (MVCC) revision chains for Part VII
-- time travel ('^[tx]GLOBAL(subs)' and '^["timestamp"]GLOBAL(subs)'),
-- optimistic concurrency checks ('EXPECTING'), and WAL durability.
module Phase.Storage.Engine
  ( -- * Storage Engine
    StorageEngine (..)
  , initStorageEngine
  , openStorageEngine

    -- * Cross-process exclusive file lock
  , DbLock
  , acquireDbLock
  , releaseDbLock

    -- * Live Access & Node Duality
  , readLive
  , dataLive
  , orderLive
  , getLiveTree
  , setLiveTree

    -- * Time Travel (Part VII)
  , readAtTx
  , readAtTime
  , dataAtTx
  , orderAtTx

    -- * Direct & Transactional Commit
  , commitTransaction
  , rollbackTransaction
  , commitSet
  , commitKill
  , commitKillSubtree

    -- * WAL Flushing
  , flushPendingWal

    -- * Blob Store (§6.5 / Part XI)
  , readBlob
  , writeBlob

    -- * Checkpointing & Compaction
  , takeCheckpoint
  , compactTo
  ) where

import Data.Int (Int64)
import Data.List (find, foldl', group)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Control.Exception (try, IOException)
import GHC.IO.Handle.Lock (LockMode (..), hTryLock)
import System.Directory (copyFile, doesFileExist, removeFile, renameFile)
import System.IO (Handle, hClose, openFile, IOMode (..))

import Phase.AST (PValue (..), mkValue)
import Phase.Storage.Transaction (TxHandle (..))
import Phase.Storage.Types
  ( Revision (..)
  , StorageError (..)
  , TxId
  , WalOp (..)
  , WalRecord (..)
  )
import Phase.Storage.WAL
  ( appendWalRecord
  , appendWalRecords
  , readWalRecords
  , recoverWalFile
  , walRecordByteSize
  )
import Phase.Tree
  ( Node (..)
  , SubscriptKey (..)
  , dataNode
  , dumpTree
  , emptyNode
  , findSubNode
  , getNode
  , killNode
  , orderNode
  , setNode
  , toSubKey
  )

-- | The multi-version storage engine state.
data StorageEngine = StorageEngine
  { seLive         :: !(Map Text Node)                            -- ^ Current live global trees
  , seHistory      :: !(Map (Text, [SubscriptKey]) [Revision])    -- ^ MVCC historical revisions (latest first) SINCE the resident base (see 'openStorageEngine')
  , seLastTxMap    :: !(Map (Text, [SubscriptKey]) TxId)          -- ^ Last writer TxId per path
  , seLastTx       :: !TxId                                       -- ^ Monotonic last-committed TxId
  , seWalPath      :: !(Maybe FilePath)                           -- ^ Active WAL path if disk-backed
  , sePendingWal   :: ![WalRecord]                                -- ^ WAL records queued for IO flush
  , seCommitsSinceCheckpoint :: !Int                              -- ^ Commits since the last checkpoint (manual or automatic); see 'maybeAutoCheckpoint'
  , seBytesSinceCheckpoint   :: !Int64                            -- ^ Bytes of encoded WAL records appended since the last checkpoint
  , seBlobs        :: !(Map Text ByteString)                      -- ^ Content-addressed blob store: lower-case hex SHA-256 hash -> raw bytes (§6.5 / Part XI)
  } deriving (Eq, Show)

-- | Initialize a fresh in-memory storage engine.
initStorageEngine :: StorageEngine
initStorageEngine = StorageEngine
  { seLive         = Map.empty
  , seHistory      = Map.empty
  , seLastTxMap    = Map.empty
  , seLastTx       = 0
  , seWalPath      = Nothing
  , sePendingWal   = []
  , seCommitsSinceCheckpoint = 0
  , seBytesSinceCheckpoint   = 0
  , seBlobs        = Map.empty
  }

-- --------------------------------------------------------------------
-- Cross-process exclusive file lock
-- --------------------------------------------------------------------

-- | An opaque handle representing an exclusive lock on a @.db@ lock
-- file (e.g. @"phase.db.lock"@).  Acquire with 'acquireDbLock' before
-- opening the storage engine and release with 'releaseDbLock' at
-- shutdown.
--
-- Uses 'GHC.IO.Handle.Lock.hTryLock' from @base@, which maps to
-- @LockFileEx@ on Windows and @flock@/@fcntl@ on POSIX -- no @unix@
-- package required, same pattern as 'Phase.Storage.Sync'.
newtype DbLock = DbLock Handle

-- | Try to acquire an exclusive cross-process lock on @\<dbPath\>.lock@.
-- Returns 'Nothing' when another process (or the same process, in tests)
-- already holds it.
--
-- 'openFile' itself may throw an 'IOException' with GHC's own handle-level
-- lock tracking when the lock file is already exclusively held in the same
-- process; that is treated the same as 'hTryLock' returning 'False'.
acquireDbLock :: FilePath -> IO (Maybe DbLock)
acquireDbLock dbPath = do
  let lockPath = dbPath ++ ".lock"
  result <- try (openFile lockPath AppendMode) :: IO (Either IOException Handle)
  case result of
    Left _  -> pure Nothing   -- file already locked (GHC or OS level)
    Right h -> do
      got <- hTryLock h ExclusiveLock
      if got
        then pure (Just (DbLock h))
        else do
          hClose h
          pure Nothing

-- | Release a 'DbLock' obtained from 'acquireDbLock'.
releaseDbLock :: DbLock -> IO ()
releaseDbLock (DbLock h) = hClose h

-- | Open a disk-backed storage engine from a @.db@ file path.
--
-- Finds the LAST checkpoint record in the file (if any), loads its
-- captured live state directly, and replays only the ordinary records
-- AFTER it, this is the whole point of checkpointing: recalculation
-- cost becomes "distance since the last checkpoint," not "distance
-- since the start of the file." A file with no checkpoint yet (or no
-- file at all) behaves exactly as before: full replay from the
-- beginning.
--
-- 'seHistory' after this call only covers records SINCE the resume
-- point, NOT the full history back to the start of the file. This is
-- intentional and matches the checkpoint design ("a checkpoint is a
-- resume point, not a history archive", see
-- 'Phase.Storage.Types.WalCheckpoint'). A query that needs history
-- further back than what's resident (deep time travel) is served by
-- 'readAtTxDeep'/'readAtTimeDeep' instead, which perform their OWN
-- dedicated re-read of the file scoped to whatever earlier checkpoint
-- actually covers the requested target, see their own docs for why
-- this can't be folded into the ordinary, fast, resident-memory path.
openStorageEngine :: FilePath -> IO (Either StorageError StorageEngine)
openStorageEngine walPath = do
  fileExists <- doesFileExist walPath
  if not fileExists
    then pure (Right initStorageEngine { seWalPath = Just walPath })
    else do
      -- 'recoverWalFile' repairs the file first (cuts off a torn
      -- trailing record and any transaction whose commit never reached
      -- disk) and refuses a corrupt or foreign file, so everything
      -- below only ever sees whole, committed units.
      recovered <- recoverWalFile walPath
      case recovered of
        Left err -> pure (Left err)
        Right records -> do
          let (baseEngine, afterCheckpoint) = case findLastCheckpoint records of
                Nothing -> (initStorageEngine { seWalPath = Just walPath }, records)
                Just (idx, tx, ts, entries) ->
                  let priorBlobs = Map.fromList [ (h, b) | WalRecord _ (WalBlob h b) <- take (idx + 1) records ]
                  in ( (loadCheckpointEntries ts entries initStorageEngine)
                         { seLastTx = tx, seWalPath = Just walPath, seBlobs = priorBlobs }
                     , drop (idx + 1) records
                     )
          pure (Right (replayFrom baseEngine afterCheckpoint))

-- | Find the LAST 'WalCheckpoint' record in a record list, with no
-- upper bound on transaction or time (used for ordinary startup,
-- which always wants the most recent resume point available).
-- Returns its position in the list (so the caller knows which records
-- come after it), its captured tx/timestamp, and its flattened live
-- entries. See 'findCheckpointBefore' for the time-travel variant that
-- DOES take an upper bound.
findLastCheckpoint :: [WalRecord] -> Maybe (Int, TxId, UTCTime, [(Text, [PValue], PValue, TxId)])
findLastCheckpoint records =
  let indexed = zip [0 ..] records
      checkpoints = [ (i, tx, ts, entries)
                    | (i, WalRecord _ (WalCheckpoint tx ts entries)) <- indexed
                    ]
  in case checkpoints of
       [] -> Nothing
       _  -> Just (last checkpoints)

-- | Find the LAST 'WalCheckpoint' record at or before a given
-- transaction bound (used by time-travel replay: "the last checkpoint
-- before the transaction or date requested"). Returns the same shape
-- as 'findLastCheckpoint'.
findCheckpointBeforeTx :: TxId -> [WalRecord] -> Maybe (Int, TxId, UTCTime, [(Text, [PValue], PValue, TxId)])
findCheckpointBeforeTx targetTx records =
  let indexed = zip [0 ..] records
      checkpoints = [ (i, tx, ts, entries)
                    | (i, WalRecord _ (WalCheckpoint tx ts entries)) <- indexed
                    , tx <= targetTx
                    ]
  in case checkpoints of
       [] -> Nothing
       _  -> Just (last checkpoints)

-- | Same as 'findCheckpointBeforeTx', but bounded by a timestamp
-- instead of a transaction ID (used by 'readAtTimeDeep').
findCheckpointBeforeTime :: UTCTime -> [WalRecord] -> Maybe (Int, TxId, UTCTime, [(Text, [PValue], PValue, TxId)])
findCheckpointBeforeTime targetTime records =
  let indexed = zip [0 ..] records
      checkpoints = [ (i, tx, ts, entries)
                    | (i, WalRecord _ (WalCheckpoint tx ts entries)) <- indexed
                    , ts <= targetTime
                    ]
  in case checkpoints of
       [] -> Nothing
       _  -> Just (last checkpoints)

-- | Rebuild a live 'Map Text Node' tree, AND the per-path
-- history/last-tx a time-travel read, @$DATA@, or @EXPECTING@ needs,
-- from a checkpoint's flattened @(global, subscript path, value,
-- lastWriteTx)@ entries. 'checkpointTs' (the checkpoint's OWN
-- timestamp) seeds every entry's synthesized revision timestamp, see
-- 'Phase.Storage.Types.WalCheckpoint''s own docs for why the per-entry
-- 'TxId' is preserved exactly but the per-entry TIMESTAMP is only
-- approximated this way.
--
-- Before this took 'checkpointTs' and each entry's own 'TxId', it only
-- rebuilt 'seLive', leaving 'seHistory'/'seLastTxMap' with NOTHING
-- for any path whose last real write predated the checkpoint, so a
-- time-travel read, @$DATA@, or @EXPECTING@ against such a path (after
-- reopening from this checkpoint, or ALWAYS for a compacted file,
-- where this is the only surviving record of it) would wrongly see it
-- as never written at all.
loadCheckpointEntries :: UTCTime -> [(Text, [PValue], PValue, TxId)] -> StorageEngine -> StorageEngine
loadCheckpointEntries checkpointTs entries eng =
  eng { seLive = foldl applyLive (seLive eng) entries
      , seHistory = foldl applyHist (seHistory eng) entries
      , seLastTxMap = foldl applyTxMap (seLastTxMap eng) entries
      }
  where
    applyLive live (g, subs, val, _lastTx) =
      let root = Map.findWithDefault emptyNode g live
          root' = setNode subs val root
      in Map.insert g root' live
    applyHist hist (g, subs, val, lastTx) =
      Map.insertWith (++) (g, map toSubKey subs) [Revision lastTx checkpointTs (Just val)] hist
    applyTxMap txMap (g, subs, _val, lastTx) =
      Map.insert (g, map toSubKey subs) lastTx txMap

-- | Replay a list of ordinary (non-checkpoint) ops onto a base engine,
-- bringing it forward exactly as far as the given records go. Shared
-- by ordinary startup ('openStorageEngine', replaying "since the last
-- checkpoint") and dedicated time-travel replay ('readAtTxDeep'/
-- 'readAtTimeDeep', replaying "from an earlier checkpoint up to a
-- historical target"), this is the same fold either way, just given
-- a different slice of the file's records.
--
-- A commit's real timestamp is recovered from that transaction's own
-- 'WalCommit' record (always present later in the SAME slice being
-- replayed, appended right after the write records it belongs to,
-- see 'Phase.Storage.Engine.commitTransaction') rather than
-- fabricated. A write whose 'WalCommit' isn't present in this slice
-- at all (e.g. genuinely corrupt/truncated data) falls back to the
-- Unix epoch as a clearly-wrong, immediately-recognizable sentinel.
replayFrom :: StorageEngine -> [WalRecord] -> StorageEngine
replayFrom baseEngine records = foldl replayRecord baseEngine records
  where
    commitTimes :: Map TxId UTCTime
    commitTimes = Map.fromList
      [ (ctx, cts) | WalRecord _ (WalCommit ctx cts _user) <- records ]

    timestampFor :: TxId -> UTCTime
    timestampFor tx = Map.findWithDefault (posixSecondsToUTCTime 0) tx commitTimes

    replayRecord eng (WalRecord tx op) =
      case op of
        WalSet g subs val _ ->
          let k = (g, map toSubKey subs)
              gNode = Map.findWithDefault emptyNode g (seLive eng)
              gNode' = setNode subs val gNode
              live' = Map.insert g gNode' (seLive eng)
              rev = Revision tx (timestampFor tx) (Just val)
              hist' = Map.insertWith (++) k [rev] (seHistory eng)
              txMap' = Map.insert k tx (seLastTxMap eng)
          in eng { seLive = live', seHistory = hist', seLastTxMap = txMap', seLastTx = max tx (seLastTx eng) }

        WalKill g subs ->
          let subKeys = map toSubKey subs
              gNode = Map.findWithDefault emptyNode g (seLive eng)
              -- Same reasoning as the live commit path
              -- ('applyPointKill' in 'commitTransaction'): every
              -- descendant 'killNode' is about to prune needs its own
              -- tombstone too, not just the killed path itself, or
              -- replaying from the WAL would reconstruct the same
              -- gap this fix is closing in the live path.
              tombstonePaths = killTombstonePaths subKeys gNode
              live' = case killNode subs gNode of
                        Nothing -> Map.delete g (seLive eng)
                        Just n  -> Map.insert g n (seLive eng)
              rev = Revision tx (timestampFor tx) Nothing
              hist' = foldl' (\h p -> Map.insertWith (++) (g, p) [rev] h) (seHistory eng) tombstonePaths
              txMap' = foldl' (\m p -> Map.insert (g, p) tx m) (seLastTxMap eng) tombstonePaths
          in eng { seLive = live', seHistory = hist', seLastTxMap = txMap', seLastTx = max tx (seLastTx eng) }

        WalKillSubtree g subs ->
          let subKeys = map toSubKey subs
              gNode = Map.findWithDefault emptyNode g (seLive eng)
              tombstonePaths = killTombstonePaths subKeys gNode
              live' = case killNode subs gNode of
                        Nothing -> Map.delete g (seLive eng)
                        Just n  -> Map.insert g n (seLive eng)
              rev = Revision tx (timestampFor tx) Nothing
              hist' = foldl' (\h p -> Map.insertWith (++) (g, p) [rev] h) (seHistory eng) tombstonePaths
              txMap' = foldl' (\m p -> Map.insert (g, p) tx m) (seLastTxMap eng) tombstonePaths
          in eng { seLive = live', seHistory = hist', seLastTxMap = txMap', seLastTx = max tx (seLastTx eng) }

        WalCommit committedTx _ts _user ->
          eng { seLastTx = max committedTx (seLastTx eng) }

        WalRollback _ -> eng

        WalCheckpoint cpTx cpTs entries ->
          -- A checkpoint encountered MID-REPLAY (not the resume point
          -- itself, which the caller already skipped past) still
          -- represents a definite state as of its own tx, folding
          -- its entries in is harmless and correct either way, since
          -- every entry it carries is exactly what the ordinary
          -- records up to that point would have produced anyway.
          (loadCheckpointEntries cpTs entries eng) { seLastTx = max cpTx (seLastTx eng) }

        WalBlob hash bytes ->
          eng { seBlobs = Map.insert hash bytes (seBlobs eng) }

-- | Read a value from the live global storage.
readLive :: Text -> [PValue] -> StorageEngine -> Either StorageError PValue
readLive global subs eng =
  case Map.lookup global (seLive eng) of
    Nothing   -> Left (StorageUndefinedVar global subs)
    Just root ->
      case getNode subs root of
        Nothing   -> Left (StorageUndefinedVar global subs)
        Just val  -> Right val

-- | 4-state existence check for a live global path (0, 1, 10, 11).
dataLive :: Text -> [PValue] -> StorageEngine -> Int
dataLive global subs eng =
  case Map.lookup global (seLive eng) of
    Nothing   -> 0
    Just root -> dataNode subs root

-- | Traversal for live global sibling keys in collation order.
orderLive :: Text -> [PValue] -> PValue -> StorageEngine -> PValue
orderLive global subs curSub eng =
  case Map.lookup global (seLive eng) of
    Nothing   -> mkValue ""
    Just root ->
      case orderNode subs curSub root of
        Nothing  -> mkValue ""
        Just nxt -> nxt

getLiveTree :: StorageEngine -> Map Text Node
getLiveTree = seLive

setLiveTree :: Map Text Node -> StorageEngine -> StorageEngine
setLiveTree tree eng = eng { seLive = tree }

-- --------------------------------------------------------------------
-- Time Travel (Part VII)
-- --------------------------------------------------------------------

-- | Read a global path as it existed at a specific transaction ID.
--
-- Tries the FAST path first: whatever history is already resident in
-- 'seHistory' (everything since this engine's own last checkpoint
-- resume point, or from the start of the file if it hasn't
-- checkpointed yet). If that doesn't cover 'targetTx', the target
-- predates what's currently loaded in memory, falls back to a
-- DEDICATED re-read of the @.db@ file: find the last checkpoint at or
-- before 'targetTx' and replay forward from there; if NO checkpoint
-- covers the target at all (the target predates every checkpoint the
-- file has, including the common case of a file with exactly one
-- checkpoint sitting AFTER the target), replay from the very
-- beginning of the file instead, exactly the same "no checkpoint
-- yet, replay everything" behavior 'openStorageEngine' itself falls
-- back to. This is necessarily 'IO' (it may need to touch disk),
-- which is exactly why 'Phase.Eval' itself gained 'IO' access, a
-- time-travel query that can't reach its own stored history defeats
-- the purpose of storing it.
--
-- This fallback only ever triggers for genuinely DEEP time travel
-- (further back than the resident engine's own base); ordinary,
-- recent time-travel queries are answered entirely from memory, with
-- no disk access at all.
readAtTx :: TxId -> Text -> [PValue] -> StorageEngine -> IO (Either StorageError PValue)
readAtTx targetTx global subs eng =
  case readAtTxResident targetTx global subs eng of
    Right val -> pure (Right val)
    Left err ->
      case seWalPath eng of
        Nothing -> pure (Left err)
        Just path -> do
          records <- readWalRecords path
          let (baseEngine, recordsToTarget) = case findCheckpointBeforeTx targetTx records of
                Just (idx, cpTx, cpTs, cpEntries) ->
                  ( (loadCheckpointEntries cpTs cpEntries initStorageEngine) { seLastTx = cpTx }
                  , drop (idx + 1) records
                  )
                Nothing ->
                  -- No checkpoint covers this target (it predates
                  -- every checkpoint in the file, if any), replay
                  -- from the absolute start, same as ordinary
                  -- recovery does when there's no checkpoint at all.
                  (initStorageEngine, records)
              deepEngine = replayFrom baseEngine (takeWhileUpToTx targetTx recordsToTarget)
              deepResult = readAtTxResident targetTx global subs deepEngine
          pure (either (const (Left err)) Right deepResult)

-- | The original, purely in-memory lookup, kept as its own function
-- so 'readAtTx' can try it first without any 'IO', and so the deep
-- fallback can reuse the exact same lookup logic against its
-- freshly-built replay engine instead of duplicating it.
readAtTxResident :: TxId -> Text -> [PValue] -> StorageEngine -> Either StorageError PValue
readAtTxResident targetTx global subs eng =
  let key = (global, map toSubKey subs)
  in case Map.lookup key (seHistory eng) of
       Nothing ->
         -- Fallback: if no history exists for this key, check if live node was created before targetTx
         case Map.lookup key (seLastTxMap eng) of
           Just lastTx | lastTx <= targetTx -> readLive global subs eng
           _ -> Left (StorageUndefinedVar global subs)
       Just revs ->
         -- Find latest revision committed at or before targetTx
         let validRevs = filter (\r -> revTx r <= targetTx) revs
         in case validRevs of
              [] -> Left (StorageUndefinedVar global subs)
              (latest : _) ->
                case revValue latest of
                  Just val -> Right val
                  Nothing  -> Left (StorageUndefinedVar global subs)

-- | Stop replaying once a record's own tx exceeds the target, a deep
-- replay only needs to reconstruct state UP TO 'targetTx', not the
-- whole rest of the file, so this keeps a deep query's cost
-- proportional to "distance from the relevant checkpoint to the
-- target," not "distance from that checkpoint to the current end of
-- the file."
takeWhileUpToTx :: TxId -> [WalRecord] -> [WalRecord]
takeWhileUpToTx targetTx = takeWhile (\(WalRecord tx _) -> tx <= targetTx)

-- | Read a global path as it existed at a specific timestamp cutoff.
-- Same resident-first, deep-replay-fallback shape as 'readAtTx', see
-- its docs for the full rationale, including the "no checkpoint
-- covers the target at all" case, which falls back to replaying from
-- the very start of the file rather than giving up.
readAtTime :: UTCTime -> Text -> [PValue] -> StorageEngine -> IO (Either StorageError PValue)
readAtTime cutoff global subs eng =
  case readAtTimeResident cutoff global subs eng of
    Right val -> pure (Right val)
    Left err ->
      case seWalPath eng of
        Nothing -> pure (Left err)
        Just path -> do
          records <- readWalRecords path
          let (baseEngine, recordsToTarget) = case findCheckpointBeforeTime cutoff records of
                Just (idx, cpTx, cpTs, cpEntries) ->
                  ( (loadCheckpointEntries cpTs cpEntries initStorageEngine) { seLastTx = cpTx }
                  , drop (idx + 1) records
                  )
                Nothing ->
                  (initStorageEngine, records)
              deepEngine = replayFrom baseEngine (takeWhileUpToTime cutoff recordsToTarget)
              deepResult = readAtTimeResident cutoff global subs deepEngine
          pure (either (const (Left err)) Right deepResult)

readAtTimeResident :: UTCTime -> Text -> [PValue] -> StorageEngine -> Either StorageError PValue
readAtTimeResident cutoff global subs eng =
  let key = (global, map toSubKey subs)
  in case Map.lookup key (seHistory eng) of
       Nothing -> Left (StorageUndefinedVar global subs)
       Just revs ->
         let validRevs = filter (\r -> revTimestamp r <= cutoff) revs
         in case validRevs of
              [] -> Left (StorageUndefinedVar global subs)
              (latest : _) ->
                case revValue latest of
                  Just val -> Right val
                  Nothing  -> Left (StorageUndefinedVar global subs)

-- | Analogous stopping rule to 'takeWhileUpToTx', bounded by
-- timestamp (using each record's tx's own commit time, recovered the
-- same way 'replayFrom' recovers it, a record with no 'WalCommit' in
-- this slice is treated as being at-or-before the cutoff, erring
-- toward inclusion rather than silently dropping data whose real time
-- can't be determined from this slice alone).
takeWhileUpToTime :: UTCTime -> [WalRecord] -> [WalRecord]
takeWhileUpToTime cutoff records = takeWhile withinCutoff records
  where
    commitTimes = Map.fromList [ (ctx, cts) | WalRecord _ (WalCommit ctx cts _user) <- records ]
    withinCutoff (WalRecord tx _) =
      case Map.lookup tx commitTimes of
        Just ts -> ts <= cutoff
        Nothing -> True

-- | 4-state $DATA check as of a transaction ID.
-- | 4-state $DATA check as of a transaction ID.
--
-- NOTE: only the "has a value at this exact path" half
-- ('readAtTx') gets the deep-replay fallback for history further
-- back than what's resident. The "has descendants" half still only
-- consults resident 'seHistory', a $DATA check for descendant
-- existence far behind the current checkpoint window may
-- under-report until this is given the same deep-replay treatment.
-- Flagged here rather than silently left inconsistent; the value-read
-- half is what real routines overwhelmingly rely on for time-travel,
-- so it is fixed first.
dataAtTx :: TxId -> Text -> [PValue] -> StorageEngine -> IO Int
dataAtTx targetTx global subs eng = do
  hasValResult <- readAtTx targetTx global subs eng
  let hasVal = case hasValResult of
                 Right _ -> True
                 Left _  -> False
      prefixKey = (global, map toSubKey subs)
      -- Check if any descendant paths existed as of targetTx
      hasDescendants =
        let isDescendant (g, p) =
              g == global &&
              length p > length (snd prefixKey) &&
              take (length (snd prefixKey)) p == snd prefixKey
            descendantRevs = Map.filterWithKey (\k _ -> isDescendant k) (seHistory eng)
            hasLiveDescendant revs =
              case filter (\r -> revTx r <= targetTx) revs of
                (Revision _ _ (Just _) : _) -> True
                _                           -> False
        in any hasLiveDescendant (Map.elems descendantRevs)
  pure $ case (hasVal, hasDescendants) of
       (False, False) -> 0
       (True,  False) -> 1
       (False, True)  -> 10
       (True,  True)  -> 11

-- | @$ORDER@ as of a past transaction: the next child subscript of
-- @parentSubs@ after @curSub@ (the empty string means "start from the
-- beginning"), in collation order, among children that EXISTED at
-- @targetTx@, a child exists if it had a value then, or any
-- descendant of it did (the same test 'dataAtTx' applies). Returns the
-- empty string when there is none, exactly like the live @$ORDER@.
--
-- Candidate children come from the history map's keys: a path that has
-- never been written inside the resident history has no key there and
-- so cannot be found, the same limit every other time-travel read
-- here already has before a checkpoint seeds it (see
-- 'loadCheckpointEntries'). Each candidate is then judged by 'dataAtTx'
-- rather than by re-deriving existence a second way, so the two can
-- never disagree about what existed.
orderAtTx :: TxId -> Text -> [PValue] -> PValue -> StorageEngine -> IO PValue
orderAtTx targetTx global parentSubs curSub eng = do
  let depth = length parentSubs
      parentKeys = map toSubKey parentSubs
      isUnder (g, p) = g == global && length p > depth && take depth p == parentKeys
      -- Map.keys is ascending by (global, path), and every path under
      -- one given child prefix is contiguous in that order, so the
      -- child keys come out already collation-sorted with duplicates
      -- adjacent.
      childKeys = map head (group [ p !! depth | k@(_, p) <- Map.keys (seHistory eng), isUnder k ])
      candidates
        | unPValue curSub == "" = childKeys
        | otherwise             = filter (> toSubKey curSub) childKeys
      go [] = pure (mkValue "")
      go (k : ks) = do
        d <- dataAtTx targetTx global (parentSubs ++ [unSubscriptKey k]) eng
        if d /= 0 then pure (unSubscriptKey k) else go ks
  go candidates

-- --------------------------------------------------------------------
-- Transactional Commit & Optimistic Concurrency
-- --------------------------------------------------------------------

-- | Every path that needs its own tombstone revision when the subtree
-- rooted at 'subKeys' is killed: the killed path itself, plus every
-- descendant 'killNode' prunes along with it. A "point" kill is not
-- actually narrower than a "subtree" kill at the live-tree level --
-- 'killNode' always removes the whole subtree either way, only the
-- WAL record's own TYPE differs (see 'Phase.Storage.Types.WalOp') --
-- so both callers need the same full set of paths, computed from
-- 'root' as it stood just BEFORE the kill.
killTombstonePaths :: [SubscriptKey] -> Node -> [[SubscriptKey]]
killTombstonePaths subKeys root =
  subKeys : case findSubNode (map unSubscriptKey subKeys) root of
              Nothing      -> []
              Just subRoot ->
                -- 'dumpTree' includes an entry for the node's OWN
                -- value at relative path [] (that's how a caller like
                -- 'takeCheckpoint' gets it at all), already covered
                -- by the explicit 'subKeys :' above, so it's filtered
                -- out here; without this, every kill double-inserted
                -- its own tombstone revision, throwing off anything
                -- that counts revisions (relative revision addressing,
                -- ^[-N], see docs/tx-pinning-design.md Part 1).
                [ subKeys ++ map toSubKey relPath
                | (relPath, _) <- dumpTree subRoot
                , not (null relPath)
                ]

-- | Commit a staged transaction, verifying all optimistic-concurrency
-- assertions. Takes the acting username so the resulting commit
-- record can attribute the whole transaction to whoever made it (see
-- 'Phase.Storage.Types.WalOp''s 'WalCommit' docs for why attribution
-- lives on the commit marker rather than on every individual
-- write/kill record: one commit is always exactly one user's doing).
commitTransaction :: Text -> TxHandle -> UTCTime -> StorageEngine -> Either StorageError (TxId, StorageEngine)
commitTransaction actingUser tx commitTime eng = do
  -- 1. Validate all 'EXPECTING' assertions against live lastTxMap
  forM_Entries (Map.toList (txStagedWrites tx)) $ \((g, subKeys), (_, mExpected)) ->
    case mExpected of
      Nothing -> pure ()
      Just expectedTx ->
        let actualTx = Map.findWithDefault 0 (g, subKeys) (seLastTxMap eng)
        in if actualTx /= expectedTx
             then
               let subs = map (\(SubscriptKey p) -> p) subKeys
               in Left (StorageWriteConflict g subs expectedTx actualTx)
             else pure ()

  -- 2. Allocate new transaction ID
  let newTx = seLastTx eng + 1

  -- 3. Apply mutations to live tree, history, and lastTxMap
  let applyWrite ((g, subKeys), (val, mExp)) (live, hist, txMap, walAcc) =
        let subs = map (\(SubscriptKey p) -> p) subKeys
            root = Map.findWithDefault emptyNode g live
            root' = setNode subs val root
            live' = Map.insert g root' live
            rev = Revision newTx commitTime (Just val)
            hist' = Map.insertWith (++) (g, subKeys) [rev] hist
            txMap' = Map.insert (g, subKeys) newTx txMap
            walRec = WalRecord newTx (WalSet g subs val mExp)
        in (live', hist', txMap', walRec : walAcc)

  let (live1, hist1, txMap1, setWalRecs) = foldr applyWrite (seLive eng, seHistory eng, seLastTxMap eng, []) (Map.toList (txStagedWrites tx))

  let applyPointKill (g, subKeys) (live, hist, txMap, walAcc) =
        let subs = map (\(SubscriptKey p) -> p) subKeys
            root = Map.findWithDefault emptyNode g live
            -- Every path that needs ITS OWN tombstone: 'killNode'
            -- always prunes the whole subtree (a "point" kill isn't
            -- actually narrower at the live-tree level -- see
            -- 'killTombstonePaths'), so a descendant left untombstoned
            -- would make a time-traveled read of it, or an EXPECTING
            -- against it, still see the pre-kill state at any tx at or
            -- after this one.
            tombstonePaths = killTombstonePaths subKeys root
            live' = case killNode subs root of
                      Nothing -> Map.delete g live
                      Just n  -> Map.insert g n live
            rev = Revision newTx commitTime Nothing
            hist' = foldl' (\h p -> Map.insertWith (++) (g, p) [rev] h) hist tombstonePaths
            txMap' = foldl' (\m p -> Map.insert (g, p) newTx m) txMap tombstonePaths
            walRec = WalRecord newTx (WalKill g subs)
        in (live', hist', txMap', walRec : walAcc)

  let (live2, hist2, txMap2, killWalRecs) = foldr applyPointKill (live1, hist1, txMap1, []) (Set.toList (txStagedPointKills tx))

  let applySubtreeKill (g, subKeys) (live, hist, txMap, walAcc) =
        let subs = map (\(SubscriptKey p) -> p) subKeys
            root = Map.findWithDefault emptyNode g live
            -- Same reasoning as 'applyPointKill': every path under
            -- (and including) 'subs' needs a tombstone. Unlike that
            -- function, this one had NO history update at all before
            -- this fix -- not even for the killed path itself.
            tombstonePaths = killTombstonePaths subKeys root
            live' = case killNode subs root of
                      Nothing -> Map.delete g live
                      Just n  -> Map.insert g n live
            rev = Revision newTx commitTime Nothing
            hist' = foldl' (\h p -> Map.insertWith (++) (g, p) [rev] h) hist tombstonePaths
            txMap' = foldl' (\m p -> Map.insert (g, p) newTx m) txMap tombstonePaths
            walRec = WalRecord newTx (WalKillSubtree g subs)
        in (live', hist', txMap', walRec : walAcc)

  let (live3, hist3, txMap3, treeWalRecs) = foldr applySubtreeKill (live2, hist2, txMap2, []) (Set.toList (txStagedSubtreeKills tx))

  -- 4. Build commit WAL record and queue all records for IO flushing
  let commitRec = WalRecord newTx (WalCommit newTx commitTime actingUser)
  let thisCommitRecords = setWalRecs ++ killWalRecs ++ treeWalRecs ++ [commitRec]
  let allPending = sePendingWal eng ++ thisCommitRecords

  -- Track commits/bytes since the last checkpoint (manual or
  -- automatic), consulted by 'maybeAutoCheckpoint' after this
  -- transaction's records actually reach disk. This is pure
  -- bookkeeping (an Int and an Int64 increment), NOT the disk
  -- append itself, that still only happens in 'flushPendingWal'.
  let addedBytes = sum (map walRecordByteSize thisCommitRecords)

  let eng' = eng
        { seLive         = live3
        , seHistory      = hist3
        , seLastTxMap    = txMap3
        , seLastTx       = newTx
        , sePendingWal   = allPending
        , seCommitsSinceCheckpoint = seCommitsSinceCheckpoint eng + 1
        , seBytesSinceCheckpoint   = seBytesSinceCheckpoint eng + addedBytes
        }
  pure (newTx, eng')
  where
    forM_Entries [] _ = pure ()
    forM_Entries (x : xs) f = f x >> forM_Entries xs f

-- | Rollback an uncommitted transaction.
rollbackTransaction :: TxHandle -> StorageEngine -> StorageEngine
rollbackTransaction _ eng = eng

-- | Direct SET outside an explicit transaction block (auto-commits as single transaction).
commitSet :: Text -> Text -> [PValue] -> PValue -> Maybe TxId -> UTCTime -> StorageEngine -> Either StorageError (TxId, StorageEngine)
commitSet actingUser global subs val mExp commitTime eng =
  let tx = TxHandle 0 commitTime (Map.singleton (global, map toSubKey subs) (val, mExp)) Set.empty Set.empty
  in commitTransaction actingUser tx commitTime eng

-- | Direct point KILL outside an explicit transaction block.
commitKill :: Text -> Text -> [PValue] -> UTCTime -> StorageEngine -> (TxId, StorageEngine)
commitKill actingUser global subs commitTime eng =
  let tx = TxHandle 0 commitTime Map.empty (Set.singleton (global, map toSubKey subs)) Set.empty
  in case commitTransaction actingUser tx commitTime eng of
       Right res -> res
       Left _    -> (seLastTx eng, eng)

-- | Direct subtree KILL outside an explicit transaction block.
commitKillSubtree :: Text -> Text -> [PValue] -> UTCTime -> StorageEngine -> (TxId, StorageEngine)
commitKillSubtree actingUser global subs commitTime eng =
  let tx = TxHandle 0 commitTime Map.empty Set.empty (Set.singleton (global, map toSubKey subs))
  in case commitTransaction actingUser tx commitTime eng of
       Right res -> res
       Left _    -> (seLastTx eng, eng)

-- --------------------------------------------------------------------
-- Blob Store (§6.5 / Part XI)
-- --------------------------------------------------------------------

-- | Look up a blob by its lower-case hex SHA-256 hash.  Returns
-- 'Nothing' when no blob with that hash is in the current engine.
readBlob :: Text -> StorageEngine -> Maybe ByteString
readBlob hash eng = Map.lookup hash (seBlobs eng)

-- | Store a new blob.  The blob is added to 'seBlobs' immediately
-- (for reads within the same session), AND a 'WalBlob' record is
-- queued in 'sePendingWal' so the blob is persisted on the next
-- 'flushPendingWal'.  The caller is expected to call
-- 'flushPendingWal' (through the normal commit path) before
-- returning a success response.
--
-- If a blob with the same hash already exists this is a no-op: the
-- content-addressed store is idempotent.
writeBlob :: TxId -> Text -> ByteString -> StorageEngine -> StorageEngine
writeBlob tx hash bytes eng
  | Map.member hash (seBlobs eng) = eng  -- idempotent
  | otherwise =
      let blobRec = WalRecord tx (WalBlob hash bytes)
      in eng { seBlobs     = Map.insert hash bytes (seBlobs eng)
             , sePendingWal = sePendingWal eng ++ [blobRec]
             }

-- --------------------------------------------------------------------
-- Transactional Commit & Optimistic Concurrency
-- --------------------------------------------------------------------

-- | Flush any pending WAL records to disk, clearing the pending queue.
-- Should be called from the IO layer (REPL or transaction commit hook) after
-- every operation that may have staged writes.
flushPendingWal :: StorageEngine -> IO StorageEngine
flushPendingWal eng = flushWal eng >>= maybeAutoCheckpoint

-- | Write out 'sePendingWal' (no checkpoint logic). All pending records
-- go to disk in ONE append and are fsynced before this returns, so a
-- crash can never leave a transaction's writes on disk without its
-- 'WalCommit' except as a trailing fragment, which
-- 'Phase.Storage.WAL.recoverWalFile' discards on the next open.
flushWal :: StorageEngine -> IO StorageEngine
flushWal eng =
  case (seWalPath eng, sePendingWal eng) of
    (_, []) -> pure eng
    (Nothing, _) -> pure eng { sePendingWal = [] }
    (Just walPath, pending) -> do
      appendWalRecords walPath pending
      pure eng { sePendingWal = [] }

-- --------------------------------------------------------------------
-- Checkpointing & Compaction
-- --------------------------------------------------------------------

-- | Read the configured commit/byte checkpoint thresholds from
-- @^_CONFIG("checkpoint","commits")@ / @^_CONFIG("checkpoint","bytes")@
--, an ordinary global, read the same way any other global is (see
-- 'readLive'), NOT a Haskell-level config file or CLI flag. This is
-- deliberate: it means the thresholds are configured through Phase's
-- own @SET@ syntax, consistent with how @^_CATALOG@ already stores
-- routines and @^_USERS@ stores accounts, the database configures
-- itself via its own data model, rather than needing an out-of-band
-- mechanism.
--
-- 'Nothing' for either threshold means "no automatic checkpointing on
-- that axis" (either genuinely unset, or set to something that
-- doesn't parse as a positive integer), automatic checkpointing is
-- OFF by default until explicitly configured, so a database nobody
-- has touched this setting on behaves exactly as before.
checkpointThresholds :: StorageEngine -> (Maybe Int, Maybe Int64)
checkpointThresholds eng =
  ( readPositiveConfig ["checkpoint", "commits"]
  , fromIntegral <$> (readPositiveConfig ["checkpoint", "bytes"] :: Maybe Int)
  )
  where
    readPositiveConfig :: [Text] -> Maybe Int
    readPositiveConfig path =
      case readLive "_CONFIG" (map mkValue path) eng of
        Left _ -> Nothing
        Right pval ->
          case reads (T.unpack (unPValue pval)) of
            ((n, "") : _) | n > 0 -> Just n
            _                     -> Nothing

-- | After a flush, check the accumulated commit/byte counters against
-- whatever thresholds are currently configured in @^_CONFIG@, and
-- automatically take a checkpoint (resetting both counters) if either
-- is met or exceeded. Called from 'flushPendingWal', the one 'IO'
-- boundary every commit path already funnels through, so this needs
-- no separate hook in the REPL or anywhere else that flushes.
maybeAutoCheckpoint :: StorageEngine -> IO StorageEngine
maybeAutoCheckpoint eng =
  let (commitThreshold, byteThreshold) = checkpointThresholds eng
      commitsMet = maybe False (seCommitsSinceCheckpoint eng >=) commitThreshold
      bytesMet   = maybe False (seBytesSinceCheckpoint eng >=) byteThreshold
  in if commitsMet || bytesMet
       then takeCheckpoint eng
       else pure eng

-- | Take a checkpoint: append a 'WalCheckpoint' record, capturing the
-- engine's current live state, to the SAME @.db@ file, there is no
-- separate snapshot file anymore. This is always safe and
-- non-destructive: it only ever appends a new resume point, never
-- removes or alters any existing record, so every earlier checkpoint
-- (and the records between them) remains available for time travel
-- exactly as before. Only 'compactTo' ever removes data.
--
-- Returns the engine with 'seCommitsSinceCheckpoint' and
-- 'seBytesSinceCheckpoint' reset to 0, callers (manual @.checkpoint@
-- in the REPL, or 'maybeAutoCheckpoint') MUST use the returned engine
-- going forward, not the one they passed in, or the counters will
-- never actually reset and automatic checkpointing will keep firing
-- on every subsequent flush.
takeCheckpoint :: StorageEngine -> IO StorageEngine
takeCheckpoint eng0 = do
  -- The checkpoint captures 'seLive', which already reflects any
  -- pending commits, so those records must reach the file FIRST.
  -- Otherwise the checkpoint would land before the records it already
  -- includes.
  eng <- flushWal eng0
  case seWalPath eng of
    Nothing -> pure eng { seCommitsSinceCheckpoint = 0, seBytesSinceCheckpoint = 0 }  -- purely in-memory engine, nothing to persist, but still reset counters
    Just path -> do
      now <- getCurrentTime
      -- Each entry carries its OWN last-write tx (falling back to the
      -- checkpoint's own tx only if 'seLastTxMap' somehow has nothing
      -- for a path that is nonetheless live -- shouldn't happen via
      -- any ordinary SET, but a safe default either way: treating an
      -- unknown last-write as "now" never makes EXPECTING wrongly
      -- think a live path was never written).
      let lastTxFor g subs = Map.findWithDefault (seLastTx eng) (g, map toSubKey subs) (seLastTxMap eng)
          entries = [ (g, subs, val, lastTxFor g subs) | (g, node) <- Map.toList (seLive eng), (subs, val) <- dumpTree node ]
          -- Re-emit all live blobs as WalBlob records BEFORE the checkpoint
          -- so that a reader starting from this checkpoint can still load them.
          blobRecs = [ WalRecord (seLastTx eng) (WalBlob h b) | (h, b) <- Map.toList (seBlobs eng) ]
          cpRec = WalRecord (seLastTx eng) (WalCheckpoint (seLastTx eng) now entries)
      appendWalRecords path (blobRecs ++ [cpRec])
      pure eng { seCommitsSinceCheckpoint = 0, seBytesSinceCheckpoint = 0 }

-- | Compact (vacuum) a @.db@ file: permanently discard every record
-- before the given transaction, keeping only a fresh baseline
-- checkpoint as of that transaction and whatever ordinary records
-- follow it. This is the team's own explicit design: "disregard every
-- historical and dead value until this transaction", a genuinely
-- destructive operation, unlike 'takeCheckpoint'.
--
-- Safety sequence, exactly as specified:
--
--   1. Copy the CURRENT file to a backup path in full, before
--      touching anything.
--   2. Compute live state as of the target transaction (by the same
--      checkpoint-or-replay-from-start logic 'readAtTx' itself uses
--      to find a resume point, just applied to the whole live tree
--      rather than one path).
--   3. Write a BRAND NEW file (to a temporary path) containing just a
--      single checkpoint record for that baseline, followed by every
--      ORIGINAL record whose own tx is strictly after the target
--      (preserved verbatim, so anything AFTER the compaction point
--      keeps its full fidelity, including any of ITS OWN checkpoints).
--   4. Only once that new file is COMPLETELY written, atomically
--      rename it over the original path, so a crash partway through
--      compaction leaves the ORIGINAL file untouched and the backup
--      already safely written, never a half-compacted primary file.
--
-- After this, time-travel to any point before the target transaction
-- is permanently gone from the primary file, recoverable only from
-- the backup this function created.
compactTo :: FilePath -> TxId -> IO (Either StorageError FilePath)
compactTo path targetTx = do
  fileExists <- doesFileExist path
  if not fileExists
    then pure (Left (StorageCustom "cannot compact: database file does not exist"))
    else do
      records <- readWalRecords path
      now <- getCurrentTime
      backupPath <- makeBackupPath path now
      copyFile path backupPath

      let (baseEngine, afterCheckpoint) = case findCheckpointBeforeTx targetTx records of
            Nothing -> (initStorageEngine, records)
            Just (idx, cpTx, cpTs, cpEntries) ->
              let priorBlobs = Map.fromList [ (h, b) | WalRecord _ (WalBlob h b) <- take (idx + 1) records ]
              in ( (loadCheckpointEntries cpTs cpEntries initStorageEngine)
                     { seLastTx = cpTx, seBlobs = priorBlobs }
                 , drop (idx + 1) records
                 )
          upToTarget = takeWhileUpToTx targetTx afterCheckpoint
          engineAtTarget = replayFrom baseEngine upToTarget
          keptRecords = filter (\(WalRecord tx _) -> tx > targetTx) records
          -- Same per-entry last-tx preservation as 'takeCheckpoint':
          -- this is the ONE place that survives compaction for
          -- whatever a path's true last-write tx was, so losing it
          -- here specifically (not just for an ordinary checkpoint)
          -- would make it unrecoverable.
          lastTxFor g subs = Map.findWithDefault targetTx (g, map toSubKey subs) (seLastTxMap engineAtTarget)
          baselineEntries = [ (g, subs, val, lastTxFor g subs) | (g, node) <- Map.toList (seLive engineAtTarget), (subs, val) <- dumpTree node ]
          baselineCheckpoint = WalRecord targetTx (WalCheckpoint targetTx now baselineEntries)

          -- Blob GC: keep only blobs referenced in the baseline live
          -- state OR by any WalSet in the kept tail (a future SET
          -- might reference a blob uploaded before the compaction point).
          extractBlobHash v =
            let (pfx, rest) = T.splitAt 5 (unPValue v)
            in if T.toLower pfx == "blob:"
               then Just (T.toLower rest)
               else Nothing
          liveHashes = Set.fromList
            [ h | (_, _, val, _) <- baselineEntries, Just h <- [extractBlobHash val] ]
          futureHashes = Set.fromList
            [ h | WalRecord _ (WalSet _ _ val _) <- keptRecords, Just h <- [extractBlobHash val] ]
          liveBlobs = liveHashes `Set.union` futureHashes
          -- Emit WalBlob records only for the live set
          blobRecs = [ WalRecord targetTx (WalBlob h b)
                     | (h, b) <- Map.toList (seBlobs engineAtTarget)
                     , Set.member h liveBlobs ]

      let tmpPath = path ++ ".compact-tmp"
      tmpExists <- doesFileExist tmpPath
      if tmpExists then removeFileIfExists tmpPath else pure ()
      appendWalRecords tmpPath (blobRecs ++ [baselineCheckpoint] ++ keptRecords)
      renameFile tmpPath path
      pure (Right backupPath)
  where
    makeBackupPath p now =
      pure (p ++ ".backup-" ++ show (round (utcTimeToPOSIXSeconds now * 1000000) :: Integer))

    removeFileIfExists p = do
      e <- doesFileExist p
      if e then removeFile p else pure ()