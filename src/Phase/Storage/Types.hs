{-# LANGUAGE OverloadedStrings #-}

-- | Core types for the PhaseDB storage engine: transactions, paths,
-- multi-version concurrency control (MVCC) revisions, WAL operations,
-- and storage errors.
module Phase.Storage.Types
  ( -- * Transaction Identifiers
    TxId

    -- * Paths and Revisions
  , GlobalPath (..)
  , mkGlobalPath
  , Revision (..)

    -- * Write-Ahead Log (WAL)
  , WalOp (..)
  , WalRecord (..)

    -- * Errors
  , StorageError (..)
  , formatStorageError
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Word (Word64)

import Phase.AST (PValue (..))

-- | Monotonically increasing transaction identifier.
type TxId = Word64

-- | A canonical subscript path targeting a global node.
data GlobalPath = GlobalPath
  { gpGlobal :: !Text     -- ^ Name of global (without '^' sigil)
  , gpSubs   :: ![PValue] -- ^ Evaluated subscript values
  } deriving (Eq, Show)

-- | Construct a 'GlobalPath'.
mkGlobalPath :: Text -> [PValue] -> GlobalPath
mkGlobalPath = GlobalPath

-- | An MVCC revision representing a point-in-time state of a global path.
data Revision = Revision
  { revTx        :: !TxId           -- ^ Transaction ID that committed this revision
  , revTimestamp :: !UTCTime        -- ^ Commit timestamp
  , revValue     :: !(Maybe PValue) -- ^ Node value ('Nothing' indicates node was killed)
  } deriving (Eq, Show)

-- | Atomic operations logged in the persistent record stream (a file
-- historically called the "WAL," though it now also holds checkpoint
-- records interleaved with ordinary write records, see
-- 'Phase.Storage.WAL' for the on-disk binary format and
-- 'Phase.Storage.Engine' for how checkpoints and time-travel replay
-- consume this).
data WalOp
  = WalSet !Text ![PValue] !PValue !(Maybe TxId) -- ^ Set value with optional expectedTx
  | WalKill !Text ![PValue]                       -- ^ Point kill: 'killNode' prunes this path and
                                                   -- every descendant exactly as a subtree kill
                                                   -- does (there is no narrower live-tree effect),
                                                   -- the two 'WalOp's differ only in which shape of
                                                   -- KILL produced them, not in what they do. Both
                                                   -- record a tombstone revision for this path AND
                                                   -- every descendant (see 'Phase.Storage.Engine.
                                                   -- killTombstonePaths').
  | WalKillSubtree !Text ![PValue]                -- ^ Subtree kill, see 'WalKill', above.
  | WalCommit !TxId !UTCTime !Text                -- ^ Transaction commit marker, with the acting username
  | WalRollback !TxId                             -- ^ Transaction rollback marker
  | WalCheckpoint !TxId !UTCTime ![(Text, [PValue], PValue, TxId)]
    -- ^ A resume point: the full LIVE global state (flattened to
    -- @(global, subscript path, value, lastWriteTx)@ quadruples, one
    -- per live path, see 'Phase.Tree.dumpTree') as of transaction
    -- 'TxId' / 'UTCTime'. The trailing 'TxId' on each entry is the
    -- transaction that MOST RECENTLY wrote that exact path (which can
    -- be long before the checkpoint's own 'TxId', for a path nothing
    -- has touched since), carried per-entry, rather than assumed to
    -- be the checkpoint's own tx, so that reopening from a checkpoint
    -- (or from a compacted file, where this is the ONLY surviving
    -- record of it) still answers time-travel reads, @$DATA@, and
    -- @EXPECTING@ correctly for a path whose last real write predates
    -- the checkpoint. The checkpoint's own 'UTCTime' is reused as
    -- every such entry's synthesized revision timestamp, compaction
    -- discards the exact original commit time along with the rest of
    -- that write's own history, so this is already the closest
    -- available approximation, consistent with history before a
    -- compaction being gone by design.
    --
    -- Deliberately carries ONLY live state, not historical revisions
    --, time-travel to any point at or after this checkpoint is
    -- served by loading this state and replaying the ordinary records
    -- that follow it, which reconstructs whatever history that
    -- specific range needs on demand. This keeps a checkpoint's size
    -- proportional to live data, not total accumulated history.
    -- Taking a checkpoint NEVER deletes or replaces any earlier
    -- record in the file, it only appends a new resume point, so
    -- time-travel to a point before this checkpoint still works by
    -- falling back to an earlier checkpoint (or the start of the
    -- file). Only compaction ever removes old records.
  deriving (Eq, Show)

-- | A discrete record stored in the write-ahead log.
data WalRecord = WalRecord
  { wrTxId :: !TxId
  , wrOp   :: !WalOp
  } deriving (Eq, Show)

-- | Storage-level error conditions.
data StorageError
  = StorageUndefinedVar !Text ![PValue]
  | StorageWriteConflict !Text ![PValue] !TxId !TxId -- ^ Path, expectedTx, actualTx
  | StorageReadOnlyTimeTravel
  | StorageCorruptWal !Text
  | StorageCustom !Text
  deriving (Eq, Show)

-- | Format a 'StorageError' according to Phase standard error messages.
formatStorageError :: StorageError -> Text
formatStorageError (StorageUndefinedVar name subs) =
  let subTxt = if null subs then "" else "(" <> T.intercalate "," (map unPValue subs) <> ")"
  in "<UNDEFINED VARIABLE> ^" <> name <> subTxt
formatStorageError (StorageWriteConflict name subs expected actual) =
  let subTxt = if null subs then "" else "(" <> T.intercalate "," (map unPValue subs) <> ")"
  in "<CONFLICT> ^" <> name <> subTxt <> " expected tx " <> T.pack (show expected) <> " but was " <> T.pack (show actual)
formatStorageError StorageReadOnlyTimeTravel =
  "<SET ON READONLY> Cannot write to a time-traveled reference"
formatStorageError (StorageCorruptWal msg) =
  "<CORRUPT WAL> " <> msg
formatStorageError (StorageCustom msg) =
  "<ERROR> " <> msg
