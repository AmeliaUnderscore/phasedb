{-# LANGUAGE OverloadedStrings #-}

-- | Shared mutable server state (§8 concurrency model).
--
-- The spec (§8) calls for:
--
--   * An 'MVar' holding the current 'StorageEngine' + 'LockTable' + version
--     counter @v@.  Whoever takes this MVar is the exclusive commit holder.
--
--   * An 'IORef' holding the latest COMMITTED snapshot (engine + lock table
--     + @v@).  Requests take their starting snapshot from here without
--     blocking any ongoing commit.
--
--   * A 'TVar' mapping token digests to 'Session' records (M2).
--
-- Config-value resolution (D9): 'configValue' reads @^_CONFIG("server", k)@
-- from the live snapshot and falls back to the hard-coded default, so a live
-- SET takes effect without a restart unless a CLI flag pins the value.
module Server.State
  ( -- * Snapshot (lock-free read path)
    Snapshot (..)
  , SnapshotRef

    -- * Engine lock (commit path)
  , EngineLock

    -- * Full server state
  , ServerState (..)
  , newServerState

    -- * Config-value resolution (D9)
  , configValueInt
  , configValueText
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Data.IORef (IORef, newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

import Phase.AST (mkValue, unPValue)
import Phase.Storage.Engine (StorageEngine (..))
import Phase.Storage.Lock (LockTable, emptyLockTable)
import Phase.Storage.Engine (readLive)

import Server.Config (ServerConfig)
import Server.Session (SessionMap)

-- | A consistent snapshot: storage engine + lock table + monotone version.
-- Multiple concurrent requests read from the same 'Snapshot' without
-- blocking each other.
data Snapshot = Snapshot
  { snapEngine  :: !StorageEngine
  , snapLocks   :: !LockTable
  , snapVersion :: !Int           -- ^ Incremented on every commit.
  } deriving (Eq)

-- | An 'IORef' cell holding the latest committed snapshot.
-- Written only while holding 'EngineLock'; readable by anyone at any time.
type SnapshotRef = IORef Snapshot

-- | Exclusive commit lock: whoever holds this 'MVar' may append to the WAL
-- and update 'SnapshotRef'.  The value carried is the same as
-- 'SnapshotRef' but authoritative: it may be ahead of the IORef during the
-- brief window between WAL flush and IORef publication.
type EngineLock = MVar Snapshot

-- | All mutable state the server owns.
data ServerState = ServerState
  { ssConfig      :: !ServerConfig   -- ^ Parsed CLI flags (immutable after startup).
  , ssEngineLock  :: !EngineLock     -- ^ Exclusive lock for commits.
  , ssSnapshot    :: !SnapshotRef    -- ^ Lock-free read snapshot.
  , ssSessions    :: !(TVar SessionMap) -- ^ Active session table (M2).
  , ssDbPath      :: !FilePath       -- ^ Path to the .db file (for compaction).
  }

-- | Build 'ServerState' from the initial storage engine after startup.
newServerState :: ServerConfig -> FilePath -> StorageEngine -> IO ServerState
newServerState cfg dbPath eng = do
  let snap = Snapshot { snapEngine = eng, snapLocks = emptyLockTable, snapVersion = 0 }
  lock  <- newMVar snap
  ref   <- newIORef snap
  sessions <- newTVarIO Map.empty
  pure ServerState
    { ssConfig     = cfg
    , ssEngineLock = lock
    , ssSnapshot   = ref
    , ssSessions   = sessions
    , ssDbPath     = dbPath
    }

-- --------------------------------------------------------------------
-- Config-value resolution (D9)
-- --------------------------------------------------------------------

-- | Read @^_CONFIG("server", key)@ from the current live snapshot, parse it
-- as an 'Int', and fall back to the default when absent or unparseable.
-- The CLI flag (already resolved by the caller) takes precedence; pass
-- 'Just n' from the flag to bypass this entirely.
configValueInt :: Text      -- ^ Config key
               -> Int       -- ^ Default value
               -> Maybe Int -- ^ CLI-pinned value (Nothing = consult DB + default)
               -> Snapshot
               -> Int
configValueInt key def mFlag snap =
  case mFlag of
    Just n  -> n
    Nothing ->
      case readLive "_CONFIG" [mkValue "server", mkValue key] (snapEngine snap) of
        Right pv | [(n, "")] <- reads (T.unpack (unPValue pv)), n > 0 -> n
        _                                                              -> def

-- | Same as 'configValueInt' but returns 'Text'.
configValueText :: Text       -- ^ Config key
                -> Text       -- ^ Default value
                -> Maybe Text -- ^ CLI-pinned value
                -> Snapshot
                -> Text
configValueText key def mFlag snap =
  case mFlag of
    Just t  -> t
    Nothing ->
      case readLive "_CONFIG" [mkValue "server", mkValue key] (snapEngine snap) of
        Right pv | not (T.null (unPValue pv)) -> unPValue pv
        _                                     -> def
