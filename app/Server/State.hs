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
  , setLogger

    -- * Config-value resolution (D9)
  , configValueInt
  , configValueText
  , configSourceInt

    -- * Bounded engine lock (M3)
  , withBoundedEngineLock
  , tryEngineLock

    -- * Effective limits: flag, then ^_CONFIG(\"server\", name), then default
  , effMaxBodyBytes
  , effExecTimeoutMs
  , effExecTimeoutMaxMs
  , effMaxOutputBytes
  , effSessionIdleSecs
  , effSessionMaxSecs
  , effMaxSessions
  , effQueueTimeoutMs
  ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Concurrent.STM (TVar, newTVarIO)
import Control.Exception (SomeException, catch, mask, throwIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import Data.IORef (IORef, newIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import System.IO (hFlush, stderr)
import System.Timeout (timeout)

import Phase.AST (mkValue, unPValue)
import Phase.Storage.Engine (StorageEngine (..))
import Phase.Storage.Lock (LockTable, emptyLockTable)
import Phase.Storage.Engine (readLive)

import Server.Config
  ( ServerConfig (..)
  , defaultExecTimeoutMaxMs, defaultExecTimeoutMs, defaultMaxBodyBytes
  , defaultMaxOutputBytes, defaultMaxSessions, defaultQueueTimeoutMs
  , defaultSessionIdleSecs, defaultSessionMaxSecs
  )
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
-- Engine lock (commit path)
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
  , ssLogger      :: !(ByteString -> IO ()) -- ^ Access logger (§9.3, default writes to stderr).
  }

-- | Set a custom logger action for the server state.
setLogger :: (ByteString -> IO ()) -> ServerState -> ServerState
setLogger logger ss = ss { ssLogger = logger }

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
    , ssLogger     = \bs -> BSC.hPutStrLn stderr bs >> hFlush stderr
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

-- --------------------------------------------------------------------
-- Effective limits (D9)
-- --------------------------------------------------------------------
--
-- Each limit is resolved on every use, in this order: the command-line
-- flag (when given it pins the value), then @^_CONFIG("server", name)@
-- read from the request's snapshot, then the built-in default.  Reading
-- the snapshot means a committed @SET@ applies from the next request.

effMaxBodyBytes, effExecTimeoutMs, effExecTimeoutMaxMs, effMaxOutputBytes,
  effSessionIdleSecs, effSessionMaxSecs, effMaxSessions, effQueueTimeoutMs
  :: ServerConfig -> Snapshot -> Int
effMaxBodyBytes      cfg = configValueInt "maxBodyBytes"       defaultMaxBodyBytes      (cfgMaxBodyBytes cfg)
effExecTimeoutMs     cfg = configValueInt "execTimeoutMs"      defaultExecTimeoutMs     (cfgExecTimeoutMs cfg)
effExecTimeoutMaxMs  cfg = configValueInt "execTimeoutMaxMs"   defaultExecTimeoutMaxMs  (cfgExecTimeoutMaxMs cfg)
effMaxOutputBytes    cfg = configValueInt "maxOutputBytes"     defaultMaxOutputBytes    (cfgMaxOutputBytes cfg)
effSessionIdleSecs   cfg = configValueInt "sessionIdleSeconds" defaultSessionIdleSecs   (cfgSessionIdleSecs cfg)
effSessionMaxSecs    cfg = configValueInt "sessionMaxSeconds"  defaultSessionMaxSecs    (cfgSessionMaxSecs cfg)
effMaxSessions       cfg = configValueInt "maxSessions"        defaultMaxSessions       (cfgMaxSessions cfg)
effQueueTimeoutMs    cfg = configValueInt "queueTimeoutMs"     defaultQueueTimeoutMs    (cfgQueueTimeoutMs cfg)

-- --------------------------------------------------------------------
-- Config-source resolution (M3 §6.3 status)
-- --------------------------------------------------------------------

-- | The source of a config value: the CLI flag, the database @^_CONFIG@, or the built-in default.
-- Used by @GET /v1/admin/status@ to populate the @config@ object.
configSourceInt :: Text       -- ^ Config key (the @^_CONFIG(\"server\", key)@ name)
                -> Maybe Int  -- ^ CLI-pinned value
                -> Snapshot
                -> Text       -- ^ \"flag\", \"config\", or \"default\"
configSourceInt _key (Just _) _snap = "flag"
configSourceInt key  Nothing  snap  =
  case readLive "_CONFIG" [mkValue "server", mkValue key] (snapEngine snap) of
    Right pv | [(n, "")] <- reads (T.unpack (unPValue pv)), n > (0 :: Int) -> "config"
    _                                                                        -> "default"

-- --------------------------------------------------------------------
-- Bounded engine lock (M3 §6.3, §8)
-- --------------------------------------------------------------------

-- | Take the engine lock and run an action, waiting at most @waitMs@
-- milliseconds.  Returns 'Nothing' if the wait timed out.
--
-- Exception-safe: the lock is always returned to the MVar on any
-- exception from the action, with the snapshot unchanged.
withBoundedEngineLock :: Int            -- ^ Max wait in milliseconds
                      -> EngineLock
                      -> (Snapshot -> IO (Snapshot, a))
                      -> IO (Maybe a)
withBoundedEngineLock waitMs lock action =
  mask $ \restore -> do
    mSnap <- timeout (waitMs * 1000) (takeMVar lock)
    case mSnap of
      Nothing   -> pure Nothing
      Just snap ->
        (do (snap', result) <- restore (action snap)
            putMVar lock snap'
            pure (Just result))
        `catch`
        (\e -> do putMVar lock snap
                  throwIO (e :: SomeException))

-- | Attempt to take the engine lock without waiting (try-lock for the
-- expiry sweep, D12).  If the lock is free, runs the action with it;
-- if it is held, returns '()' immediately without blocking.
tryEngineLock :: EngineLock
              -> (Snapshot -> IO (Snapshot, ()))
              -> IO ()
tryEngineLock lock action = mask $ \restore -> do
  mSnap <- tryTakeMVar lock
  case mSnap of
    Nothing   -> pure ()   -- lock is busy; skip (D12)
    Just snap ->
      (do (snap', ()) <- restore (action snap)
          putMVar lock snap')
      `catch`
      (\e -> do putMVar lock snap
                throwIO (e :: SomeException))


