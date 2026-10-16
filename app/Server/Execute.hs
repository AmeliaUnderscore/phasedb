{-# LANGUAGE OverloadedStrings #-}

-- | Optimistic snapshot execution with serialized commits (§7, §8).
--
-- Algorithm (D2):
--   1. Read the committed snapshot without holding the engine lock.
--   2. Build the 'Env', run the command under timeout+output cap.
--   3a. Read-only outcome (no WAL records staged, lock table unchanged):
--       publish only session state; no lock needed.
--   3b. Write outcome: take the engine lock ('MVar' acquire).
--        * Version unchanged: flush WAL (masked), update snapshot, release.
--        * Version moved: re-run the command ONCE under the lock (this
--          always commits, because we now hold the lock for the whole run).
--   4. On error or timeout: discard the Env. No side effects escape.
--
-- The flush-and-publish step is masked against async exceptions (§7,
-- "Cancelling a request is safe").
module Server.Execute
  ( ExecOutcome (..)
  , executeOneShot
  , executeSession
    -- * Session lifecycle (lock release, per-session serialization)
  , sweepExpiredSessions
  , closeSession
  , withSessionLock
  ) where

import Control.Concurrent.MVar (modifyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (atomically, modifyTVar')
import Control.Exception (finally, mask, mask_)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import Data.IORef (readIORef, writeIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Timeout (timeout)

import Phase.Exec (RunOutput (..), RunResult (..), runCommandNoFlush)
import Phase.State (Env (..), EvalError (..), initEnvWithStorage)
import Phase.Storage.Engine (StorageEngine (..), flushPendingWal)
import Phase.Storage.Lock (LockTable, releaseAllOwnerLocks)
import Phase.Storage.Types (TxId)

import Server.Session (Session (..), TokenDigest, anyExpired, generateToken, reapExpired)
import Server.State (Snapshot (..), ServerState (..), effSessionIdleSecs, effSessionMaxSecs)

-- | The outcome of executing one command.
data ExecOutcome
  = ExecOk
      { eoOutput     :: !Text
      , eoValue      :: !(Maybe Text)
      , eoLastTx     :: !TxId
      , eoRegistered :: !(Maybe [Text]) -- ^ Non-Nothing for tag registration
      }
  | ExecError !EvalError !TxId
  | ExecTimeout
  | ExecParseError !Text
  deriving (Show)

-- --------------------------------------------------------------------
-- One-shot execution (M1)
-- --------------------------------------------------------------------

-- | Execute a command with no persistent session state.
executeOneShot :: ServerState
               -> Text   -- ^ Authenticated username
               -> Text   -- ^ Phase source
               -> Text   -- ^ Namespace ("" → "_GLOBAL")
               -> Int    -- ^ Timeout in milliseconds
               -> Int    -- ^ Max output bytes
               -> IO ExecOutcome
executeOneShot ss user cmd ns timeoutMs maxOut = do
  snap  <- readIORef (ssSnapshot ss)
  -- A one-shot request has no session to outlive it, so it gets its own job
  -- id and every lock it took is released when the request ends: LOCK then
  -- works as a test-and-release against locks held by sessions.
  jobId <- T.take 16 <$> generateToken
  let env0 = (mkEnv user ns (snapEngine snap) (snapLocks snap) maxOut) { envJobId = jobId }
      releaseOwn e = e { envLocks = releaseAllOwnerLocks jobId (envLocks e) }
  fst <$> runWithSnapshotExec ss snap cmd ns env0 timeoutMs releaseOwn

-- --------------------------------------------------------------------
-- Session execution (M2)
-- --------------------------------------------------------------------

-- | Execute a command inside a persistent session, returning the updated
-- session 'Env' alongside the outcome.  The caller is responsible for
-- storing the env back in the session table on success.
executeSession :: ServerState
               -> Text   -- ^ Authenticated username (re-validated)
               -> Env    -- ^ Session env (locals, namespace, prior locks)
               -> Text   -- ^ Phase source
               -> Text   -- ^ Namespace override ("" → session default)
               -> Int    -- ^ Timeout in milliseconds
               -> Int    -- ^ Max output bytes
               -> IO (ExecOutcome, Env)
executeSession ss user sessEnv cmd ns timeoutMs maxOut = do
  snap <- readIORef (ssSnapshot ss)
  let ns'  = if T.null ns then envCurrentNamespace sessEnv else ns
      env0 = sessEnv
               { envStorage        = snapEngine snap
               , envGlobals        = seLive (snapEngine snap)
               , envLocks          = snapLocks snap
               , envFileIOEnabled  = False
               , envMaxOutputBytes = Just maxOut
               , envCurrentUser    = user
               , envCurrentNamespace = ns'
               }
  (outcome, mEnv) <- runWithSnapshotExec ss snap cmd ns' env0 timeoutMs id
  -- On success the session continues from the env the run (or the re-run
  -- that committed) produced, so locals set by one request are visible to
  -- the next.  On error/timeout the session env is unchanged (atomicity).
  pure (outcome, fromMaybe sessEnv mEnv)

-- --------------------------------------------------------------------
-- Core snapshot-execution loop
-- --------------------------------------------------------------------

-- | Longest a re-run under the engine lock may take, as a fraction
-- (1/n) of the request's own timeout.  The re-run holds the lock for its
-- whole duration, so it gets a smaller cap than the optimistic first run.
-- Placeholder pending a decision on the actual value.
rerunCapDivisor :: Int
rerunCapDivisor = 2

-- | Run a command on a snapshot and commit if necessary.
--
-- Runs with 'runCommandNoFlush': the library must NOT append to the WAL
-- here, because this run holds no lock.  Flushing happens only in
-- 'commitFlush', under the engine lock.
--
-- The @fin@ function is applied to the environment a run produces, before
-- deciding whether the run was a write (the one-shot path uses it to drop
-- the request's own locks).
runWithSnapshotExec :: ServerState -> Snapshot -> Text -> Text -> Env -> Int -> (Env -> Env) -> IO (ExecOutcome, Maybe Env)
runWithSnapshotExec ss snap0 cmd ns env0 timeoutMs fin = do
  mResult <- timeout (timeoutMs * 1000) (runCommandNoFlush cmd ns env0)
  case mResult of
    Nothing     -> pure (ExecTimeout, Nothing)
    Just result -> handleResult ss snap0 env0 cmd ns timeoutMs fin result

-- | Decide what to do with a 'RunResult'.  The 'Env' is the post-run
-- environment on success (what a session continues from), 'Nothing' on
-- any failure.
handleResult :: ServerState -> Snapshot -> Env -> Text -> Text -> Int -> (Env -> Env) -> RunResult -> IO (ExecOutcome, Maybe Env)
handleResult _ss _snap0 _env0 _cmd _ns _timeoutMs _fin (RunParseError msg) =
  pure (ExecParseError msg, Nothing)

handleResult _ss snap0 _env0 _cmd _ns _timeoutMs _fin (RunError err _) =
  pure (ExecError err (seLastTx (snapEngine snap0)), Nothing)

handleResult ss snap0 env0 cmd ns timeoutMs fin (RunOk ro) =
  let (envRaw, finish) = splitRunOutput ro
      env' = fin envRaw
  in if isWrite snap0 env'
       then commitWrite ss snap0 env0 cmd ns timeoutMs fin finish env'
       else pure (finish (seLastTx (envStorage env')), Just env')

-- | The resulting 'Env' of a successful run, and how to report it once
-- the final TxId is known.  Tag registration writes the catalog, so it
-- goes through the commit path like any other write.
splitRunOutput :: RunOutput -> (Env, TxId -> ExecOutcome)
splitRunOutput (RunOutput output val _tx env') =
  (env', \tx -> ExecOk output val tx Nothing)
splitRunOutput (RunRegistered names _tx env') =
  (env', \tx -> ExecOk "" Nothing tx (Just names))

-- | Did this run stage anything that must be committed?  A write is any
-- run that left WAL records pending, advanced the last TxId, or changed
-- the lock table.
isWrite :: Snapshot -> Env -> Bool
isWrite snap env =
  not (null (sePendingWal (envStorage env)))
  || seLastTx (envStorage env) /= seLastTx (snapEngine snap)
  || envLocks env /= snapLocks snap

-- --------------------------------------------------------------------
-- Commit path
-- --------------------------------------------------------------------

-- | Take the engine lock and commit.  If the version moved while we were
-- executing, re-run the command once under the lock, starting from the
-- ORIGINAL pre-run environment ('Env' @env0@) on the latest state, never
-- from the first run's result (that would apply session-local changes
-- twice).
commitWrite :: ServerState -> Snapshot -> Env -> Text -> Text -> Int -> (Env -> Env)
            -> (TxId -> ExecOutcome) -> Env -> IO (ExecOutcome, Maybe Env)
commitWrite ss snap0 env0 cmd ns timeoutMs fin finish env =
  modifyMVar (ssEngineLock ss) $ \locked ->
    if snapVersion locked == snapVersion snap0
      then -- Happy path: version unchanged, flush and publish.
        commitFlush ss locked env finish
      else do
        -- Version moved: re-run once under the lock (guaranteed to commit).
        let eng' = snapEngine locked
            lks' = snapLocks  locked
            envR = env0 { envStorage = eng', envGlobals = seLive eng', envLocks = lks' }
            capMs = max 1 (timeoutMs `div` rerunCapDivisor)
        mRe <- timeout (capMs * 1000) (runCommandNoFlush cmd ns envR)
        case mRe of
          Nothing -> pure (locked, (ExecTimeout, Nothing))
          Just (RunOk ro) -> do
            let (envRRaw, finishR) = splitRunOutput ro
                envR' = fin envRRaw
            if isWrite locked envR'
              then commitFlush ss locked envR' finishR
              else pure (locked, (finishR (seLastTx (envStorage envR')), Just envR'))
          Just (RunError err _) ->
            pure (locked, (ExecError err (seLastTx eng'), Nothing))
          Just (RunParseError msg) ->
            pure (locked, (ExecParseError msg, Nothing))

-- | Flush pending WAL (masked) and atomically update the snapshot.
-- Returns the new snapshot (for the MVar) and the outcome.
commitFlush :: ServerState -> Snapshot -> Env -> (TxId -> ExecOutcome) -> IO (Snapshot, (ExecOutcome, Maybe Env))
commitFlush ss locked env finish = do
  storage' <- mask_ (flushPendingWal (envStorage env))
  let snap' = Snapshot
        { snapEngine  = storage'
        , snapLocks   = envLocks env
        , snapVersion = snapVersion locked + 1
        }
  writeIORef (ssSnapshot ss) snap'
  pure (snap', (finish (seLastTx storage'), Just env { envStorage = storage' }))

-- --------------------------------------------------------------------
-- Session lifecycle: lock release and per-session serialization
-- --------------------------------------------------------------------

-- | Remove every expired session and release the locks it held.  Called at
-- the start of every request (except health), before the request takes its
-- snapshot, so no request ever observes a lock whose owner has expired.  The
-- common case (nothing expired) is one read of the session table.
--
-- Removal and release happen together under the engine lock, so a second
-- request that finds nothing left to remove only proceeds once the first
-- one has published the released locks.
sweepExpiredSessions :: ServerState -> IO ()
sweepExpiredSessions ss = do
  snap <- readIORef (ssSnapshot ss)
  let cfg   = ssConfig ss
      idleS = effSessionIdleSecs cfg snap
      maxS  = effSessionMaxSecs  cfg snap
  expired <- anyExpired idleS maxS (ssSessions ss)
  when expired $
    modifyMVar (ssEngineLock ss) $ \locked -> do
      dead <- reapExpired idleS maxS (ssSessions ss)
      releaseLocksOf ss locked (map sessJobId dead)

-- | Log a session out: remove it and release its locks.  The caller holds
-- the session's own lock ('withSessionLock'), so no request of that session
-- is still running.
closeSession :: ServerState -> TokenDigest -> Session -> IO ()
closeSession ss digest sess =
  modifyMVar (ssEngineLock ss) $ \locked -> do
    atomically (modifyTVar' (ssSessions ss) (Map.delete digest))
    releaseLocksOf ss locked [sessJobId sess]

-- | Drop the locks owned by the given job ids from the committed lock
-- table.  Publishes a new snapshot (a write) only if some lock was held.
releaseLocksOf :: ServerState -> Snapshot -> [Text] -> IO (Snapshot, ())
releaseLocksOf ss locked jobs =
  let locks' = foldr releaseAllOwnerLocks (snapLocks locked) jobs
  in if locks' == snapLocks locked
       then pure (locked, ())
       else do
         let snap' = locked { snapLocks = locks', snapVersion = snapVersion locked + 1 }
         writeIORef (ssSnapshot ss) snap'
         pure (snap', ())

-- | Run an action holding the session's own lock (D6: two requests for one
-- session never run at the same time).  Waits at most @waitMs@ for the lock;
-- 'Nothing' means it timed out and the action did not run.  The lock is
-- taken with asynchronous exceptions masked and always handed back.
withSessionLock :: Int -> Session -> IO a -> IO (Maybe a)
withSessionLock waitMs sess act = mask $ \restore -> do
  got <- timeout (waitMs * 1000) (takeMVar (sessLock sess))
  case got of
    Nothing -> pure Nothing
    Just () -> (Just <$> restore act) `finally` putMVar (sessLock sess) ()

-- --------------------------------------------------------------------
-- Helpers
-- --------------------------------------------------------------------

-- | Build a fresh 'Env' for an authenticated request.
mkEnv :: Text -> Text -> StorageEngine -> LockTable -> Int -> Env
mkEnv user ns eng lks maxOut =
  (initEnvWithStorage eng)
    { envCurrentUser      = user
    , envCurrentNamespace = if T.null ns then "_GLOBAL" else ns
    , envLocks            = lks
    , envFileIOEnabled    = False
    , envMaxOutputBytes   = Just maxOut
    }
