{-# LANGUAGE OverloadedStrings #-}

-- | Library-level command execution for PhaseDB.
--
-- Extracts the "parse and run" logic from @app/Main.hs@ into a proper
-- library function that returns a structured result rather than printing
-- to stdout.  The REPL and the future HTTP server are both clients of
-- this module; neither duplicates the execution path.
--
-- Also exposes the admin operations that were previously baked into REPL
-- dot-commands (.checkpoint, .compact, .adduser), so the HTTP API can
-- call them through the same code path with the same permission checks.
module Phase.Exec
  ( -- * Structured execution result
    RunResult (..)
  , RunOutput (..)

    -- * Execute Phase source
  , runCommand
  , runCommandNoFlush

    -- * Admin operations
  , adminCheckpoint
  , adminCompact
  , adminAddUser
  , AdminError (..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)

import Phase.AST (Line (..), PredStmt (..), Stmt (..), VarKind (..), mkValue, unPValue)
import Phase.Auth
  ( Role (..)
  , checkAdminAccess
  , generateSalt
  , hashPassword
  , lookupUserRole
  , roleFromText
  , roleToText
  )
import Phase.Catalog (parseSignature, registerRoutineTags)
import Phase.Eval (evalExpr, evalState, runRoutine)
import Phase.Lines (LogicalLine (..), buildLogicalLines)
import Phase.State
  ( Env (..)
  , EvalError
  , clearOutput
  , formatError
  , getOutputText
  , writeRef
  )
import Phase.Storage.Engine
  ( StorageEngine (..)
  , compactTo
  , flushPendingWal
  , openStorageEngine
  , takeCheckpoint
  )
import Phase.Storage.Types (TxId)
import Phase.Stmt (parseLogicalLine)

-- --------------------------------------------------------------------
-- Result types
-- --------------------------------------------------------------------

-- | The kind of successful output a 'runCommand' call produced.
data RunOutput
  = -- | Normal execution: buffered WRITE output and the last committed tx.
    RunOutput
      { roOutput   :: !Text   -- ^ Everything written by WRITE statements.
      , roValue    :: !(Maybe Text)
        -- ^ Non-Nothing when the input was a single bare expression: its
        --   evaluated value (the REPL's "- : value" echo, minus the label).
      , roLastTx   :: !TxId   -- ^ 'seLastTx' after the run.
      , roEnv      :: !Env    -- ^ The resulting environment (to thread back into the session).
      }
  | -- | Routine-tag registration (column-0 tag definitions).
    RunRegistered
      { rrNames  :: ![Text]  -- ^ "NS.NAME" for each tag defined.
      , rrLastTx :: !TxId
      , rrEnv    :: !Env
      }
  deriving (Show)

-- | The outcome of a single 'runCommand' call.
data RunResult
  = RunOk    !RunOutput           -- ^ Phase ran and succeeded.
  | RunError !EvalError !Env      -- ^ Phase ran and returned an error;
                                  --   the env is the ORIGINAL one (no side
                                  --   effects, matching REPL behaviour).
  | RunParseError !Text           -- ^ The source could not be parsed.
  deriving (Show)

-- --------------------------------------------------------------------
-- Core execution function
-- --------------------------------------------------------------------

-- | Parse and execute Phase source text inside the given environment.
--
-- This is the library version of the REPL's @runInput@.  It returns a
-- structured 'RunResult' and never prints anything; all output is
-- captured in 'roOutput'.
--
-- The @namespace@ argument overrides the session's current namespace for
-- this call only when it is non-empty; pass @""@ to keep 'envCurrentNamespace'.
--
-- Committed work is flushed to the WAL before this returns (the REPL's
-- behaviour).  A caller that must serialize its own commits, such as the
-- HTTP server, uses 'runCommandNoFlush' instead.
runCommand :: Text        -- ^ Phase source (one or more logical lines)
           -> Text        -- ^ Namespace override ("" = keep session namespace)
           -> Env         -- ^ Session environment BEFORE this call
           -> IO RunResult
runCommand = runCommandWith flushEnv

-- | Like 'runCommand', but leaves staged WAL records in
-- 'sePendingWal' instead of appending them to disk.
--
-- The caller owns the flush: it must call 'flushPendingWal' on the
-- resulting 'envStorage' under whatever lock serializes its commits,
-- or discard the result (a discarded run then has no effect on disk).
-- Used by the HTTP server (spec §7/§8, decision D2), whose write
-- detection and commit lock depend on writes still being pending when
-- the run returns.
runCommandNoFlush :: Text -> Text -> Env -> IO RunResult
runCommandNoFlush = runCommandWith pure

-- | Shared implementation: the first argument decides what happens to
-- staged WAL records once a run has succeeded.
runCommandWith :: (Env -> IO Env) -> Text -> Text -> Env -> IO RunResult
runCommandWith flush input nsOverride env0 = do
  now <- getCurrentTime
  let ns   = if T.null nsOverride then envCurrentNamespace env0 else nsOverride
      env  = env0 { envNow = now, envCurrentNamespace = ns }
      lls  = buildLogicalLines input
  case lls of
    [] -> pure (RunOk RunOutput
                  { roOutput = "", roValue = Nothing
                  , roLastTx = seLastTx (envStorage env), roEnv = env })
    _  ->
      let hasTagDef = any (\ll -> case llTag ll of Just _ -> True; Nothing -> False) lls
      in if hasTagDef
           then registerTags flush ns input lls env
           else executeLines flush lls env

-- | Handle routine-tag (column-0 definition) input.
registerTags :: (Env -> IO Env) -> Text -> Text -> [LogicalLine] -> Env -> IO RunResult
registerTags flush ns input lls env =
  case registerRoutineTags ns input env of
    Left err   -> pure (RunError err env)
    Right env' -> do
      env'' <- flush env'
      let names = [ T.concat [ns, ".", fst (parseSignature t)]
                  | LogicalLine _ (Just t) _ <- lls ]
      pure (RunOk RunRegistered
              { rrNames  = names
              , rrLastTx = seLastTx (envStorage env'')
              , rrEnv    = env'' })

-- | Handle ordinary Phase statement / bare-expression input.
executeLines :: (Env -> IO Env) -> [LogicalLine] -> Env -> IO RunResult
executeLines flush lls env =
  case mapM parseLogicalLine lls of
    Left err -> pure (RunParseError (T.pack ("parse error: " ++ show err)))
    Right [Line _ _ [PredStmt Nothing (SExpr expr)]] -> do
      -- Single bare expression: evaluate without side effects.
      evalResult <- evalState (evalExpr expr) env
      case evalResult of
        Left err  -> pure (RunError err env)
        Right val ->
          pure (RunOk RunOutput
                  { roOutput = "", roValue = Just (unPValue val)
                  , roLastTx = seLastTx (envStorage env), roEnv = env })
    Right astLines -> do
      routineResult <- runRoutine astLines env
      case routineResult of
        Left err   -> pure (RunError err env)
        Right env' -> do
          env'' <- flush env'
          let out = getOutputText env''
          pure (RunOk RunOutput
                  { roOutput = out
                  , roValue  = Nothing
                  , roLastTx = seLastTx (envStorage env'')
                  , roEnv    = clearOutput env'' })

-- | Flush pending WAL records from the environment's storage engine.
flushEnv :: Env -> IO Env
flushEnv env = do
  storage' <- flushPendingWal (envStorage env)
  pure env { envStorage = storage' }

-- --------------------------------------------------------------------
-- Admin operations
-- --------------------------------------------------------------------

-- | Errors returned by admin operations.
data AdminError
  = AdminPermissionDenied !Text  -- ^ The user does not have admin role.
  | AdminStorageError !Text      -- ^ An underlying storage failure.
  | AdminUserExists !Text        -- ^ '.adduser': the user already exists.
  | AdminBadRole !Text           -- ^ '.adduser': unrecognised role text.
  | AdminOther !Text             -- ^ Anything else.
  deriving (Eq, Show)

-- | Write a WAL checkpoint (equivalent to the REPL's @.checkpoint@ /
-- @.save@ dot-command).  Any authenticated user may call this; it
-- carries no admin-only guard in the original REPL either.
--
-- Returns the updated 'StorageEngine' on success.
adminCheckpoint :: Env -> IO (Either AdminError StorageEngine)
adminCheckpoint env = do
  storage' <- takeCheckpoint (envStorage env)
  pure (Right storage')

-- | Compact the database up to a given transaction number (equivalent
-- to the REPL's @.compact <tx>@ dot-command).  Requires admin role.
--
-- On success returns the backup path that was created and a freshly
-- opened 'StorageEngine' pointing at the compacted file.
adminCompact :: TxId            -- ^ Compact all history before this tx.
             -> FilePath        -- ^ The database file path (needed to reopen).
             -> Env
             -> IO (Either AdminError (FilePath, StorageEngine))
adminCompact tx dbPath env =
  case checkAdminAccess (envCurrentUser env) (envStorage env) of
    Just reason -> pure (Left (AdminPermissionDenied reason))
    Nothing -> do
      result <- compactTo dbPath tx
      case result of
        Left err -> pure (Left (AdminStorageError (T.pack (show err))))
        Right backupPath -> do
          reopened <- openStorageEngine dbPath
          case reopened of
            Left openErr -> pure (Left (AdminStorageError (T.pack (show openErr))))
            Right storage' -> pure (Right (backupPath, storage'))

-- | Create a new user account (equivalent to the REPL's @.adduser@
-- dot-command).  Requires admin role.
adminAddUser :: Text    -- ^ New username.
             -> Text    -- ^ New password (plaintext; hashed here).
             -> Text    -- ^ Role text: "admin", "write", "read", or "none".
             -> Env
             -> IO (Either AdminError Env)
adminAddUser username password roleText env =
  case checkAdminAccess (envCurrentUser env) (envStorage env) of
    Just reason -> pure (Left (AdminPermissionDenied reason))
    Nothing
      | roleText `notElem` ["admin", "write", "read", "none"] ->
          pure (Left (AdminBadRole roleText))
      | lookupUserRole username (envStorage env) /= RNone ->
          pure (Left (AdminUserExists username))
      | otherwise -> do
          salt <- generateSalt
          let hash   = hashPassword password salt
              writes =
                [ (mkValue "role",         mkValue (roleToText (roleFromText roleText)))
                , (mkValue "passwordHash", mkValue hash)
                , (mkValue "passwordSalt", mkValue salt)
                ]
              applyWrite envE (field, val) =
                writeRef Global "_USERS" [mkValue username, field] val envE
              result = foldl (\acc w -> acc >>= flip applyWrite w) (Right env) writes
          case result of
            Left err   -> pure (Left (AdminOther (formatError err)))
            Right env' -> do
              env'' <- flushEnv env'
              pure (Right env'')
