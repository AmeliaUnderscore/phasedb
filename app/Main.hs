{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime)
import System.Directory (getHomeDirectory)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, hPutStrLn, stderr, stdout)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline
  ( InputT, Settings (..), defaultSettings
  , getInputLine, getPassword, outputStrLn
  , runInputT, withInterrupt, handleInterrupt
  , completeWord, simpleCompletion
  , modifyHistory
  )
import System.Console.Haskeline.Completion (Completion)
import System.Console.Haskeline.History (addHistoryUnlessConsecutiveDupe)
import Phase.AST (Line (..), PredStmt (..), Stmt (..), VarKind (..), mkValue, unPValue)
import Phase.Auth (Role (..), authenticateUser, bootstrapCreateUser, checkAdminAccess, generateSalt, hashPassword, hasAnyUser, lookupUserRole, roleFromText, roleToText)
import Phase.Catalog
  ( getItemHistory
  , listItems
  , listNamespaces
  , parseSignature
  , registerRoutineTags
  , revertItem
  )
import Phase.Eval (evalExpr, evalState, runRoutine)
import Phase.Lines (LogicalLine (..), buildLogicalLines, needsContinuation)
import Phase.State (Env (..), clearOutput, formatError, getOutputText, initEnv, writeRef)
import Phase.Storage.Engine
  ( StorageEngine (..)
  , acquireDbLock
  , compactTo
  , flushPendingWal
  , openStorageEngine
  , releaseDbLock
  , takeCheckpoint
  )
import Phase.Stmt (parseLogicalLine)

-- --------------------------------------------------------------------
-- ANSI helpers (no extra dependency, escape sequences only)
-- --------------------------------------------------------------------

-- | Reset all SGR attributes.
ansiReset :: String
ansiReset = "\ESC[0m"

ansiRed, ansiGreen, ansiCyan, ansiBold, ansiDim :: String
ansiRed   = "\ESC[31m"
ansiGreen = "\ESC[32m"
ansiCyan  = "\ESC[36m"
ansiBold  = "\ESC[1m"
ansiDim   = "\ESC[2m"

coloured :: String -> String -> String
coloured code s = code ++ s ++ ansiReset

errorMsg :: Text -> String
errorMsg t = coloured ansiRed (T.unpack t)

resultMsg :: Text -> String
resultMsg t = coloured ansiGreen ("- : " ++ T.unpack t)

infoMsg :: String -> String
infoMsg s = coloured ansiCyan s

dimMsg :: String -> String
dimMsg s = coloured ansiDim s

-- | Build the primary prompt string (not printed through haskeline's
-- prompt argument here, used as the label passed to getInputLine).
mkPrompt :: Text -> String
mkPrompt ns
  | ns == "_GLOBAL" = coloured (ansiBold ++ ansiCyan) "phase" ++ coloured ansiBold "# "
  | otherwise       = coloured (ansiBold ++ ansiCyan) ("phase[" ++ T.unpack ns ++ "]") ++ coloured ansiBold "# "

-- | Continuation prompt shown while accumulating a multi-line block.
contPrompt :: String
contPrompt = coloured ansiDim "...# "

-- --------------------------------------------------------------------
-- Tab completion
-- --------------------------------------------------------------------

-- | Phase keywords and REPL dot-commands offered for completion.
phaseKeywords :: [String]
phaseKeywords =
  [ "SET", "KILL", "WRITE", "IF", "ELSE", "FOR", "EACH", "IN", "DO", "END"
  , "GOTO", "QUIT", "RETURN", "NEW", "MERGE", "LOCK", "UNLOCK"
  , "CALL", "NAMESPACE", "TRANSACTION", "HANDLE", "CATCH", "RETRY", "SIGNAL"
  , "EVALUATE", "WHEN", "OTHER", "CLOSURE", "CAPTURING"
  , "$DATA", "$ORDER", "$GET", "$LENGTH", "$EXTRACT", "$PIECE"
  , "$TEST", "$JOB", "$TX", "$CHILDCOUNT"
  ]

dotCommands :: [String]
dotCommands =
  [ ".quit", ".exit", ".ns", ".namespaces", ".fns", ".history"
  , ".revert", ".checkpoint", ".save", ".compact", ".adduser", ".help"
  ]

phaseCompletion :: String -> [String]
phaseCompletion prefix
  | "." `T.isPrefixOf` T.pack prefix =
      filter (T.unpack (T.pack prefix) `isPrefixOfStr`) dotCommands
  | "$" `T.isPrefixOf` T.pack prefix =
      filter (isPrefixOfStr prefix) phaseKeywords
  | otherwise =
      filter (isPrefixOfStr prefix) phaseKeywords
  where
    isPrefixOfStr p s = take (length p) s == p

replCompletion :: String -> IO [Completion]
replCompletion prefix =
  pure (map simpleCompletion (phaseCompletion prefix))

-- --------------------------------------------------------------------
-- Multi-line continuation detection
-- --------------------------------------------------------------------

-- (needsContinuation lives in Phase.Lines so it can be tested; see there.)

-- --------------------------------------------------------------------
-- Entry point
-- --------------------------------------------------------------------

-- | Phase REPL (handbook Part XIV):
-- Maintains session state (locals and in-memory globals), executes statements,
-- prints WRITE output, evaluates/echoes bare expressions with "- : <value>",
-- and manages ^_CATALOG namespaces, functions, and version history.
--
-- Usage: phasedb-hs [dbfile]
--   dbfile defaults to "phase.db" in the current directory.
--
-- == Login / bootstrap
--
-- Every database is guaranteed to have at least one admin account,
-- never "wide open", by construction: if the @.db@ file does not
-- exist yet when this process starts, that means a BRAND NEW database
-- is about to be created, and the very first thing written to it is
-- an admin account (named @SYSTEM@ by default, editable), created
-- interactively right here before the REPL proper ever starts. An
-- EXISTING file instead goes through an ordinary username/password
-- login prompt. See 'bootstrapFlow'/'loginFlow'.
main :: IO ()
main = do
  args <- getArgs
  let dbPath = case args of
                 (p : _) -> p
                 []      -> "phase.db"
  putStrLn $ infoMsg "PhaseDB" ++ "  database: " ++ dimMsg dbPath
  -- Acquire the cross-process exclusive file lock (§9.5 of the HTTP
  -- spec) BEFORE opening the database. If another process (e.g.
  -- phasedb-server) already holds it, we refuse to start so neither
  -- process can corrupt the WAL by appending concurrently.
  mLock <- acquireDbLock dbPath
  case mLock of
    Nothing -> do
      hPutStrLn stderr $ "Error: " ++ dbPath ++ " is locked by another process."
      hPutStrLn stderr "Stop the other phasedb or phasedb-server process first."
      exitFailure
    Just dbLock -> do
      engResult <- openStorageEngine dbPath
      histFile <- (++ "/.phasedb_history") <$> getHomeDirectory
      let settings = (defaultSettings :: Settings IO)
            { historyFile    = Just histFile
            , autoAddHistory = False       -- we add manually for fine-grained control
            , complete       = completeWord Nothing " \t" replCompletion
            }
      case engResult of
        Left err -> do
          -- Refuse to continue. Starting "fresh" here would keep
          -- 'seWalPath' pointed at the damaged or foreign file, and the
          -- first commit would append into it.
          releaseDbLock dbLock
          hPutStrLn stderr $ "Error: could not open " ++ dbPath ++ ": " ++ show err
          hPutStrLn stderr "The file has not been modified. Restore a backup, or move it aside to start a new database."
          exitFailure
        Right eng -> do
          putStrLn $ "Loaded " ++ show (seLastTx eng) ++ " transactions from " ++ dimMsg dbPath
          runInputT settings $ do
            -- Bootstrap whenever the database has NO admin account yet, not
            -- only when the file itself is brand new: an existing file that
            -- somehow ended up with zero users (created empty by some other
            -- tool, or left behind by a first run that was aborted before
            -- the admin account was created) must not be stuck in
            -- 'loginFlow' forever with nobody able to log in.
            mEnv <-
              if hasAnyUser eng
                then loginFlow eng
                else bootstrapFlow dbPath eng
            case mEnv of
              Nothing  -> pure ()
              Just env -> loop dbPath env
          releaseDbLock dbLock
  where
    -- | Flush any pending WAL records from the env's storage engine to disk.
    flushEnv :: Env -> IO Env
    flushEnv env = do
      storage' <- flushPendingWal (envStorage env)
      pure env { envStorage = storage' }

    -- | Run on a brand-new database (the @.db@ file did not exist
    -- before this process opened it). Creates the first admin account
    -- interactively, 'Nothing' means the user aborted (Ctrl-D), in
    -- which case 'main' simply exits without writing a half-configured
    -- database.
    bootstrapFlow :: FilePath -> StorageEngine -> InputT IO (Maybe Env)
    bootstrapFlow dbPath eng = do
      outputStrLn (infoMsg "No admin account exists yet. Creating first admin user.")
      mUsername <- getInputLine "Admin username [SYSTEM]: "
      case mUsername of
        Nothing -> pure Nothing
        Just raw -> do
          let username = let t = T.strip (T.pack raw)
                         in if T.null t then "SYSTEM" else t
          mPassword <- promptPasswordHaskeline "Admin password: "
          case mPassword of
            Nothing -> pure Nothing
            Just pwd1 -> do
              mPassword2 <- promptPasswordHaskeline "Confirm password: "
              case mPassword2 of
                Nothing -> pure Nothing
                Just pwd2
                  | pwd1 /= pwd2 -> do
                      outputStrLn (errorMsg "Passwords did not match. Try again.")
                      bootstrapFlow dbPath eng
                  | otherwise -> do
                      now <- liftIO getCurrentTime
                      result <- liftIO $ bootstrapCreateUser username pwd1 RAdmin now eng
                      case result of
                        Left err -> do
                          outputStrLn (errorMsg ("Failed to create admin account: " <> err))
                          pure Nothing
                        Right eng' -> do
                          eng'' <- liftIO $ flushPendingWal eng'
                          outputStrLn (infoMsg ("Created admin account " ++ T.unpack username ++ "."))
                          pure (Just initEnv
                            { envStorage     = eng''
                            , envGlobals     = seLive eng''
                            , envCurrentUser = username
                            })

    -- | Run on an existing database: ordinary username/password login,
    -- retried until success or EOF.
    loginFlow :: StorageEngine -> InputT IO (Maybe Env)
    loginFlow eng = do
      mUsername <- getInputLine "Username: "
      case mUsername of
        Nothing -> pure Nothing
        Just raw -> do
          let username = T.strip (T.pack raw)
          mPassword <- promptPasswordHaskeline "Password: "
          case mPassword of
            Nothing -> pure Nothing
            Just password ->
              if authenticateUser username password eng
                then do
                  outputStrLn (infoMsg ("Logged in as " ++ T.unpack username ++ "."))
                  pure (Just initEnv
                    { envStorage     = eng
                    , envGlobals     = seLive eng
                    , envCurrentUser = username
                    })
                else do
                  outputStrLn (errorMsg "Invalid username or password.")
                  loginFlow eng

    -- | Read a password line with no echo via haskeline's own getPassword.
    promptPasswordHaskeline :: String -> InputT IO (Maybe Text)
    promptPasswordHaskeline label = do
      mLine <- getPassword (Just '*') label
      pure (fmap T.pack mLine)

    -- | The main REPL loop.
    loop :: FilePath -> Env -> InputT IO ()
    loop dbPath env =
      withInterrupt (handleInterrupt (onInterrupt dbPath env) (loopStep dbPath env))

    -- | On Ctrl-C: discard any partial input and re-prompt.
    onInterrupt :: FilePath -> Env -> InputT IO ()
    onInterrupt dbPath env = do
      outputStrLn (dimMsg "Interrupted.")
      loop dbPath env

    -- | One iteration: read input (possibly multi-line), dispatch.
    loopStep :: FilePath -> Env -> InputT IO ()
    loopStep dbPath env = do
      let ns = envCurrentNamespace env
      mLine <- getInputLine (mkPrompt ns)
      case mLine of
        Nothing -> outputStrLn (dimMsg "Goodbye.")
        Just raw -> do
          let trimmed = T.strip (T.pack raw)
          case trimmed of
            "" -> loop dbPath env

            ".quit" -> outputStrLn (dimMsg "Goodbye.")
            ".exit" -> outputStrLn (dimMsg "Goodbye.")

            ".help" -> do
              printHelp
              loop dbPath env

            -- Multi-line block, GHCi-style: ":{" opens, ":}" closes.
            -- This is kept as an explicit escape hatch for code that
            -- auto-continuation does not catch.
            ":{" -> do
              mBlock <- readBlock []
              case mBlock of
                Nothing -> do
                  outputStrLn (errorMsg "Unexpected end of input inside :{ ... :} block.")
                  loop dbPath env
                Just blockText -> do
                  addToHistory raw
                  env' <- liftIO $ runInput dbPath env blockText
                  loop dbPath env'

            -- Dot-commands
            _ | trimmed == ".ns" -> do
                  outputStrLn (infoMsg ("Current namespace: " ++ T.unpack (envCurrentNamespace env)))
                  loop dbPath env

              | trimmed == ".namespaces" -> do
                  let nss = listNamespaces env
                  outputStrLn (infoMsg "Namespaces in ^_CATALOG:")
                  mapM_ (\ns' -> outputStrLn ("  - " ++ T.unpack ns')) (if null nss then ["_GLOBAL"] else nss)
                  loop dbPath env

              | ".fns" `T.isPrefixOf` trimmed -> do
                  let nsArg    = T.strip (T.drop 4 trimmed)
                      targetNS = if T.null nsArg then envCurrentNamespace env else nsArg
                      items    = listItems targetNS env
                  if null items
                    then outputStrLn (dimMsg ("No functions found in namespace: " ++ T.unpack targetNS))
                    else do
                      outputStrLn (infoMsg ("Items in ^_CATALOG(" ++ T.unpack targetNS ++ "):"))
                      mapM_ (\(name, kind, ver) ->
                        outputStrLn ("  [" ++ show kind ++ " v" ++ show ver ++ "] " ++ T.unpack name)) items
                  loop dbPath env

              | ".history " `T.isPrefixOf` trimmed -> do
                  let args' = T.words (T.drop 9 trimmed)
                  case args' of
                    (name : restNS) -> do
                      let targetNS = case restNS of
                                       (ns' : _) -> ns'
                                       []        -> envCurrentNamespace env
                          history  = getItemHistory targetNS name env
                      if null history
                        then outputStrLn (dimMsg ("No prior version history for " ++ T.unpack targetNS ++ "." ++ T.unpack name))
                        else do
                          outputStrLn (infoMsg ("Version history for " ++ T.unpack targetNS ++ "." ++ T.unpack name ++ ":"))
                          mapM_ (\(v, code) ->
                            outputStrLn ("  v" ++ show v ++ ": " ++ T.unpack code)) history
                    [] -> outputStrLn (errorMsg "Syntax: .history <name> [namespace]")
                  loop dbPath env

              -- Every branch below ends in exactly ONE tail call to
              -- 'loop', with the env that should actually continue.
              -- This used to call 'loop dbPath newEnv' NESTED inside
              -- the branch and then fall through to a further
              -- 'loop dbPath env' (the STALE, pre-revert/pre-compact
              -- env) once the user eventually quit that inner loop --
              -- so '.revert'/'.compact' followed by '.quit' didn't
              -- actually exit, it dropped back into a session whose
              -- 'envStorage' (seLastTx, pending writes) no longer
              -- matched the file on disk.
              | ".revert " `T.isPrefixOf` trimmed -> do
                  let args' = T.words (T.drop 8 trimmed)
                  case args' of
                    (name : verStr : restNS) -> do
                      let targetNS = case restNS of
                                       (ns' : _) -> ns'
                                       []        -> envCurrentNamespace env
                      case reads (T.unpack verStr) of
                        ((vNum, "") : _) ->
                          case revertItem targetNS name vNum env of
                            Left err -> do
                              outputStrLn (errorMsg (formatError err))
                              loop dbPath env
                            Right env' -> do
                              env'' <- liftIO $ flushEnv env'
                              outputStrLn (infoMsg ("Reverted " ++ T.unpack targetNS ++ "." ++ T.unpack name ++ " to version " ++ T.unpack verStr))
                              loop dbPath env''
                        _ -> do
                          outputStrLn (errorMsg "Invalid version number.")
                          loop dbPath env
                    _ -> do
                      outputStrLn (errorMsg "Syntax: .revert <name> <version> [namespace]")
                      loop dbPath env

              | trimmed == ".checkpoint" || trimmed == ".save" -> do
                  storage' <- liftIO $ takeCheckpoint (envStorage env)
                  outputStrLn (infoMsg ("Checkpoint written."))
                  loop dbPath env { envStorage = storage' }

              | ".compact " `T.isPrefixOf` trimmed ->
                  case checkAdminAccess (envCurrentUser env) (envStorage env) of
                    Just reason -> do
                      outputStrLn (errorMsg ("Permission denied: " <> reason))
                      loop dbPath env
                    Nothing -> do
                      let txArg = T.strip (T.drop 9 trimmed)
                      case reads (T.unpack txArg) of
                        ((tx, "") : _) -> do
                          result <- liftIO $ compactTo dbPath tx
                          case result of
                            Left err -> do
                              outputStrLn (errorMsg (T.pack (show err)))
                              loop dbPath env
                            Right backupPath -> do
                              outputStrLn (infoMsg ("Compacted to transaction " ++ show tx ++ "."))
                              outputStrLn (dimMsg ("Pre-compaction backup: " ++ backupPath))
                              reopened <- liftIO $ openStorageEngine dbPath
                              case reopened of
                                Left openErr -> do
                                  -- The file on disk HAS already been
                                  -- rewritten by the compact that just
                                  -- succeeded; 'env' here is now stale
                                  -- too (its seLastTx/history no longer
                                  -- matches what's on disk), but it's
                                  -- the only env available with the
                                  -- reopen itself failing, so it's the
                                  -- least-wrong fallback rather than no
                                  -- fallback at all.
                                  outputStrLn (errorMsg (T.pack (show openErr)))
                                  loop dbPath env
                                Right newStorage ->
                                  loop dbPath (env { envStorage = newStorage, envGlobals = seLive newStorage })
                        _ -> do
                          outputStrLn (errorMsg "Syntax: .compact <transaction-number>")
                          loop dbPath env

              | ".adduser " `T.isPrefixOf` trimmed ->
                  case checkAdminAccess (envCurrentUser env) (envStorage env) of
                    Just reason -> do
                      outputStrLn (errorMsg ("Permission denied: " <> reason))
                      loop dbPath env
                    Nothing ->
                      case T.words (T.drop 9 trimmed) of
                        (username : roleArgs) | not (T.null username) -> do
                          let roleText = case roleArgs of
                                           (r : _) -> T.toLower (T.strip r)
                                           []      -> "write"
                          if roleText `notElem` ["admin", "write", "read", "none"]
                            then do
                              outputStrLn (errorMsg (T.pack ("Unknown role " ++ show roleText ++ ". Use admin, write, read, or none.")))
                              loop dbPath env
                            else if lookupUserRole username (envStorage env) /= RNone
                              -- 'lookupUserRole' defaults to 'RNone' for a
                              -- username with no '^_USERS(username,"role")'
                              -- node at all, so this also catches "exists
                              -- with explicitly no access" as "exists",
                              -- the point is not to silently reset an
                              -- existing account's password on a typo.
                              then do
                                outputStrLn (errorMsg (T.pack ("User " ++ T.unpack username ++ " already exists.")))
                                loop dbPath env
                              else do
                                mPassword <- promptPasswordHaskeline "New user's password: "
                                case mPassword of
                                  Nothing -> loop dbPath env
                                  Just pwd1 -> do
                                    mPassword2 <- promptPasswordHaskeline "Confirm password: "
                                    case mPassword2 of
                                      Nothing -> loop dbPath env
                                      Just pwd2
                                        | pwd1 /= pwd2 -> do
                                            outputStrLn (errorMsg "Passwords did not match.")
                                            loop dbPath env
                                        | otherwise -> do
                                            salt <- liftIO generateSalt
                                            let hash = hashPassword pwd1 salt
                                                result = do
                                                  e1 <- writeRef Global "_USERS" [mkValue username, mkValue "role"] (mkValue (roleToText (roleFromText roleText))) env
                                                  e2 <- writeRef Global "_USERS" [mkValue username, mkValue "passwordHash"] (mkValue hash) e1
                                                  writeRef Global "_USERS" [mkValue username, mkValue "passwordSalt"] (mkValue salt) e2
                                            case result of
                                              Left err -> do
                                                outputStrLn (errorMsg (formatError err))
                                                loop dbPath env
                                              Right env' -> do
                                                -- 'writeRef' only stages the write in 'envStorage's
                                                -- pending WAL, ordinary Phase statements get
                                                -- flushed after each top-level command by 'loop'
                                                -- itself, but a dot-command writing directly (same
                                                -- as '.revert') has to flush explicitly, or the
                                                -- account exists only in THIS process's memory
                                                -- until something else happens to flush it.
                                                env'' <- liftIO $ flushEnv env'
                                                outputStrLn (infoMsg ("Created user " ++ T.unpack username ++ " with role " ++ T.unpack roleText ++ "."))
                                                loop dbPath env''
                        _ -> do
                          outputStrLn (errorMsg "Syntax: .adduser <username> [admin|write|read|none]")
                          loop dbPath env

              -- Standard Phase statements & bare expressions,
              -- with auto-continuation for multi-line blocks.
              | otherwise -> do
                  accumulated <- gatherInput raw
                  addToHistory (T.unpack accumulated)
                  env' <- liftIO $ runInput dbPath env accumulated
                  loop dbPath env'

    -- | Accumulate lines until the current input is "complete" (no
    -- open DO/TRANSACTION blocks and the last line does not end with a
    -- bare DO). Each continuation line is prefixed into the result with
    -- a real newline so that 'buildLogicalLines' sees the correct
    -- physical line structure.
    gatherInput :: String -> InputT IO Text
    gatherInput firstLine = go (T.pack firstLine)
      where
        go acc
          | needsContinuation acc = do
              mMore <- getInputLine contPrompt
              case mMore of
                Nothing   -> pure acc     -- EOF mid-block: execute what we have
                Just more -> go (acc <> "\n" <> T.pack more)
          | otherwise = pure acc

    -- | Read lines for a ":{ ... :}" block until ":}" or EOF.
    readBlock :: [String] -> InputT IO (Maybe Text)
    readBlock acc = do
      mLine <- getInputLine contPrompt
      case mLine of
        Nothing   -> pure Nothing
        Just line ->
          if T.strip (T.pack line) == ":}"
            then pure (Just (T.pack (unlines (reverse acc))))
            else readBlock (line : acc)

    -- | Add to haskeline history, skipping duplicates of the most
    -- recent entry (same behaviour as bash's HISTCONTROL=ignoredups).
    addToHistory :: String -> InputT IO ()
    addToHistory entry = modifyHistory (addHistoryUnlessConsecutiveDupe entry)

    -- | The shared "parse and run" path for standard input.
    runInput :: FilePath -> Env -> Text -> IO Env
    runInput _dbPath env0 input = do
      now <- getCurrentTime
      let env    = env0 { envNow = now }
          curNS  = envCurrentNamespace env
          lls    = buildLogicalLines input
      case lls of
        [] -> pure env
        _  -> do
          let hasTagDef = any (\ll -> case llTag ll of Just _ -> True; Nothing -> False) lls
          if hasTagDef
            then case registerRoutineTags curNS input env of
              Left err -> do
                TIO.putStrLn (T.pack (errorMsg (formatError err)))
                pure env
              Right env' -> do
                let definedTags = [t | LogicalLine _ (Just t) _ <- lls]
                mapM_ (\t -> do
                  let (name, _) = parseSignature t
                  TIO.putStrLn (T.pack (infoMsg ("Registered " ++ T.unpack curNS ++ "." ++ T.unpack name ++ " in ^_CATALOG")))
                  ) definedTags
                flushEnv env'
            else case mapM parseLogicalLine lls of
              Left err -> do
                TIO.putStrLn (T.pack (errorMsg (T.pack ("parse error: " ++ show err))))
                pure env
              Right [Line _ _ [PredStmt Nothing (SExpr expr)]] -> do
                -- Bare expression: evaluate and echo "- : value"
                evalResult <- evalState (evalExpr expr) env
                case evalResult of
                  Left evalErr -> do
                    TIO.putStrLn (T.pack (errorMsg (formatError evalErr)))
                    pure env
                  Right val -> do
                    TIO.putStrLn (T.pack (resultMsg (unPValue val)))
                    pure env
              Right astLines -> do
                routineResult <- runRoutine astLines env
                case routineResult of
                  Left evalErr -> do
                    TIO.putStrLn (T.pack (errorMsg (formatError evalErr)))
                    pure env
                  Right env' -> do
                    env'' <- flushEnv env'
                    let out = getOutputText env''
                    if not (T.null out)
                      then TIO.putStr out >> hFlush stdout
                      else pure ()
                    pure (clearOutput env'')

    -- | Print a brief help summary.
    printHelp :: InputT IO ()
    printHelp = mapM_ outputStrLn
      [ infoMsg "Phase REPL, dot-commands:"
      , "  " ++ dimMsg ".help"           ++ "                    this message"
      , "  " ++ dimMsg ".quit / .exit"   ++ "            exit the REPL  (also Ctrl-D)"
      , "  " ++ dimMsg ".ns"             ++ "                      show current namespace"
      , "  " ++ dimMsg ".namespaces"     ++ "              list all namespaces in ^_CATALOG"
      , "  " ++ dimMsg ".fns [ns]"       ++ "               list functions/constants in a namespace"
      , "  " ++ dimMsg ".history <n> [ns]" ++ "        show version history for a catalog item"
      , "  " ++ dimMsg ".revert <n> <v> [ns]" ++ "   revert a catalog item to an earlier version"
      , "  " ++ dimMsg ".checkpoint"     ++ "              write a WAL checkpoint"
      , "  " ++ dimMsg ".compact <tx>"   ++ "          vacuum history before <tx>  (admin only)"
      , "  " ++ dimMsg ".adduser <name> [role]" ++ "  create a user: admin/write/read/none, default write  (admin only)"
      , ""
      , infoMsg "Multi-line input:"
      , "  End a line with bare " ++ dimMsg "DO" ++ " to open a dot-block, the REPL"
      , "  continues automatically.  Use " ++ dimMsg ":{" ++ " / " ++ dimMsg ":}" ++ " for explicit blocks."
      , "  " ++ dimMsg "Ctrl-C" ++ " discards the current line.  " ++ dimMsg "Ctrl-D" ++ " exits."
      ]

