{-# LANGUAGE OverloadedStrings #-}

-- | PhaseDB HTTP server entry point.
--
-- Startup sequence:
--   1. Parse CLI flags ('Server.Config.parseArgs').
--   2. Acquire the cross-process file lock ('Phase.Storage.Engine.acquireDbLock').
--   3. Open the storage engine ('Phase.Storage.Engine.openStorageEngine').
--   4. Build 'Server.State.ServerState'.
--   5. Start Warp (or Warp-TLS if --tls-cert/--tls-key are given).
--   6. On shutdown: release the file lock.
module Main (main) where

import Control.Exception (bracket, catch, SomeException)
import Data.String (fromString)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)

import Phase.Storage.Engine (acquireDbLock, openStorageEngine, releaseDbLock)

import Server.Auth (newLoginGuard)
import Server.Config (ServerConfig (..), parseArgs)
import Server.Routes (mkApp)
import Server.State (newServerState)

main :: IO ()
main = do
  cfg <- parseArgs

  let dbPath = cfgDbPath cfg
  -- Step 2: exclusive file lock.
  mLock <- acquireDbLock dbPath
  case mLock of
    Nothing -> do
      hPutStrLn stderr $ "phasedb-server: " ++ dbPath ++ " is locked by another process."
      exitFailure
    Just dbLock ->
      (`catch` (\e -> releaseDbLock dbLock >> ioError e)) $
      bracket (pure dbLock) releaseDbLock $ \_ -> do
        -- Step 3: open the database.
        engResult <- openStorageEngine dbPath
        case engResult of
          Left err -> do
            hPutStrLn stderr $ "phasedb-server: cannot open " ++ dbPath ++ ": " ++ show err
            exitFailure
          Right eng -> do
            -- Step 4: server state.
            ss     <- newServerState cfg dbPath eng
            guard  <- newLoginGuard 4  -- default 4 concurrent PBKDF2s (§5.3)
            -- Step 5: run Warp.
            let app      = mkApp ss guard
                settings = setHost (fromString (cfgListenAddr cfg))
                         $ setPort (cfgPort cfg)
                         $ defaultSettings
            hPutStrLn stderr $
              "phasedb-server: listening on " ++ cfgListenAddr cfg ++ ":" ++ show (cfgPort cfg)
            runSettings settings app

