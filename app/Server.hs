{-# LANGUAGE OverloadedStrings #-}

-- | PhaseDB HTTP server entry point.
--
-- Startup sequence:
--   1. Parse CLI flags ('Server.Config.parseArgs').
--   2. Validate TLS settings ('Server.Config.validateTlsConfig').
--   3. Acquire the cross-process file lock ('Phase.Storage.Engine.acquireDbLock').
--   4. Open the storage engine ('Phase.Storage.Engine.openStorageEngine').
--   5. Build 'Server.State.ServerState'.
--   6. Start Warp (or Warp-TLS if --tls-cert/--tls-key are given).
--   7. On shutdown: release the file lock.
module Main (main) where

import Control.Concurrent (forkFinally)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, tryTakeMVar)
import Control.Exception (AsyncException (UserInterrupt), bracket, catch, throwIO)
import Data.String (fromString)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, setHost, setPort
  , setGracefulShutdownTimeout, setInstallShutdownHandler
  )
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)

import Phase.Storage.Engine (acquireDbLock, openStorageEngine, releaseDbLock)

import Server.Auth (newLoginGuard)
import Server.Config (ServerConfig (..), parseArgs, validateTlsConfig)
import Server.Routes (mkApp)
import Server.State (newServerState)

main :: IO ()
main = do
  cfg <- parseArgs

  -- Step 1: validate TLS configuration before acquiring the DB lock.
  tlsResult <- validateTlsConfig cfg
  mTls <- case tlsResult of
    Left err -> do
      hPutStrLn stderr err
      exitFailure
    Right res -> pure res

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

            -- Step 5: run Warp with graceful shutdown.
            closeActionVar <- newEmptyMVar
            serverDoneVar  <- newEmptyMVar

            let app      = mkApp ss guard
                settings = setHost (fromString (cfgListenAddr cfg))
                         $ setPort (cfgPort cfg)
                         $ setInstallShutdownHandler (\closeSocket -> putMVar closeActionVar closeSocket)
                         $ setGracefulShutdownTimeout (Just 10)
                         $ defaultSettings
                proto    = case mTls of
                             Just _  -> "https"
                             Nothing -> "http"

            hPutStrLn stderr $
              "phasedb-server: listening on " ++ proto ++ "://" ++ cfgListenAddr cfg ++ ":" ++ show (cfgPort cfg)

            let runServer = case mTls of
                  Just (cert, key) -> runTLS (tlsSettings cert key) settings app
                  Nothing          -> runSettings settings app

            _ <- forkFinally runServer (\res -> putMVar serverDoneVar res)

            let waitForShutdown = do
                  res <- takeMVar serverDoneVar
                  case res of
                    Left ex  -> throwIO ex
                    Right () -> pure ()

                handleInterrupt UserInterrupt = do
                  mClose <- tryTakeMVar closeActionVar
                  case mClose of
                    Just closeSocket -> do
                      closeSocket
                      _ <- takeMVar serverDoneVar
                      pure ()
                    Nothing -> pure ()
                handleInterrupt other = throwIO other

            catch waitForShutdown handleInterrupt
