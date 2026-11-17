{-# LANGUAGE OverloadedStrings #-}

-- | Server configuration: all flags described in §9.1 of the HTTP spec.
--
-- Three-tier resolution order (D9): CLI flag > @^%CONFIG("server", name)@
-- in the database > built-in default.  The database tier is consulted
-- per-use in 'Server.State.configValue' rather than at startup, so a
-- live @SET@ takes effect without a restart, unless a flag pins it.
module Server.Config
  ( ServerConfig (..)
  , parseArgs
  , defaultConfig
  , validateTlsConfig
  , resolveMaxBodyBytes
  , resolveExecTimeoutMs
  , resolveExecTimeoutMaxMs
  , resolveMaxOutputBytes
  , resolveSessionIdleSecs
  , resolveSessionMaxSecs
  , resolveMaxSessions
  , resolveQueueTimeoutMs
  , resolveMaxBlobBytes
    -- * Built-in defaults (the last tier of the D9 resolution order)
  , defaultMaxBodyBytes
  , defaultExecTimeoutMs
  , defaultExecTimeoutMaxMs
  , defaultMaxOutputBytes
  , defaultSessionIdleSecs
  , defaultSessionMaxSecs
  , defaultMaxSessions
  , defaultQueueTimeoutMs
  , defaultMaxBlobBytes
  ) where

import Options.Applicative
import System.Directory (doesFileExist)

-- | All configuration knobs for the server.
--
-- 'Nothing' means "not pinned by a flag; use the database or default".
-- The database + default fallback is resolved at use time via
-- 'Server.State.configValue'.
data ServerConfig = ServerConfig
  { cfgDbPath            :: !FilePath    -- ^ --db
  , cfgListenAddr        :: !String      -- ^ --listen
  , cfgPort              :: !Int         -- ^ --port
  , cfgTlsCert           :: !(Maybe FilePath) -- ^ --tls-cert
  , cfgTlsKey            :: !(Maybe FilePath) -- ^ --tls-key
  , cfgMaxBodyBytes      :: !(Maybe Int) -- ^ --max-body-bytes (pins the database value)
  , cfgExecTimeoutMs     :: !(Maybe Int) -- ^ --exec-timeout-ms default
  , cfgExecTimeoutMaxMs  :: !(Maybe Int) -- ^ --exec-timeout-ms max
  , cfgMaxOutputBytes    :: !(Maybe Int) -- ^ --max-output-bytes
  , cfgSessionIdleSecs   :: !(Maybe Int) -- ^ --session-idle-seconds
  , cfgSessionMaxSecs    :: !(Maybe Int) -- ^ --session-max-seconds
  , cfgMaxSessions       :: !(Maybe Int) -- ^ --max-sessions
  , cfgQueueTimeoutMs    :: !(Maybe Int) -- ^ --queue-timeout-ms
  , cfgCorsOrigin        :: !(Maybe String) -- ^ --cors-origin
  , cfgAllowInsecureBasic :: !Bool       -- ^ --allow-insecure-basic
  , cfgMaxBlobBytes      :: !(Maybe Int) -- ^ --max-blob-bytes (max single-blob upload size)
  } deriving (Show)

-- | Hard-coded defaults used when neither a flag nor the database
-- supplies a value.  Matches the table in §9.1.
defaultConfig :: ServerConfig
defaultConfig = ServerConfig
  { cfgDbPath            = "phase.db"
  , cfgListenAddr        = "127.0.0.1"
  , cfgPort              = 8484
  , cfgTlsCert           = Nothing
  , cfgTlsKey            = Nothing
  , cfgMaxBodyBytes      = Nothing
  , cfgExecTimeoutMs     = Nothing
  , cfgExecTimeoutMaxMs  = Nothing
  , cfgMaxOutputBytes    = Nothing
  , cfgSessionIdleSecs   = Nothing
  , cfgSessionMaxSecs    = Nothing
  , cfgMaxSessions       = Nothing
  , cfgQueueTimeoutMs    = Nothing
  , cfgCorsOrigin        = Nothing
  , cfgAllowInsecureBasic = False
  , cfgMaxBlobBytes      = Nothing
  }

-- | Built-in defaults used when a 'Nothing' flag value needs a concrete
-- number.  Callers that want the three-tier resolution must use
-- 'Server.State.configValue' instead.
defaultMaxBodyBytes :: Int
defaultMaxBodyBytes = 1024 * 1024   -- 1 MiB

defaultExecTimeoutMs :: Int
defaultExecTimeoutMs = 10000

defaultExecTimeoutMaxMs :: Int
defaultExecTimeoutMaxMs = 60000

defaultMaxOutputBytes :: Int
defaultMaxOutputBytes = 1024 * 1024 -- 1 MiB

defaultSessionIdleSecs :: Int
defaultSessionIdleSecs = 1800

defaultSessionMaxSecs :: Int
defaultSessionMaxSecs = 43200

defaultMaxSessions :: Int
defaultMaxSessions = 256

defaultQueueTimeoutMs :: Int
defaultQueueTimeoutMs = 5000

-- | Default max blob upload size (10 MiB). Blobs larger than this are
-- rejected with HTTP 413 unless @--max-blob-bytes@ overrides it.
defaultMaxBlobBytes :: Int
defaultMaxBlobBytes = 10 * 1024 * 1024  -- 10 MiB

-- Re-export so Routes/Execute can use them without importing this module twice.
resolveMaxBodyBytes :: ServerConfig -> Int
resolveMaxBodyBytes cfg = maybe defaultMaxBodyBytes id (cfgMaxBodyBytes cfg)

resolveExecTimeoutMs :: ServerConfig -> Int
resolveExecTimeoutMs cfg = maybe defaultExecTimeoutMs id (cfgExecTimeoutMs cfg)

resolveExecTimeoutMaxMs :: ServerConfig -> Int
resolveExecTimeoutMaxMs cfg = maybe defaultExecTimeoutMaxMs id (cfgExecTimeoutMaxMs cfg)

resolveMaxOutputBytes :: ServerConfig -> Int
resolveMaxOutputBytes cfg = maybe defaultMaxOutputBytes id (cfgMaxOutputBytes cfg)

resolveSessionIdleSecs :: ServerConfig -> Int
resolveSessionIdleSecs cfg = maybe defaultSessionIdleSecs id (cfgSessionIdleSecs cfg)

resolveSessionMaxSecs :: ServerConfig -> Int
resolveSessionMaxSecs cfg = maybe defaultSessionMaxSecs id (cfgSessionMaxSecs cfg)

resolveMaxSessions :: ServerConfig -> Int
resolveMaxSessions cfg = maybe defaultMaxSessions id (cfgMaxSessions cfg)

resolveQueueTimeoutMs :: ServerConfig -> Int
resolveQueueTimeoutMs cfg = maybe defaultQueueTimeoutMs id (cfgQueueTimeoutMs cfg)

resolveMaxBlobBytes :: ServerConfig -> Int
resolveMaxBlobBytes cfg = maybe defaultMaxBlobBytes id (cfgMaxBlobBytes cfg)

-- | Parse command-line arguments into 'ServerConfig'.
parseArgs :: IO ServerConfig
parseArgs = execParser opts
  where
    opts = info (configParser <**> helper)
      ( fullDesc
     <> progDesc "PhaseDB HTTP API server"
     <> header   "phasedb-server -- serve a PhaseDB database over HTTP" )

configParser :: Parser ServerConfig
configParser = ServerConfig
  <$> strOption
        ( long "db" <> metavar "PATH" <> value "phase.db"
       <> showDefault <> help "Database file path" )
  <*> strOption
        ( long "listen" <> metavar "ADDR" <> value "127.0.0.1"
       <> showDefault <> help "Bind address" )
  <*> option auto
        ( long "port" <> metavar "N" <> value 8484
       <> showDefault <> help "TCP port" )
  <*> optional (strOption ( long "tls-cert" <> metavar "FILE" <> help "TLS certificate PEM (enables TLS)" ))
  <*> optional (strOption ( long "tls-key"  <> metavar "FILE" <> help "TLS private key PEM" ))
  <*> optional (option auto ( long "max-body-bytes"   <> metavar "N" <> help "Request body size cap (default 1 MiB)" ))
  <*> optional (option auto ( long "exec-timeout-ms"  <> metavar "N" <> help "Default execution timeout in ms (default 10000)" ))
  <*> optional (option auto ( long "exec-timeout-max-ms" <> metavar "N" <> help "Maximum execution timeout in ms (default 60000)" ))
  <*> optional (option auto ( long "max-output-bytes" <> metavar "N" <> help "Output cap per request in bytes (default 1 MiB)" ))
  <*> optional (option auto ( long "session-idle-seconds" <> metavar "N" <> help "Session idle timeout (default 1800)" ))
  <*> optional (option auto ( long "session-max-seconds"  <> metavar "N" <> help "Session absolute lifetime (default 43200)" ))
  <*> optional (option auto ( long "max-sessions"     <> metavar "N" <> help "Max concurrent sessions (default 256)" ))
  <*> optional (option auto ( long "queue-timeout-ms" <> metavar "N" <> help "Engine-lock wait timeout (default 5000)" ))
  <*> optional (strOption ( long "cors-origin" <> metavar "ORIGIN" <> help "Allowed CORS origin (off by default)" ))
  <*> switch ( long "allow-insecure-basic" <> help "Allow Basic auth on non-TLS, non-loopback connections" )
  <*> optional (option auto ( long "max-blob-bytes" <> metavar "N" <> help "Max blob upload size in bytes (default 10 MiB)" ))

-- | Validate TLS certificate and private key configuration (§9.1, §10).
-- Returns 'Right Nothing' if TLS is disabled (neither cert nor key given),
-- 'Right (Just (cert, key))' if both files exist, or 'Left errorMessage'
-- if the configuration is invalid.
validateTlsConfig :: ServerConfig -> IO (Either String (Maybe (FilePath, FilePath)))
validateTlsConfig cfg = case (cfgTlsCert cfg, cfgTlsKey cfg) of
  (Just _, Nothing) ->
    pure (Left "phasedb-server: both --tls-cert and --tls-key must be provided to enable TLS")
  (Nothing, Just _) ->
    pure (Left "phasedb-server: both --tls-cert and --tls-key must be provided to enable TLS")
  (Just cert, Just key) -> do
    certExists <- doesFileExist cert
    if not certExists
      then pure (Left ("phasedb-server: TLS certificate file does not exist: " ++ cert))
      else do
        keyExists <- doesFileExist key
        if not keyExists
          then pure (Left ("phasedb-server: TLS key file does not exist: " ++ key))
          else pure (Right (Just (cert, key)))
  (Nothing, Nothing) ->
    pure (Right Nothing)
