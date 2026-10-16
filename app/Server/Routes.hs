{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | WAI router and request handlers for all v1 endpoints (§6).
--
-- Routing is hand-written on WAI per §10 ("a dozen routes do not repay
-- the type-level machinery of servant").  Each handler:
--   1. Generates a request-id.
--   2. Enforces the body size cap.
--   3. Authenticates (Basic or Bearer).
--   4. Runs the Phase command via 'Server.Execute'.
--   5. Returns the JSON envelope.
module Server.Routes
  ( mkApp
  ) where

import Control.Concurrent.STM (atomically, readTVar)
import Control.Exception (SomeException, mask_, try)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Control.Monad (unless)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (addUTCTime, getCurrentTime)
import Data.Version (showVersion)
import System.Directory (doesFileExist, getFileSize)
import System.IO (hPutStrLn, stderr)
import qualified Paths_phasedb_hs
import Network.HTTP.Types
  ( Status, HeaderName
  , hAuthorization, hContentType
  , status200, status201, status400, status401, status403, status404
  , status413, status429, status503, statusCode
  )
import qualified Data.CaseInsensitive as CI
import qualified Crypto.Hash as Hash
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.Set as Set
import Text.Parsec (parse, eof)

import Network.Wai
  ( Application, Request, Response
  , pathInfo, requestHeaders, requestMethod, responseLBS, getRequestBodyChunk
  , isSecure, mapResponseHeaders, remoteHost, rawPathInfo
  , responseHeaders, responseStatus, queryString
  )
import Data.Aeson (object, encode, (.=))
import System.Clock (Clock (Monotonic), TimeSpec (..), getTime)

import Phase.AST (Expr (..), Ref (..), VarKind (..), PValue (..), mkValue, unPValue, UnOp (..), BinOp (..))
import Phase.Auth (Role (..), lookupUserRole, roleToText, checkReadAccess, checkWriteAccess)
import Phase.Exec (AdminError (..), applyAddUser, prepareNewUser)
import Phase.Parser (pRef)
import Phase.State (Env (..), EvalError (..), initEnvWithStorage)
import Phase.Storage.Engine
  ( StorageEngine (..), compactTo, flushPendingWal, openStorageEngine
  , seLastTx, takeCheckpoint, readBlob, writeBlob, commitTransaction, readLive
  )
import Phase.Storage.Transaction (TxHandle (..))
import Phase.Storage.Types (StorageError (..), TxId, formatStorageError)
import Phase.Tree (toSubKey)

import Server.Auth
  ( Credential (..), CredError (..)
  , LoginGuard, LoginResult (..)
  , extractCredential, isLoopback, tryLogin
  )
import Server.Config (ServerConfig (..))
import Server.Execute
  ( ExecOutcome (..), closeSession, executeOneShot, executeSession
  , sweepExpiredSessions, withSessionLock
  )
import Server.Json
import Server.Session
  ( Session (..), lookupSession, newSession, touchSession, tokenDigest )
import Server.State
  ( ServerState (..), Snapshot (..), configSourceInt, withBoundedEngineLock
  , effExecTimeoutMaxMs, effExecTimeoutMs, effMaxBodyBytes, effMaxOutputBytes
  , effMaxSessions, effQueueTimeoutMs, effSessionIdleSecs, effSessionMaxSecs
  , effMaxBlobBytes, ssLogger
  )

-- --------------------------------------------------------------------
-- CORS helpers (§9.4)
-- --------------------------------------------------------------------

-- | Headers added to all responses when CORS is enabled (§9.4).
corsHeadersFor :: ServerConfig -> [(HeaderName, ByteString)]
corsHeadersFor cfg =
  case cfgCorsOrigin cfg of
    Nothing -> []
    Just orig ->
      let origBs = BSC.pack orig
          base = [ (CI.mk "Access-Control-Allow-Origin", origBs)
                 , (CI.mk "Access-Control-Expose-Headers", "X-Request-Id")
                 ]
      in if orig == "*"
           then base
           else (CI.mk "Access-Control-Allow-Credentials", "true") : base

-- | Headers specific to preflight OPTIONS response (§9.4).
corsPreflightHeaders :: [(HeaderName, ByteString)]
corsPreflightHeaders =
  [ (CI.mk "Access-Control-Allow-Methods", "GET, POST, DELETE, PUT, HEAD, OPTIONS")
  , (CI.mk "Access-Control-Allow-Headers", "Authorization, Content-Type, X-Request-Id")
  , (CI.mk "Access-Control-Max-Age", "86400")
  , (CI.mk "Content-Length", "0")
  ]

-- --------------------------------------------------------------------
-- Application entry point
-- --------------------------------------------------------------------

-- | Build the WAI 'Application'.
mkApp :: ServerState -> LoginGuard -> Application
mkApp ss guard req respond = do
  t0 <- getTime Monotonic
  userRef <- newIORef Nothing
  generated <- generateRequestId
  let reqId     = fromMaybe generated (clientRequestId req)
      method    = requestMethod req
      path      = pathInfo req
      cfg       = ssConfig ss
      corsHdrs  = corsHeadersFor cfg

      respond' resp = do
        let st = statusCode (responseStatus resp)
        t1 <- getTime Monotonic
        let elapsed = elapsedMs t0 t1
            respH = responseHeaders resp
            respBytes = fromMaybe 0 $ do
              bs <- lookup (CI.mk "Content-Length") respH
              case BSC.readInt bs of
                Just (n, _) -> Just n
                Nothing     -> Nothing
        mUser <- readIORef userRef
        let logVal = buildAccessLogEntry
                       reqId
                       mUser
                       (TE.decodeUtf8With lenientDecode method)
                       (TE.decodeUtf8With lenientDecode (rawPathInfo req))
                       st
                       elapsed
                       respBytes
        ssLogger ss (LBS.toStrict (encode logVal))
        respond (mapResponseHeaders (\hdrs -> (CI.mk "X-Request-Id", TE.encodeUtf8 reqId) : (hdrs ++ corsHdrs)) resp)

  if method == "OPTIONS" && isJust (cfgCorsOrigin cfg)
    then respond' (responseLBS status200 corsPreflightHeaders "")
    else do
      -- Expired sessions are removed, and their locks released, before this
      -- request takes its snapshot.  Health stays lock-free.
      unless (method == "GET" && path == ["v1", "health"]) (sweepExpiredSessions ss)
      dispatch ss guard userRef reqId method path req respond'

-- | A client-supplied @X-Request-Id@, if it is short and printable (1 to 64
-- visible ASCII characters, no spaces); anything else is ignored and the
-- server generates its own.
clientRequestId :: Request -> Maybe Text
clientRequestId req = do
  v <- lookupHeader "X-Request-Id" req
  if BS.length v >= 1 && BS.length v <= 64 && BS.all (\c -> c >= 0x21 && c <= 0x7e) v
    then Just (TE.decodeLatin1 v)
    else Nothing

-- --------------------------------------------------------------------
-- Router
-- --------------------------------------------------------------------

dispatch :: ServerState
         -> LoginGuard
         -> IORef (Maybe Text)
         -> Text             -- ^ request-id
         -> ByteString       -- ^ method
         -> [Text]           -- ^ path segments
         -> Request
         -> (Response -> IO a)
         -> IO a
dispatch _ss _guard _uRef reqId "GET"    ["v1","health"]                  _req respond =
  respond (jsonOk (healthBody reqId))

dispatch ss guard  uRef reqId "POST"   ["v1","execute"]                 req respond =
  handleExecuteOneShot ss guard uRef reqId req respond

dispatch ss guard  uRef reqId "POST"   ["v1","sessions"]                req respond =
  handleCreateSession ss guard uRef reqId req respond

dispatch ss _guard uRef reqId "GET"    ["v1","sessions","current"]      req respond =
  handleGetSession ss uRef reqId req respond

dispatch ss _guard uRef reqId "DELETE" ["v1","sessions","current"]      req respond =
  handleDeleteSession ss uRef reqId req respond

dispatch ss _guard uRef reqId "POST"   ["v1","sessions","current","execute"] req respond =
  handleSessionExecute ss uRef reqId req respond

dispatch ss guard  uRef reqId "GET"    ["v1","admin","status"]          req respond =
  handleAdminStatus ss guard uRef reqId req respond

dispatch ss guard  uRef reqId "POST"   ["v1","admin","checkpoint"]      req respond =
  handleAdminCheckpoint ss guard uRef reqId req respond

dispatch ss guard  uRef reqId "POST"   ["v1","admin","compact"]         req respond =
  handleAdminCompact ss guard uRef reqId req respond

dispatch ss guard  uRef reqId "POST"   ["v1","admin","users"]           req respond =
  handleAdminAddUser ss guard uRef reqId req respond

dispatch ss guard  uRef reqId "PUT"    ["v1","blobs"]                   req respond =
  handlePutBlob ss guard uRef reqId req respond

dispatch ss guard  uRef reqId "GET"    ["v1","blobs"]                   req respond =
  handleGetBlobRef False ss guard uRef reqId req respond

dispatch ss guard  uRef reqId "HEAD"   ["v1","blobs"]                   req respond =
  handleGetBlobRef True ss guard uRef reqId req respond

dispatch ss guard  uRef reqId "GET"    ["v1","blobs",hash]              req respond =
  handleGetBlobHash False ss guard uRef reqId hash req respond

dispatch ss guard  uRef reqId "HEAD"   ["v1","blobs",hash]              req respond =
  handleGetBlobHash True ss guard uRef reqId hash req respond

dispatch _ss _guard _uRef reqId _method _path _req respond =
  respond (jsonErr status404 (transportError "NOT_FOUND" "unknown path" reqId))

-- --------------------------------------------------------------------
-- POST /v1/execute  (M1 one-shot)
-- --------------------------------------------------------------------

handleExecuteOneShot :: ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleExecuteOneShot ss guard uRef reqId req respond = do
  snap <- readIORef (ssSnapshot ss)
  let cfg     = ssConfig ss
  snapB <- readIORef (ssSnapshot ss)
  let maxBody = effMaxBodyBytes cfg snapB
  body <- readBodyCapped maxBody req
  case body of
    Nothing -> respond (jsonErr status413 (transportError "TOO_LARGE" "request body too large" reqId))
    Just bs -> do
      let authHdrVal = lookupHeader hAuthorization req
      case extractCredential (maybe "" id authHdrVal) of
        Left NoCredential ->
          respond (jsonErr status401 (transportError "UNAUTHENTICATED" "credentials required" reqId))
        Left (MalformedCredential msg) ->
          respond (jsonErr status401 (transportError "UNAUTHENTICATED" msg reqId))
        Right (BearerCred _) ->
          respond (jsonErr status401 (transportError "UNAUTHENTICATED" "one-shot requires Basic auth" reqId))
        Right (BasicCred user password) -> do
          writeIORef uRef (Just user)
          -- Enforce Basic-over-loopback rule (§5.1).
          let hasTls     = isSecure req
              loopback   = isLoopback (remoteHost req)
              allowBasic = hasTls || loopback || cfgAllowInsecureBasic cfg
          if not allowBasic
            then respond (jsonErr status401 (transportError "UNAUTHENTICATED" "Basic auth requires TLS or a loopback connection" reqId))
            else do
              let eng = snapEngine snap
              loginResult <- tryLogin guard user password (addrKey req) eng
              case loginResult of
                LoginRateLimited n ->
                  respond (jsonErrHdr status429
                    [(CI.mk "Retry-After", BSC.pack (show n))]
                    (transportError "RATE_LIMITED" "too many login attempts" reqId))
                LoginFailed ->
                  respond (jsonErr status401 (transportError "UNAUTHENTICATED" "invalid credentials" reqId))
                LoginOk -> do
                  -- Re-check role: must not be RNone (§6).
                  let role = lookupUserRole user eng
                  if role == RNone
                    then respond (jsonErr status403 (transportError "FORBIDDEN" "account has no access" reqId))
                    else do
                      case parseExecuteBody (LBS.fromStrict bs) of
                        Left msg ->
                          respond (jsonErr status400 (transportError "BAD_REQUEST" msg reqId))
                        Right er -> do
                          let timeoutMs  = clampTimeout cfg snap (erTimeoutMs er)
                              maxOut     = effMaxOutputBytes cfg snap
                              ns         = erNamespace er
                          t0 <- getTime Monotonic
                          outcome <- executeOneShot ss user (erCommand er) ns timeoutMs maxOut
                          t1 <- getTime Monotonic
                          let elapsed = elapsedMs t0 t1
                              lastTx  = seLastTx (snapEngine snap)
                          respond (outcomeResponse reqId elapsed lastTx outcome)

-- --------------------------------------------------------------------
-- POST /v1/sessions  (M2 login)
-- --------------------------------------------------------------------

handleCreateSession :: ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleCreateSession ss guard uRef reqId req respond = do
  snap <- readIORef (ssSnapshot ss)
  let cfg     = ssConfig ss
  snapB <- readIORef (ssSnapshot ss)
  let maxBody = effMaxBodyBytes cfg snapB
  body <- readBodyCapped maxBody req
  case body of
    Nothing -> respond (jsonErr status413 (transportError "TOO_LARGE" "request body too large" reqId))
    Just bs ->
      case parseSessionBody (LBS.fromStrict bs) of
        Left msg -> respond (jsonErr status400 (transportError "BAD_REQUEST" msg reqId))
        Right sr -> do
          let eng  = snapEngine snap
              user = srUsername sr
              pass = srPassword sr
          writeIORef uRef (Just user)
          -- Check max-sessions cap.
          smap <- atomically (readTVar (ssSessions ss))
          let maxSess = effMaxSessions cfg snap
          if Map.size smap >= maxSess
            then respond (jsonErr status503 (transportError "BUSY" "too many sessions" reqId))
            else do
              loginResult <- tryLogin guard user pass (addrKey req) eng
              case loginResult of
                LoginRateLimited n ->
                  respond (jsonErrHdr status429
                    [(CI.mk "Retry-After", BSC.pack (show n))]
                    (transportError "RATE_LIMITED" "too many login attempts" reqId))
                LoginFailed ->
                  respond (jsonErr status401 (transportError "UNAUTHENTICATED" "invalid credentials" reqId))
                LoginOk -> do
                  let role    = lookupUserRole user eng
                      idleS   = effSessionIdleSecs cfg snap
                      maxS    = effSessionMaxSecs cfg snap
                      initEnv = (initEnvWithStorage eng)
                                  { envCurrentUser = user
                                  , envFileIOEnabled = False
                                  }
                  now <- getCurrentTime
                  (token, _digest) <- newSession user "_GLOBAL" initEnv (ssSessions ss)
                  let expiresAt = addUTCTime (fromIntegral maxS) now
                  respond (jsonCreated (sessionBody token expiresAt idleS user (roleToText role) "_GLOBAL" reqId))

-- --------------------------------------------------------------------
-- GET /v1/sessions/current  (M2)
-- --------------------------------------------------------------------

handleGetSession :: ServerState -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleGetSession ss uRef reqId req respond = do
  snap <- readIORef (ssSnapshot ss)
  let cfg = ssConfig ss
  withBearerSession ss uRef cfg reqId req respond $ \sess _digest -> do
    let eng       = snapEngine snap
        user      = sessUser sess
        role      = lookupUserRole user eng
        idleS     = effSessionIdleSecs cfg snap
        maxS      = effSessionMaxSecs cfg snap
        expiresAt = addUTCTime (fromIntegral maxS) (sessCreatedAt sess)
    respond (jsonOk (sessionInfoBody user (roleToText role) (sessNamespace sess) expiresAt idleS reqId))

-- --------------------------------------------------------------------
-- DELETE /v1/sessions/current  (M2 logout)
-- --------------------------------------------------------------------

handleDeleteSession :: ServerState -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleDeleteSession ss uRef reqId req respond = do
  let cfg = ssConfig ss
  snapQ <- readIORef (ssSnapshot ss)
  withBearerSession ss uRef cfg reqId req respond $ \sess digest -> do
    -- Wait for any request still running in this session, then remove it and
    -- release its LOCKs.  Same bounded wait as a second request on the session.
    r <- withSessionLock (effQueueTimeoutMs cfg snapQ) sess (closeSession ss digest sess)
    respond $ case r of
      Nothing    -> jsonErr status503 (transportError "BUSY" "session is busy with another request" reqId)
      Just False -> jsonErr status503 (transportError "BUSY" "server is busy, try again" reqId)
      Just True  -> jsonOk (handleDeleteSessionOk reqId)

-- | Body of a successful @DELETE /v1/sessions/current@.
handleDeleteSessionOk :: Text -> LBS.ByteString
handleDeleteSessionOk reqId = LBS.fromStrict $ TE.encodeUtf8 $
  "{\"ok\":true,\"requestId\":\"" <> reqId <> "\"}"

-- --------------------------------------------------------------------
-- POST /v1/sessions/current/execute  (M2 session execute)
-- --------------------------------------------------------------------

handleSessionExecute :: ServerState -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleSessionExecute ss uRef reqId req respond = do
  let cfg = ssConfig ss
  snapB <- readIORef (ssSnapshot ss)
  let maxBody = effMaxBodyBytes cfg snapB
  body <- readBodyCapped maxBody req
  case body of
    Nothing -> respond (jsonErr status413 (transportError "TOO_LARGE" "request body too large" reqId))
    Just bs ->
      case parseExecuteBody (LBS.fromStrict bs) of
        Left msg -> respond (jsonErr status400 (transportError "BAD_REQUEST" msg reqId))
        Right er ->
          withBearerSession ss uRef cfg reqId req respond $ \sess0 digest -> do
            -- Two requests for one session never run at the same time (D6):
            -- wait for the session lock, but no longer than the queue timeout.
            snapQ <- readIORef (ssSnapshot ss)
            result <- withSessionLock (effQueueTimeoutMs cfg snapQ) sess0 (runInSession ss cfg reqId digest er)
            -- the response is written after the lock is released
            respond $ case result of
              Nothing   -> jsonErr status503 (transportError "BUSY" "session is busy with another request" reqId)
              Just resp -> resp

-- | The body of a session request, run while holding the session's lock.
-- The session is looked up again here: while this request waited, the
-- previous one may have changed the session's locals, or the session may
-- have been closed or expired.
runInSession :: ServerState -> ServerConfig -> Text -> Text -> ExecuteReq -> IO Response
runInSession ss cfg reqId digest er = do
  snap <- readIORef (ssSnapshot ss)
  let idleS = effSessionIdleSecs cfg snap
      maxS  = effSessionMaxSecs  cfg snap
  mSess <- lookupSession digest idleS maxS (ssSessions ss)
  case mSess of
    Nothing -> pure (jsonErr status401 (transportError "UNAUTHENTICATED" "invalid or expired token" reqId))
    Just sess -> do
      let eng  = snapEngine snap
          user = sessUser sess
          role = lookupUserRole user eng
      if role == RNone
        then pure (jsonErr status403 (transportError "FORBIDDEN" "account has no access" reqId))
        else do
          let timeoutMs = clampTimeout cfg snap (erTimeoutMs er)
              maxOut    = effMaxOutputBytes cfg snap
              ns        = erNamespace er
          t0 <- getTime Monotonic
          (outcome, env') <- executeSession ss user (sessEnv sess) (erCommand er) ns timeoutMs maxOut
          t1 <- getTime Monotonic
          let elapsed = elapsedMs t0 t1
              lastTx  = seLastTx eng
          case outcome of
            ExecOk {} ->
              -- Update the session's env and last-used time.
              touchSession digest env' (if T.null ns then sessNamespace sess else ns) (ssSessions ss)
            _ -> pure ()
          pure (outcomeResponse reqId elapsed lastTx outcome)

-- --------------------------------------------------------------------
-- GET /v1/admin/status  (M3)
-- --------------------------------------------------------------------

handleAdminStatus :: ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleAdminStatus ss guard uRef reqId req respond = do
  snap <- readIORef (ssSnapshot ss)
  let cfg = ssConfig ss
  withAdminAuth ss guard uRef cfg reqId req respond $ \_user -> do
    smap <- atomically (readTVar (ssSessions ss))
    let lastTx    = seLastTx (snapEngine snap)
        sessCount = Map.size smap
        ver       = T.pack (showVersion Paths_phasedb_hs.version)
        configObj = object
          [ "maxBodyBytes"       .= configEntry (effMaxBodyBytes cfg snap)     (configSourceInt "maxBodyBytes" (cfgMaxBodyBytes cfg) snap)
          , "execTimeoutMs"      .= configEntry (effExecTimeoutMs cfg snap)    (configSourceInt "execTimeoutMs" (cfgExecTimeoutMs cfg) snap)
          , "execTimeoutMaxMs"   .= configEntry (effExecTimeoutMaxMs cfg snap) (configSourceInt "execTimeoutMaxMs" (cfgExecTimeoutMaxMs cfg) snap)
          , "maxOutputBytes"     .= configEntry (effMaxOutputBytes cfg snap)   (configSourceInt "maxOutputBytes" (cfgMaxOutputBytes cfg) snap)
          , "sessionIdleSeconds" .= configEntry (effSessionIdleSecs cfg snap)  (configSourceInt "sessionIdleSeconds" (cfgSessionIdleSecs cfg) snap)
          , "sessionMaxSeconds"  .= configEntry (effSessionMaxSecs cfg snap)   (configSourceInt "sessionMaxSeconds" (cfgSessionMaxSecs cfg) snap)
          , "maxSessions"        .= configEntry (effMaxSessions cfg snap)      (configSourceInt "maxSessions" (cfgMaxSessions cfg) snap)
          , "queueTimeoutMs"     .= configEntry (effQueueTimeoutMs cfg snap)   (configSourceInt "queueTimeoutMs" (cfgQueueTimeoutMs cfg) snap)
          ]
    exists <- doesFileExist (ssDbPath ss)
    fileSize <- if exists then fromIntegral <$> getFileSize (ssDbPath ss) else pure 0
    respond (jsonOk (statusBody lastTx sessCount fileSize ver configObj reqId))
  where
    configEntry val src = object [ "value" .= val, "source" .= src ]

-- --------------------------------------------------------------------
-- POST /v1/admin/checkpoint  (M3)
-- --------------------------------------------------------------------

handleAdminCheckpoint :: ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleAdminCheckpoint ss guard uRef reqId req respond = do
  let cfg = ssConfig ss
  snapB <- readIORef (ssSnapshot ss)
  let maxBody = effMaxBodyBytes cfg snapB
  body <- readBodyCapped maxBody req
  case body of
    Nothing -> respond (jsonErr status413 (transportError "TOO_LARGE" "request body too large" reqId))
    Just bs ->
      case parseCheckpointBody (LBS.fromStrict bs) of
        Left msg -> respond (jsonErr status400 (transportError "BAD_REQUEST" msg reqId))
        Right () ->
          withAdminAuth ss guard uRef cfg reqId req respond $ \user -> do
            snapQ <- readIORef (ssSnapshot ss)
            let waitMs = effQueueTimeoutMs cfg snapQ
            mResult <- withBoundedEngineLock waitMs (ssEngineLock ss) $ \locked ->
              if lookupUserRole user (snapEngine locked) < RAdmin
                then pure (locked, Left (status403, transportError "FORBIDDEN" "admin role required" reqId))
                else do
                  mStorage <- try (takeCheckpoint (snapEngine locked)) :: IO (Either SomeException StorageEngine)
                  case mStorage of
                    Left e ->
                      pure (locked, Left (status200, adminErrorBody "CHECKPOINT FAILED" (T.pack (show e)) (seLastTx (snapEngine locked)) reqId))
                    Right storage' -> do
                      let snap' = Snapshot
                            { snapEngine  = storage'
                            , snapLocks   = snapLocks locked
                            , snapVersion = snapVersion locked + 1
                            }
                      writeIORef (ssSnapshot ss) snap'
                      pure (snap', Right (seLastTx storage'))
            case mResult of
              Nothing -> respond (jsonErr status503 (transportError "BUSY" "server is busy, try again" reqId))
              Just (Left (st, b)) -> respond (jsonErr st b)
              Just (Right tx)     -> respond (jsonOk (adminOkBody tx reqId))

-- --------------------------------------------------------------------
-- POST /v1/admin/compact  (M3)
-- --------------------------------------------------------------------

handleAdminCompact :: ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleAdminCompact ss guard uRef reqId req respond = do
  let cfg = ssConfig ss
  snapB <- readIORef (ssSnapshot ss)
  let maxBody = effMaxBodyBytes cfg snapB
  body <- readBodyCapped maxBody req
  case body of
    Nothing -> respond (jsonErr status413 (transportError "TOO_LARGE" "request body too large" reqId))
    Just bs ->
      case parseCompactBody (LBS.fromStrict bs) of
        Left msg -> respond (jsonErr status400 (transportError "BAD_REQUEST" msg reqId))
        Right beforeTx ->
          withAdminAuth ss guard uRef cfg reqId req respond $ \user -> do
            snapQ <- readIORef (ssSnapshot ss)
            let waitMs = effQueueTimeoutMs cfg snapQ
            mResult <- withBoundedEngineLock waitMs (ssEngineLock ss) $ \locked ->
              if lookupUserRole user (snapEngine locked) < RAdmin
                then pure (locked, Left (status403, transportError "FORBIDDEN" "admin role required" reqId))
                else if fromIntegral beforeTx > seLastTx (snapEngine locked)
                  then pure (locked, Left (status200, adminErrorBody "COMPACT FAILED" "beforeTx is greater than lastTx" (seLastTx (snapEngine locked)) reqId))
                  else do
                    engFlushed <- flushPendingWal (snapEngine locked)
                    cRes <- try (compactTo (ssDbPath ss) (fromIntegral beforeTx)) :: IO (Either SomeException (Either StorageError FilePath))
                    case cRes of
                      Left ex ->
                        pure (locked, Left (status200, adminErrorBody "COMPACT FAILED" (T.pack (show ex)) (seLastTx engFlushed) reqId))
                      Right (Left sErr) ->
                        pure (locked, Left (status200, adminErrorBody "COMPACT FAILED" (T.pack (show sErr)) (seLastTx engFlushed) reqId))
                      Right (Right backupPath) -> do
                        hPutStrLn stderr ("compact: backup created at " ++ backupPath)
                        reopened <- openStorageEngine (ssDbPath ss)
                        case reopened of
                          Left oErr ->
                            pure (locked, Left (status200, adminErrorBody "COMPACT FAILED" (T.pack (show oErr)) (seLastTx engFlushed) reqId))
                          Right storage' -> do
                            let snap' = Snapshot
                                  { snapEngine  = storage'
                                  , snapLocks   = snapLocks locked
                                  , snapVersion = snapVersion locked + 1
                                  }
                            writeIORef (ssSnapshot ss) snap'
                            pure (snap', Right (seLastTx storage'))
            case mResult of
              Nothing -> respond (jsonErr status503 (transportError "BUSY" "server is busy, try again" reqId))
              Just (Left (st, b)) -> respond (jsonErr st b)
              Just (Right tx)     -> respond (jsonOk (adminOkBody tx reqId))

-- --------------------------------------------------------------------
-- POST /v1/admin/users  (M3)
-- --------------------------------------------------------------------

handleAdminAddUser :: ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleAdminAddUser ss guard uRef reqId req respond = do
  let cfg = ssConfig ss
  snapB <- readIORef (ssSnapshot ss)
  let maxBody = effMaxBodyBytes cfg snapB
  body <- readBodyCapped maxBody req
  case body of
    Nothing -> respond (jsonErr status413 (transportError "TOO_LARGE" "request body too large" reqId))
    Just bs ->
      case parseAddUserBody (LBS.fromStrict bs) of
        Left msg -> respond (jsonErr status400 (transportError "BAD_REQUEST" msg reqId))
        Right (uname, pass, role) ->
          withAdminAuth ss guard uRef cfg reqId req respond $ \user -> do
            -- D15: Hash password outside the engine lock
            (salt, hash) <- prepareNewUser pass
            snapQ <- readIORef (ssSnapshot ss)
            let waitMs = effQueueTimeoutMs cfg snapQ
            mResult <- withBoundedEngineLock waitMs (ssEngineLock ss) $ \locked ->
              if lookupUserRole user (snapEngine locked) < RAdmin
                then pure (locked, Left (status403, transportError "FORBIDDEN" "admin role required" reqId))
                else
                  let envL = (initEnvWithStorage (snapEngine locked)) { envCurrentUser = user }
                  in case applyAddUser uname salt hash role envL of
                        Left (AdminUserExists _) ->
                          pure (locked, Left (status200, adminErrorBody "USER EXISTS" ("<USER EXISTS> user already exists: " <> uname) (seLastTx (snapEngine locked)) reqId))
                        Left (AdminBadRole _) ->
                          pure (locked, Left (status400, transportError "BAD_REQUEST" "unknown role" reqId))
                        Left (AdminPermissionDenied r) ->
                          pure (locked, Left (status403, transportError "FORBIDDEN" r reqId))
                        Left (AdminStorageError err) ->
                          pure (locked, Left (status400, transportError "BAD_REQUEST" err reqId))
                        Left (AdminOther err) ->
                          pure (locked, Left (status400, transportError "BAD_REQUEST" err reqId))
                        Right env' -> do
                          storage' <- mask_ (flushPendingWal (envStorage env'))
                          let snap' = Snapshot
                                { snapEngine  = storage'
                                , snapLocks   = snapLocks locked
                                , snapVersion = snapVersion locked + 1
                                }
                          writeIORef (ssSnapshot ss) snap'
                          pure (snap', Right (seLastTx storage'))
            case mResult of
              Nothing -> respond (jsonErr status503 (transportError "BUSY" "server is busy, try again" reqId))
              Just (Left (st, b)) -> respond (jsonErr st b)
              Just (Right tx)     -> respond (jsonOk (adminOkBody tx reqId))

-- --------------------------------------------------------------------
-- Auth middleware helpers
-- --------------------------------------------------------------------

-- | Run a handler only if a valid Bearer session is found.
withBearerSession :: ServerState
                  -> IORef (Maybe Text)
                  -> ServerConfig
                  -> Text        -- ^ requestId
                  -> Request
                  -> (Response -> IO a)
                  -> (Session -> Text -> IO a)  -- ^ (session, digest)
                  -> IO a
withBearerSession ss uRef cfg reqId req respond action = do
  snap <- readIORef (ssSnapshot ss)
  let authHdrVal = lookupHeader hAuthorization req
  case extractCredential (maybe "" id authHdrVal) of
    Left _ ->
      respond (jsonErr status401 (transportError "UNAUTHENTICATED" "Bearer token required" reqId))
    Right (BasicCred user _) -> do
      writeIORef uRef (Just user)
      respond (jsonErr status401 (transportError "UNAUTHENTICATED" "Bearer token required" reqId))
    Right (BearerCred raw) -> do
      let digest  = tokenDigest raw
          idleS   = effSessionIdleSecs cfg snap
          maxS    = effSessionMaxSecs cfg snap
      mSess <- lookupSession digest idleS maxS (ssSessions ss)
      case mSess of
        Nothing -> respond (jsonErr status401 (transportError "UNAUTHENTICATED" "invalid or expired token" reqId))
        Just sess -> do
          writeIORef uRef (Just (sessUser sess))
          action sess digest

-- | Authenticate a user via Basic or Bearer and return (username, role).
withUserAuth :: ServerState
             -> LoginGuard
             -> IORef (Maybe Text)
             -> ServerConfig
             -> Text
             -> Request
             -> (Response -> IO a)
             -> (Text -> Role -> IO a)  -- ^ username and role
             -> IO a
withUserAuth ss guard uRef cfg reqId req respond action = do
  snap <- readIORef (ssSnapshot ss)
  let eng        = snapEngine snap
      authHdrVal = lookupHeader hAuthorization req
  case extractCredential (maybe "" id authHdrVal) of
    Left _ ->
      respond (jsonErr status401 (transportError "UNAUTHENTICATED" "credentials required" reqId))
    Right (BearerCred raw) -> do
      let digest = tokenDigest raw
          idleS  = effSessionIdleSecs cfg snap
          maxS   = effSessionMaxSecs cfg snap
      mSess <- lookupSession digest idleS maxS (ssSessions ss)
      case mSess of
        Nothing -> respond (jsonErr status401 (transportError "UNAUTHENTICATED" "invalid or expired token" reqId))
        Just sess -> do
          let user = sessUser sess
          writeIORef uRef (Just user)
          let role = lookupUserRole user eng
          action user role
    Right (BasicCred user password) -> do
      writeIORef uRef (Just user)
      let hasTls   = isSecure req
          loopback = isLoopback (remoteHost req)
          allow    = hasTls || loopback || cfgAllowInsecureBasic cfg
      if not allow
        then respond (jsonErr status401 (transportError "UNAUTHENTICATED" "Basic auth requires TLS or loopback" reqId))
        else do
          loginResult <- tryLogin guard user password (addrKey req) eng
          case loginResult of
            LoginRateLimited n ->
              respond (jsonErrHdr status429 [(CI.mk "Retry-After", BSC.pack (show n))]
                (transportError "RATE_LIMITED" "too many login attempts" reqId))
            LoginFailed ->
              respond (jsonErr status401 (transportError "UNAUTHENTICATED" "invalid credentials" reqId))
            LoginOk -> do
              let role = lookupUserRole user eng
              action user role

-- | Run a handler only if the request carries valid admin credentials
-- (either Basic or Bearer) and the user has the admin role.
withAdminAuth :: ServerState
              -> LoginGuard
              -> IORef (Maybe Text)
              -> ServerConfig
              -> Text
              -> Request
              -> (Response -> IO a)
              -> (Text -> IO a)  -- ^ username
              -> IO a
withAdminAuth ss guard uRef cfg reqId req respond action =
  withUserAuth ss guard uRef cfg reqId req respond $ \user role ->
    if role < RAdmin
      then respond (jsonErr status403 (transportError "FORBIDDEN" "admin role required" reqId))
      else action user

-- --------------------------------------------------------------------
-- Blob Handlers (§6.5)
-- --------------------------------------------------------------------

-- | Parse a client ref query parameter, e.g. @^GLOBAL(1)@ or @GLOBAL(1)@.
parseRefQueryParam :: Text -> Either Text (Text, [PValue])
parseRefQueryParam raw =
  let t = T.strip raw
      withCaret = if "^" `T.isPrefixOf` t then t else "^" <> t
  in case parse (pRef <* eof) "ref" withCaret of
       Left _ -> Left "malformed ref parameter"
       Right (Ref Global _ name subExprs _) ->
         case mapM exprToPValue subExprs of
           Just subs -> Right (name, subs)
           Nothing   -> Left "subscripts in ref parameter must be literals"
       Right _ -> Left "ref must target a global variable"
  where
    exprToPValue (Lit v) = Just v
    exprToPValue (UnExpr Neg (Lit v)) = Just (mkValue ("-" <> unPValue v))
    exprToPValue (UnExpr Pos (Lit v)) = Just v
    exprToPValue (BinExpr Concat l r) = do
      vl <- exprToPValue l
      vr <- exprToPValue r
      pure (mkValue (unPValue vl <> unPValue vr))
    exprToPValue _ = Nothing

-- | Check whether a value string is a blob pointer (@blob:<hash>@ or @BLOB:<hash>@).
isBlobPointer :: Text -> Maybe Text
isBlobPointer t
  | "blob:" `T.isPrefixOf` t = Just (T.toLower (T.drop 5 t))
  | "BLOB:" `T.isPrefixOf` t = Just (T.toLower (T.drop 5 t))
  | otherwise                = Nothing

-- | @PUT /v1/blobs@ handler (§6.5).
handlePutBlob :: ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handlePutBlob ss guard uRef reqId req respond = do
  snapB <- readIORef (ssSnapshot ss)
  let cfg = ssConfig ss
      maxBlob = effMaxBlobBytes cfg snapB
  mBody <- readBodyCapped maxBlob req
  case mBody of
    Nothing ->
      respond (jsonErr status413 (transportError "TOO_LARGE" "blob size exceeds maximum allowed" reqId))
    Just bs -> do
      let mRefParam = do
            pair <- lookup "ref" (queryString req)
            rawBytes <- pair
            pure (TE.decodeUtf8 rawBytes)
      case mapM parseRefQueryParam mRefParam of
        Left err ->
          respond (jsonErr status400 (transportError "BAD_REQUEST" err reqId))
        Right mRef ->
          withUserAuth ss guard uRef cfg reqId req respond $ \user role -> do
            if role < RWrite
              then respond (jsonErr status403 (transportError "FORBIDDEN" "write role required" reqId))
              else do
                let mDenied = case mRef of
                      Nothing -> Nothing
                      Just (g, _) -> checkWriteAccess user g (snapEngine snapB)
                case mDenied of
                  Just denial ->
                    respond (jsonErr status403 (transportError "FORBIDDEN" denial reqId))
                  Nothing -> do
                    let digest = Hash.hashWith Hash.SHA256 bs :: Hash.Digest Hash.SHA256
                        hexHash = T.toLower (TE.decodeUtf8 (convertToBase Base16 digest))
                        pointer = "blob:" <> hexHash
                        contentType = fromMaybe "application/octet-stream"
                                        (lookup (CI.mk "Content-Type") (requestHeaders req))
                        mFilename = lookup (CI.mk "X-Phase-Filename") (requestHeaders req)

                    snapQ <- readIORef (ssSnapshot ss)
                    let waitMs = effQueueTimeoutMs cfg snapQ
                    mResult <- withBoundedEngineLock waitMs (ssEngineLock ss) $ \locked -> do
                      let eng = snapEngine locked
                          mDeniedLock = case mRef of
                            Nothing -> Nothing
                            Just (g, _) -> checkWriteAccess user g eng
                      case mDeniedLock of
                        Just denial -> pure (locked, Left (status403, transportError "FORBIDDEN" denial reqId))
                        Nothing -> do
                          now <- getCurrentTime
                          let metaWrites =
                                [ (("_BLOBS", [toSubKey (mkValue hexHash), toSubKey (mkValue "size")]), (mkValue (T.pack (show (BS.length bs))), Nothing))
                                , (("_BLOBS", [toSubKey (mkValue hexHash), toSubKey (mkValue "contentType")]), (mkValue (TE.decodeUtf8 contentType), Nothing))
                                ] ++
                                [ (("_BLOBS", [toSubKey (mkValue hexHash), toSubKey (mkValue "filename")]), (mkValue (TE.decodeUtf8 fn), Nothing))
                                | Just fn <- [mFilename]
                                ]
                              refWrites = case mRef of
                                Nothing -> []
                                Just (g, subs) ->
                                  [ ((g, map toSubKey subs), (mkValue pointer, Nothing)) ]
                              allWrites = Map.fromList (metaWrites ++ refWrites)
                              txH = TxHandle 0 now allWrites Set.empty Set.empty

                          case commitTransaction user txH now eng of
                            Left sErr ->
                              pure (locked, Left (status400, transportError "BAD_REQUEST" (formatStorageError sErr) reqId))
                            Right (newTx, engCommit) -> do
                              let engWithBlob = writeBlob newTx hexHash bs engCommit
                              storage' <- mask_ (flushPendingWal engWithBlob)
                              let snap' = Snapshot
                                    { snapEngine  = storage'
                                    , snapLocks   = snapLocks locked
                                    , snapVersion = snapVersion locked + 1
                                    }
                              writeIORef (ssSnapshot ss) snap'
                              pure (snap', Right ())
                    case mResult of
                      Nothing -> respond (jsonErr status503 (transportError "BUSY" "server is busy, try again" reqId))
                      Just (Left (st, b)) -> respond (jsonErr st b)
                      Just (Right ()) ->
                        respond (jsonOk (blobUploadBody pointer hexHash (BS.length bs) reqId))

-- | @GET/HEAD /v1/blobs?ref=^GLOBAL(subs)@ handler (§6.5).
handleGetBlobRef :: Bool -> ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Request -> (Response -> IO a) -> IO a
handleGetBlobRef isHead ss guard uRef reqId req respond = do
  let mRefParam = do
        pair <- lookup "ref" (queryString req)
        rawBytes <- pair
        pure (TE.decodeUtf8 rawBytes)
  case mRefParam of
    Nothing ->
      respond (jsonErr status400 (transportError "BAD_REQUEST" "missing ref query parameter" reqId))
    Just refTxt ->
      case parseRefQueryParam refTxt of
        Left err ->
          respond (jsonErr status400 (transportError "BAD_REQUEST" err reqId))
        Right (global, subs) ->
          withUserAuth ss guard uRef (ssConfig ss) reqId req respond $ \user role -> do
            snap <- readIORef (ssSnapshot ss)
            let eng = snapEngine snap
            if role < RRead
              then respond (jsonErr status403 (transportError "FORBIDDEN" "read role required" reqId))
              else case checkReadAccess user global eng of
                Just denial ->
                  respond (jsonErr status403 (transportError "FORBIDDEN" denial reqId))
                Nothing ->
                  case readLive global subs eng of
                    Left _ ->
                      respond (jsonErr status404 (transportError "NOT_FOUND" "node not found" reqId))
                    Right pVal ->
                      case isBlobPointer (unPValue pVal) of
                        Nothing ->
                          respond (jsonErr status400 (transportError "BAD_REQUEST" "node is not a blob pointer" reqId))
                        Just hexHash ->
                          case readBlob hexHash eng of
                            Nothing ->
                              respond (jsonErr status404 (transportError "NOT_FOUND" "blob data not found" reqId))
                            Just bs -> do
                              let ctype = case readLive "_BLOBS" [mkValue hexHash, mkValue "contentType"] eng of
                                    Right ct -> TE.encodeUtf8 (unPValue ct)
                                    Left _   -> "application/octet-stream"
                                  hdrs = [ (hContentType, ctype)
                                         , (CI.mk "Content-Length", BSC.pack (show (BS.length bs)))
                                         ]
                                  body = if isHead then "" else LBS.fromStrict bs
                              respond (responseLBS status200 hdrs body)

-- | @GET/HEAD /v1/blobs/{hash}@ bare-hash handler (§6.5, admin only).
handleGetBlobHash :: Bool -> ServerState -> LoginGuard -> IORef (Maybe Text) -> Text -> Text -> Request -> (Response -> IO a) -> IO a
handleGetBlobHash isHead ss guard uRef reqId rawHash req respond =
  withUserAuth ss guard uRef (ssConfig ss) reqId req respond $ \_user role -> do
    if role < RAdmin
      then respond (jsonErr status403 (transportError "FORBIDDEN" "admin role required" reqId))
      else do
        snap <- readIORef (ssSnapshot ss)
        let eng = snapEngine snap
            hexHash = T.toLower rawHash
        case readBlob hexHash eng of
          Nothing ->
            respond (jsonErr status404 (transportError "NOT_FOUND" "blob not found" reqId))
          Just bs -> do
            let ctype = case readLive "_BLOBS" [mkValue hexHash, mkValue "contentType"] eng of
                  Right ct -> TE.encodeUtf8 (unPValue ct)
                  Left _   -> "application/octet-stream"
                hdrs = [ (hContentType, ctype)
                       , (CI.mk "Content-Length", BSC.pack (show (BS.length bs)))
                       ]
                body = if isHead then "" else LBS.fromStrict bs
            respond (responseLBS status200 hdrs body)

-- --------------------------------------------------------------------
-- Response helpers
-- --------------------------------------------------------------------

-- | Turn an 'ExecOutcome' into the appropriate WAI 'Response'.
outcomeResponse :: Text -> Int -> TxId -> ExecOutcome -> Response
outcomeResponse reqId elapsed lastTx = \case
  ExecOk out val tx reg ->
    jsonOk (runEnvelope out val tx elapsed reqId reg)
  ExecError err tx ->
    jsonOk (errorEnvelope err tx elapsed reqId)
  ExecTimeout ->
    jsonOk (serverErrorEnvelope "TIMEOUT" "<TIMEOUT> execution timed out" lastTx elapsed reqId)
  ExecParseError msg ->
    jsonOk (errorEnvelope (SyntaxError msg) lastTx elapsed reqId)
  ExecBusy ->
    jsonErr status503 (transportError "BUSY" "server is busy, try again" reqId)

jsonHeadersFor :: LBS.ByteString -> [(HeaderName, ByteString)]
jsonHeadersFor body =
  [ (hContentType, "application/json; charset=utf-8")
  , (CI.mk "Content-Length", BSC.pack (show (LBS.length body)))
  ]

jsonOk :: LBS.ByteString -> Response
jsonOk body = responseLBS status200 (jsonHeadersFor body) body

jsonCreated :: LBS.ByteString -> Response
jsonCreated body = responseLBS status201 (jsonHeadersFor body) body

jsonErr :: Status -> LBS.ByteString -> Response
jsonErr st body = responseLBS st (jsonHeadersFor body) body

jsonErrHdr :: Status -> [(HeaderName, ByteString)] -> LBS.ByteString -> Response
jsonErrHdr st extra body = responseLBS st (jsonHeadersFor body ++ extra) body

-- --------------------------------------------------------------------
-- Miscellaneous helpers
-- --------------------------------------------------------------------

-- | Read the request body up to a maximum byte count.
-- Returns 'Nothing' if the body exceeds the limit.
readBodyCapped :: Int -> Request -> IO (Maybe ByteString)
readBodyCapped cap req = go 0 []
  where
    go total acc = do
      chunk <- getRequestBodyChunk req
      if BS.null chunk
        then pure (Just (BS.concat (reverse acc)))
        else do
          let newTotal = total + BS.length chunk
          if newTotal > cap
            then pure Nothing
            else go newTotal (chunk : acc)

-- | Look up a request header by name.
lookupHeader :: HeaderName -> Request -> Maybe ByteString
lookupHeader name req = lookup name (requestHeaders req)

-- | Generate a short random-ish request id from the current time.
-- For production a proper UUID or CSPRNG hex would be better; this is
-- sufficient for a v1 that doesn't need global uniqueness guarantees.
generateRequestId :: IO Text
generateRequestId = do
  -- Use monotonic clock nanoseconds as a cheap unique id.
  TimeSpec sec nsec <- getTime Monotonic
  pure (T.pack (show sec ++ show nsec))

-- | Remote address as a string key for per-IP rate limiting.
addrKey :: Request -> Text
addrKey req = T.pack (show (remoteHost req))

-- | Milliseconds between two 'TimeSpec' readings.
elapsedMs :: TimeSpec -> TimeSpec -> Int
elapsedMs (TimeSpec s0 n0) (TimeSpec s1 n1) =
  fromIntegral ((s1 - s0) * 1000 + (n1 - n0) `div` 1_000_000)

-- | Clamp a client-supplied timeout to the server's configured max.
clampTimeout :: ServerConfig -> Snapshot -> Maybe Int -> Int
clampTimeout cfg snap mReq =
  let def = effExecTimeoutMs  cfg snap
      cap = effExecTimeoutMaxMs cfg snap
      req = maybe def id mReq
  in min req cap
