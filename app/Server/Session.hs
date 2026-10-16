{-# LANGUAGE OverloadedStrings #-}

-- | Session management for M2 (§5.2, §6.2, §8).
--
-- Sessions are identified server-side by the SHA-256 digest of a 32-byte
-- random token.  Only the digest is stored; the raw token is sent to the
-- client once and never persisted.  Idle and absolute timeouts apply; a
-- session that times out is cleaned up lazily on the next lookup.
--
-- Concurrent requests in one session queue behind a per-session 'MVar'
-- (D6): whoever holds it is the active request.
module Server.Session
  ( -- * Types
    TokenDigest
  , Session (..)
  , SessionMap

    -- * Token generation & hashing
  , generateToken
  , tokenDigest

    -- * Session lifecycle
  , newSession
  , lookupSession
  , deleteSession
  , touchSession
  , anyExpired
  , reapExpired

    -- * Lock release on session close
  , sessionJobId
  ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, tryTakeMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar, readTVarIO)
import qualified Crypto.Hash as Hash
import qualified Crypto.Random.Entropy as Entropy
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time (getCurrentTime)

import Control.Monad (forM)
import Data.Maybe (catMaybes)

import Phase.State (Env (..))
import Phase.Storage.Lock (LockTable, emptyLockTable, releaseAllOwnerLocks)

-- | The SHA-256 digest of a raw bearer token, hex-encoded.
-- This is what the server stores; the raw token is never kept.
type TokenDigest = Text

-- | A single authenticated session.
data Session = Session
  { sessUser      :: !Text        -- ^ Authenticated username.
  , sessJobId     :: !Text        -- ^ Unique job-id string ($JOB).
  , sessNamespace :: !Text        -- ^ Current namespace (updated per request).
  , sessEnv       :: !Env         -- ^ Persistent locals between requests.
  , sessCreatedAt :: !UTCTime     -- ^ Absolute creation time.
  , sessLastUsed  :: !UTCTime     -- ^ Last-touched time (for idle timeout).
  , sessLock      :: !(MVar ())   -- ^ Per-session serialisation lock (D6).
  }

-- | The live session table.
type SessionMap = Map TokenDigest Session

-- | The job-id associated with a session (used to release LOCKs).
sessionJobId :: Session -> Text
sessionJobId = sessJobId

-- --------------------------------------------------------------------
-- Token operations
-- --------------------------------------------------------------------

-- | Generate a fresh 32-byte random token, hex-encoded (64 chars).
-- Uses the same entropy source as 'Phase.Auth.generateSalt'.
generateToken :: IO Text
generateToken = do
  bytes <- Entropy.getEntropy 32 :: IO BS.ByteString
  pure (TE.decodeUtf8 (convertToBase Base16 bytes))

-- | SHA-256 digest of a raw token, hex-encoded.
-- The server stores only the digest; the plaintext token is sent to the
-- client once and never persisted.
tokenDigest :: Text -> TokenDigest
tokenDigest raw =
  let digest = Hash.hashWith Hash.SHA256 (TE.encodeUtf8 raw)
  in TE.decodeUtf8 (convertToBase Base16 digest)

-- --------------------------------------------------------------------
-- Session lifecycle
-- --------------------------------------------------------------------

-- | Create a new session and insert it into the table.
-- Returns the raw token (sent to client once) and the digest (stored).
newSession :: Text    -- ^ Username
           -> Text    -- ^ Initial namespace
           -> Env     -- ^ Initial environment (fresh locals)
           -> TVar SessionMap
           -> IO (Text, TokenDigest)  -- ^ (raw token, digest)
newSession user ns env0 tvar = do
  raw  <- generateToken
  -- The job id ($JOB, and the owner of this session's LOCKs) is random and
  -- independent of the token: deriving it from the token would let a
  -- session print part of its own bearer token with WRITE $JOB.
  jobId <- T.take 16 <$> generateToken
  now  <- getCurrentTime
  lock <- newMVar ()
  let digest = tokenDigest raw
      sess   = Session
        { sessUser      = user
        , sessJobId     = jobId
        , sessNamespace = ns
        , sessEnv       = env0 { envJobId = jobId }
        , sessCreatedAt = now
        , sessLastUsed  = now
        , sessLock      = lock
        }
  atomically $ modifyTVar' tvar (Map.insert digest sess)
  pure (raw, digest)

-- | Look up a session by its token digest, checking both timeouts.
-- Returns 'Nothing' for an unknown or expired session.  It never removes
-- anything: a session's locks must be released together with its removal,
-- which 'Server.Execute.sweepExpiredSessions' does under the engine lock.
lookupSession :: TokenDigest
              -> Int        -- ^ Idle timeout in seconds
              -> Int        -- ^ Absolute lifetime in seconds
              -> TVar SessionMap
              -> IO (Maybe Session)
lookupSession digest idleSecs maxSecs tvar = do
  now  <- getCurrentTime
  smap <- atomically (readTVar tvar)
  pure $ case Map.lookup digest smap of
    Just sess | not (isExpired now idleSecs maxSecs sess) -> Just sess
    _                                                      -> Nothing

isExpired :: UTCTime -> Int -> Int -> Session -> Bool
isExpired now idleS maxS sess =
  let idleExpiry = addUTCTime (fromIntegral idleS) (sessLastUsed  sess)
      absExpiry  = addUTCTime (fromIntegral maxS)  (sessCreatedAt sess)
  in now > idleExpiry || now > absExpiry

-- | Update a session's last-used time and persistent environment.
touchSession :: TokenDigest -> Env -> Text -> TVar SessionMap -> IO ()
touchSession digest env' ns tvar = do
  now <- getCurrentTime
  atomically $ modifyTVar' tvar $ Map.adjust
    (\s -> s { sessLastUsed = now, sessEnv = env', sessNamespace = ns }) digest

-- | Remove a session from the table.
deleteSession :: TokenDigest -> TVar SessionMap -> IO ()
deleteSession digest tvar =
  atomically $ modifyTVar' tvar (Map.delete digest)

-- | Is any session past its idle or absolute lifetime?  A cheap check, so
-- the common case (nothing expired) needs no lock at all.
anyExpired :: Int -> Int -> TVar SessionMap -> IO Bool
anyExpired idleSecs maxSecs tvar = do
  now  <- getCurrentTime
  smap <- readTVarIO tvar
  pure (any (isExpired now idleSecs maxSecs) (Map.elems smap))

-- | Remove the expired sessions and return them, so the caller can release
-- their locks.  A session with a request in flight (its per-session lock is
-- held) is skipped: it is not idle, and removing it would leak whatever
-- locks that request is about to take.  The session lock is taken for the
-- removal and handed back afterwards, so a request already waiting on it
-- wakes up, looks the session up again and gets a 401.
reapExpired :: Int -> Int -> TVar SessionMap -> IO [Session]
reapExpired idleSecs maxSecs tvar = do
  now  <- getCurrentTime
  smap <- readTVarIO tvar
  let candidates = [ (d, s) | (d, s) <- Map.toList smap, isExpired now idleSecs maxSecs s ]
  fmap catMaybes $ forM candidates $ \(digest, sess) -> do
    got <- tryTakeMVar (sessLock sess)
    case got of
      Nothing -> pure Nothing
      Just () -> do
        atomically $ modifyTVar' tvar (Map.delete digest)
        putMVar (sessLock sess) ()
        pure (Just sess)

-- | Extract the lock table entries owned by a session's job-id.
-- Used when a session expires or is explicitly deleted so its LOCKs
-- are released from the shared lock table.
releaseSessionLocks :: Session -> LockTable -> LockTable
releaseSessionLocks sess lt =
  releaseAllOwnerLocks (sessJobId sess) lt
