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
  , expireAll

    -- * Lock release on session close
  , sessionJobId
  ) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', readTVar)
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

import Phase.State (Env)
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
  now  <- getCurrentTime
  lock <- newMVar ()
  let digest = tokenDigest raw
      sess   = Session
        { sessUser      = user
        , sessJobId     = T.take 16 raw  -- first 16 hex chars as $JOB
        , sessNamespace = ns
        , sessEnv       = env0
        , sessCreatedAt = now
        , sessLastUsed  = now
        , sessLock      = lock
        }
  atomically $ modifyTVar' tvar (Map.insert digest sess)
  pure (raw, digest)

-- | Look up a session by its token digest, checking both timeouts.
-- Returns 'Nothing' and removes the session if it has expired.
lookupSession :: TokenDigest
              -> Int        -- ^ Idle timeout in seconds
              -> Int        -- ^ Absolute lifetime in seconds
              -> TVar SessionMap
              -> IO (Maybe Session)
lookupSession digest idleSecs maxSecs tvar = do
  now  <- getCurrentTime
  smap <- atomically (readTVar tvar)
  case Map.lookup digest smap of
    Nothing   -> pure Nothing
    Just sess ->
      let idleExpiry = addUTCTime (fromIntegral idleSecs) (sessLastUsed  sess)
          absExpiry  = addUTCTime (fromIntegral maxSecs)  (sessCreatedAt sess)
      in if now > idleExpiry || now > absExpiry
           then do
             atomically $ modifyTVar' tvar (Map.delete digest)
             pure Nothing
           else pure (Just sess)

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

-- | Remove all expired sessions, releasing their lock-table entries.
-- Called lazily on each request.
expireAll :: Int -> Int -> TVar SessionMap -> IO (Map TokenDigest Session)
expireAll idleSecs maxSecs tvar = do
  now  <- getCurrentTime
  atomically $ do
    smap <- readTVar tvar
    let live = Map.filter (not . isExpired now idleSecs maxSecs) smap
    modifyTVar' tvar (const live)
    pure live
  where
    isExpired now idleS maxS sess =
      let idleExpiry = addUTCTime (fromIntegral idleS) (sessLastUsed  sess)
          absExpiry  = addUTCTime (fromIntegral maxS)  (sessCreatedAt sess)
      in now > idleExpiry || now > absExpiry

-- | Extract the lock table entries owned by a session's job-id.
-- Used when a session expires or is explicitly deleted so its LOCKs
-- are released from the shared lock table.
releaseSessionLocks :: Session -> LockTable -> LockTable
releaseSessionLocks sess lt =
  releaseAllOwnerLocks (sessJobId sess) lt
