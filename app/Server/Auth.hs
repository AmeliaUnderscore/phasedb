{-# LANGUAGE OverloadedStrings #-}

-- | Authentication helpers: Basic/Bearer credential extraction, loopback
-- detection, PBKDF2 semaphore (CPU-cost guard), and per-username /
-- per-address rate limiting (§5, §5.3).
module Server.Auth
  ( -- * Credential extraction
    Credential (..)
  , extractCredential
  , CredError (..)

    -- * Basic-auth loopback guard
  , isLoopback

    -- * Login: PBKDF2 semaphore + rate limiting
  , LoginGuard
  , newLoginGuard
  , tryLogin
  , LoginResult (..)

    -- * Constant dummy PBKDF2 for user-enumeration resistance
  , dummyVerify
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Concurrent.QSem (QSem, newQSem, waitQSem, signalQSem)
import Control.Exception (bracket_)
import Data.Char (isAlphaNum, isPrint)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Network.Socket (SockAddr (..))

import Phase.Auth (authenticateUser, hasAnyUser, userExists, verifyPassword)
import Phase.Auth (hashPassword)
import Phase.Storage.Engine (StorageEngine)

-- --------------------------------------------------------------------
-- Credential extraction
-- --------------------------------------------------------------------

-- | The two authentication modes the server accepts.
data Credential
  = BasicCred !Text !Text  -- ^ username, plaintext password
  | BearerCred !Text       -- ^ raw token (64 hex chars)
  deriving (Show)

data CredError
  = NoCredential
  | MalformedCredential !Text
  deriving (Show)

-- | Parse the @Authorization@ header value.
extractCredential :: BS.ByteString -> Either CredError Credential
extractCredential hdr
  | "Basic "  `BS.isPrefixOf` hdr = parseBasic  (BS.drop 6 hdr)
  | "Bearer " `BS.isPrefixOf` hdr = parseBearer (BS.drop 7 hdr)
  | BS.null hdr                    = Left NoCredential
  | otherwise                      = Left (MalformedCredential "unrecognised scheme")
  where
    parseBasic b64 =
      case B64.decode b64 of
        Left _    -> Left (MalformedCredential "invalid base64")
        Right raw ->
          let (u, rest) = BSC.span (/= ':') raw
          in if BS.null rest
               then Left (MalformedCredential "missing colon in Basic credentials")
               else Right (BasicCred (TE.decodeUtf8Lenient u)
                                     (TE.decodeUtf8Lenient (BS.tail rest)))
    parseBearer tok =
      let t = T.strip (TE.decodeUtf8Lenient tok)
      in if T.length t == 64 && T.all (\c -> (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')) t
           then Right (BearerCred t)
           else Left (MalformedCredential "bearer token must be 64 lowercase hex chars")

-- | Is this address a loopback address?
-- Used to decide whether to allow Basic auth without TLS (§5.1).
isLoopback :: SockAddr -> Bool
isLoopback (SockAddrInet  _ host)     = host == 0x0100007f  -- 127.0.0.1
isLoopback (SockAddrInet6 _ _ host _) = host == (0, 0, 0, 1) -- ::1
isLoopback _                          = False

-- --------------------------------------------------------------------
-- Login guard: PBKDF2 semaphore + rate limiting
-- --------------------------------------------------------------------

-- | State for one tracked address or username.
data FailureRecord = FailureRecord
  { frCount    :: !Int
  , frLockedUntil :: !(Maybe UTCTime)
  } deriving (Show)

-- | Per-server login guard, held in 'Server.State.ServerState'.
data LoginGuard = LoginGuard
  { lgSem          :: !QSem          -- ^ Limits concurrent PBKDF2 verifications.
  , lgByUsername   :: !(IORef (Map Text FailureRecord))
  , lgByAddress    :: !(IORef (Map Text FailureRecord))
  }

-- | Create a new 'LoginGuard' with the given semaphore width
-- (default 4 per §5.3).
newLoginGuard :: Int -> IO LoginGuard
newLoginGuard concurrency = do
  sem  <- newQSem concurrency
  uref <- newIORef Map.empty
  aref <- newIORef Map.empty
  pure LoginGuard { lgSem = sem, lgByUsername = uref, lgByAddress = aref }

data LoginResult
  = LoginOk
  | LoginFailed              -- ^ Wrong credentials (same body for unknown user).
  | LoginRateLimited !Int    -- ^ Try again in N seconds.
  deriving (Show, Eq)

-- | Backoff delay in seconds after N consecutive failures (exponential,
-- capped at 30 minutes).
backoffSeconds :: Int -> Int
backoffSeconds n
  | n <= 0    = 0
  | n == 1    = 2
  | n == 2    = 4
  | n == 3    = 8
  | n == 4    = 16
  | n == 5    = 32
  | n <= 8    = 60
  | otherwise = 1800

-- | Attempt a login with CPU-cost and rate-limiting guards.
-- 'addrKey' should be the remote IP string.
tryLogin :: LoginGuard
         -> Text         -- ^ Username (from request)
         -> Text         -- ^ Password (from request)
         -> Text         -- ^ Address key (for per-IP limiting)
         -> StorageEngine
         -> IO LoginResult
tryLogin guard username password addrKey eng = do
  now <- getCurrentTime
  -- Check address-level rate limit first (cheapest, no DB access needed).
  addrResult <- checkLimit (lgByAddress guard) addrKey now
  case addrResult of
    Just retryAfter -> pure (LoginRateLimited retryAfter)
    Nothing -> do
      -- Check username-level rate limit.
      userResult <- checkLimit (lgByUsername guard) username now
      case userResult of
        Just retryAfter -> pure (LoginRateLimited retryAfter)
        Nothing -> do
          -- Acquire semaphore before running the expensive PBKDF2.
          bracket_ (waitQSem (lgSem guard)) (signalQSem (lgSem guard)) $ do
            let ok = if userExists username eng
                       then authenticateUser username password eng
                       else dummyVerify password
            now2 <- getCurrentTime
            if ok
              then do
                clearLimit (lgByUsername guard) username
                clearLimit (lgByAddress  guard) addrKey
                pure LoginOk
              else do
                recordFailure (lgByUsername guard) username now2
                recordFailure (lgByAddress  guard) addrKey  now2
                pure LoginFailed

-- | Check if a key is currently locked out.  Returns 'Just n' where
-- n is the seconds remaining, or 'Nothing' if the key is clear.
checkLimit :: IORef (Map Text FailureRecord) -> Text -> UTCTime -> IO (Maybe Int)
checkLimit ref key now = do
  m <- readIORef ref
  case Map.lookup key m of
    Nothing -> pure Nothing
    Just fr ->
      case frLockedUntil fr of
        Nothing -> pure Nothing
        Just lockedUntil
          | now >= lockedUntil -> do
              atomicModifyIORef' ref (\mp -> (Map.delete key mp, ()))
              pure Nothing
          | otherwise ->
              let remaining = ceiling (realToFrac (diffUTCTime lockedUntil now) :: Double)
              in pure (Just remaining)

-- | Record a login failure and update the backoff timer.
recordFailure :: IORef (Map Text FailureRecord) -> Text -> UTCTime -> IO ()
recordFailure ref key now =
  atomicModifyIORef' ref $ \m ->
    let old   = Map.findWithDefault (FailureRecord 0 Nothing) key m
        count = frCount old + 1
        delay = backoffSeconds count
        until = addUTCTime (fromIntegral delay) now
        new   = FailureRecord count (if delay > 0 then Just until else Nothing)
    in (Map.insert key new m, ())

-- | Clear the failure record for a key after a successful login.
clearLimit :: IORef (Map Text FailureRecord) -> Text -> IO ()
clearLimit ref key = atomicModifyIORef' ref (\m -> (Map.delete key m, ()))

-- --------------------------------------------------------------------
-- Dummy verify (user-enumeration resistance)
-- --------------------------------------------------------------------

-- | Perform a PBKDF2 hash even when the user does not exist, so the
-- response time is indistinguishable from a wrong-password case (§5.3).
-- Uses a fixed dummy salt; the result is always @False@.
dummyVerify :: Text -> Bool
dummyVerify password =
  let dummy = "00000000000000000000000000000000" :: Text
      hash  = hashPassword password dummy
  in hash == ("" :: Text)  -- never true; the work is done by hashPassword
