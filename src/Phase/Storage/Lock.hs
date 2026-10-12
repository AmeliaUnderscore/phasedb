{-# LANGUAGE OverloadedStrings #-}

-- | Advisory, path-granular, reentrant lock management for PhaseDB (Part XII §3).
--
-- MUMPS/Phase locks are advisory signals between cooperative processes.
-- Locking '^ORDERS(1)' is path-granular (it does not lock '^ORDERS(1,"PRICE")')
-- and reentrant per owner process ID.
module Phase.Storage.Lock
  ( -- * Lock Table
    LockTable
  , emptyLockTable

    -- * Lock Operations
  , acquireLocks
  , releaseLocks
  , releaseAllOwnerLocks
  , isPathLocked
  , getPathOwner
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)

import Phase.Storage.Types (GlobalPath (..))
import Phase.Tree (SubscriptKey, toSubKey)

-- | Internal path key representation for lock table.
type LockKey = (Text, [SubscriptKey])

pathToKey :: GlobalPath -> LockKey
pathToKey (GlobalPath g subs) = (g, map toSubKey subs)

-- | Lock Table mapping global paths to (Owner Process ID, Reentrancy Depth).
newtype LockTable = LockTable
  { unLockTable :: Map LockKey (Text, Int)
  } deriving (Eq, Show)

-- | An initial, empty lock table.
emptyLockTable :: LockTable
emptyLockTable = LockTable Map.empty

-- | Acquire a list of locks atomically.
-- If all locks are either unheld or already held by the requesting owner,
-- their reentrancy counts are incremented and 'True' is returned.
-- Otherwise, returns 'False' without modifying the table.
acquireLocks :: Text -> [GlobalPath] -> LockTable -> (Bool, LockTable)
acquireLocks owner paths lt@(LockTable table) =
  let canAcquire key = case Map.lookup key table of
                         Nothing       -> True
                         Just (o, _)   -> o == owner
      keys = map pathToKey paths
  in if all canAcquire keys
       then
         let updateKey m k =
               case Map.lookup k m of
                 Nothing     -> Map.insert k (owner, 1) m
                 Just (o, d) -> Map.insert k (o, d + 1) m
             table' = foldl updateKey table keys
         in (True, LockTable table')
       else (False, lt)

-- | Release a list of locks for an owner, decrementing reentrancy counts.
-- When depth reaches 0, the lock is freed.
releaseLocks :: Text -> [GlobalPath] -> LockTable -> LockTable
releaseLocks owner paths (LockTable table) =
  let keys = map pathToKey paths
      releaseKey m k =
        case Map.lookup k m of
          Just (o, d) | o == owner ->
            if d <= 1
              then Map.delete k m
              else Map.insert k (o, d - 1) m
          _ -> m
  in LockTable (foldl releaseKey table keys)

-- | Release all locks held by a specific owner process.
releaseAllOwnerLocks :: Text -> LockTable -> LockTable
releaseAllOwnerLocks owner (LockTable table) =
  LockTable (Map.filter (\(o, _) -> o /= owner) table)

-- | Check if a path is currently locked.
isPathLocked :: GlobalPath -> LockTable -> Bool
isPathLocked path (LockTable table) =
  Map.member (pathToKey path) table

-- | Get the owner of a lock at a given path.
getPathOwner :: GlobalPath -> LockTable -> Maybe Text
getPathOwner path (LockTable table) =
  fst <$> Map.lookup (pathToKey path) table
