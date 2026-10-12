{-# LANGUAGE OverloadedStrings #-}

-- | Transaction handles, change staging, and optimistic concurrency verification
-- for PhaseDB (Part III §2 and Part XII §2).
--
-- 'TxHandle' stages mutations (SET, point KILL, subtree KILL) and validates
-- optimistic-concurrency assertions ('EXPECTING') before committing to storage.
module Phase.Storage.Transaction
  ( -- * Transaction Handle
    TxHandle (..)
  , newTxHandle

    -- * Mutation Staging
  , stageSet
  , stageKill
  , stageKillSubtree
  , hasStagedChanges
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)

import Phase.AST (PValue)
import Phase.Storage.Types (TxId)
import Phase.Tree (SubscriptKey, toSubKey)

-- | Transaction handle tracking staged changes and optimistic concurrency assertions.
data TxHandle = TxHandle
  { txId                 :: !TxId
  , txStartTime          :: !UTCTime
  , txStagedWrites       :: !(Map (Text, [SubscriptKey]) (PValue, Maybe TxId))
  , txStagedPointKills   :: !(Set (Text, [SubscriptKey]))
  , txStagedSubtreeKills :: !(Set (Text, [SubscriptKey]))
  } deriving (Eq, Show)

-- | Create a new transaction handle.
newTxHandle :: TxId -> UTCTime -> TxHandle
newTxHandle tid startTime = TxHandle
  { txId                 = tid
  , txStartTime          = startTime
  , txStagedWrites       = Map.empty
  , txStagedPointKills   = Set.empty
  , txStagedSubtreeKills = Set.empty
  }

-- | Stage a SET mutation with optional expectedTx assertion.
stageSet :: Text -> [PValue] -> PValue -> Maybe TxId -> TxHandle -> TxHandle
stageSet global subs val mExpected tx =
  let key = (global, map toSubKey subs)
      writes' = Map.insert key (val, mExpected) (txStagedWrites tx)
      pointKills' = Set.delete key (txStagedPointKills tx)
      subtreeKills' = Set.delete key (txStagedSubtreeKills tx)
  in tx { txStagedWrites = writes', txStagedPointKills = pointKills', txStagedSubtreeKills = subtreeKills' }

-- | Stage a point KILL mutation.
stageKill :: Text -> [PValue] -> TxHandle -> TxHandle
stageKill global subs tx =
  let key = (global, map toSubKey subs)
      writes' = Map.delete key (txStagedWrites tx)
      pointKills' = Set.insert key (txStagedPointKills tx)
  in tx { txStagedWrites = writes', txStagedPointKills = pointKills' }

-- | Stage a subtree KILL mutation.
stageKillSubtree :: Text -> [PValue] -> TxHandle -> TxHandle
stageKillSubtree global subs tx =
  let key = (global, map toSubKey subs)
      writes' = Map.delete key (txStagedWrites tx)
      pointKills' = Set.delete key (txStagedPointKills tx)
      subtreeKills' = Set.insert key (txStagedSubtreeKills tx)
  in tx { txStagedWrites = writes', txStagedPointKills = pointKills', txStagedSubtreeKills = subtreeKills' }

-- | Check if this transaction has any uncommitted staged modifications.
hasStagedChanges :: TxHandle -> Bool
hasStagedChanges tx =
  not (Map.null (txStagedWrites tx)) ||
  not (Set.null (txStagedPointKills tx)) ||
  not (Set.null (txStagedSubtreeKills tx))
