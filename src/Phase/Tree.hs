{-# LANGUAGE OverloadedStrings #-}

-- | Sparse hierarchical storage tree with MUMPS node duality and collation ordering.
--
-- MUMPS/Phase variables (both locals and globals) are sparse multi-dimensional
-- trees where any node may simultaneously hold a value and have child nodes
-- indexed by subscripts. Subscripts are ordered according to Phase's
-- canonical-numeric and collation rules ('Phase.Value.collationCompare').
module Phase.Tree
  ( SubscriptKey (..)
  , toSubKey
  , Node (..)
  , emptyNode
  , isNodeEmpty
  , getNode
  , findSubNode
  , setNode
  , killNode
  , dataNode
  , orderNode
  , childCountNode
  , mergeNode
  , dumpTree
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Phase.AST (PValue (..))
import Phase.Value (collationCompare)

-- | A subscript key wrapping 'PValue' whose 'Ord' instance follows Phase's
-- canonical numeric collation order (canonical numbers before strings,
-- numbers in numeric order, strings in character order).
newtype SubscriptKey = SubscriptKey { unSubscriptKey :: PValue }
  deriving (Eq, Show)

-- | Construct a 'SubscriptKey' from a 'PValue'.
toSubKey :: PValue -> SubscriptKey
toSubKey = SubscriptKey

instance Ord SubscriptKey where
  compare (SubscriptKey a) (SubscriptKey b) = collationCompare a b

-- | A node in the Phase hierarchical storage tree.
data Node = Node
  { nodeValue    :: !(Maybe PValue)
  , nodeChildren :: !(Map SubscriptKey Node)
  } deriving (Eq, Show)

-- | An empty tree node with no value and no children.
emptyNode :: Node
emptyNode = Node Nothing Map.empty

-- | Test if a node has neither value nor children.
isNodeEmpty :: Node -> Bool
isNodeEmpty (Node Nothing children) = Map.null children
isNodeEmpty _                       = False

-- | Read a value at a given subscript path.
-- Empty path reads the node's own bare value (node duality).
getNode :: [PValue] -> Node -> Maybe PValue
getNode [] node = nodeValue node
getNode (k : ks) node =
  case Map.lookup (SubscriptKey k) (nodeChildren node) of
    Nothing    -> Nothing
    Just child -> getNode ks child

-- | Find the whole SUBNODE at a given subscript path (its own value,
-- if any, AND its children), unlike 'getNode', which only returns
-- the value there. Lets a caller inspect or enumerate (via 'dumpTree')
-- everything under a path before an operation (like a kill) that is
-- about to remove it.
findSubNode :: [PValue] -> Node -> Maybe Node
findSubNode [] node = Just node
findSubNode (k : ks) node =
  case Map.lookup (SubscriptKey k) (nodeChildren node) of
    Nothing    -> Nothing
    Just child -> findSubNode ks child

-- | Set a value at a given subscript path, creating intermediate nodes as needed.
setNode :: [PValue] -> PValue -> Node -> Node
setNode [] val node = node { nodeValue = Just val }
setNode (k : ks) val node =
  let key = SubscriptKey k
      child = Map.findWithDefault emptyNode key (nodeChildren node)
      child' = setNode ks val child
  in node { nodeChildren = Map.insert key child' (nodeChildren node) }

-- | Delete a value and/or subtree at a given subscript path.
-- If the path is empty, the entire node/tree is cleared (returns 'Nothing').
-- If the path is non-empty, the descendant subtree is pruned.
killNode :: [PValue] -> Node -> Maybe Node
killNode [] _ = Nothing
killNode (k : ks) node =
  let key = SubscriptKey k
  in case Map.lookup key (nodeChildren node) of
    Nothing -> Just node
    Just child ->
      case killNode ks child of
        Nothing ->
          let children' = Map.delete key (nodeChildren node)
              node' = node { nodeChildren = children' }
          in if isNodeEmpty node' then Nothing else Just node'
        Just child' ->
          let children' = Map.insert key child' (nodeChildren node)
          in Just (node { nodeChildren = children' })

-- | Perform Phase's 4-state existence check ($DATA, Part I §6):
--
-- * @0@:  No value at this exact node, no children either.
-- * @1@:  Has a value at this exact node, no children.
-- * @10@: No value at this exact node, but has children.
-- * @11@: Has a value at this exact node, and has children.
dataNode :: [PValue] -> Node -> Int
dataNode path root =
  case findSubNode path root of
    Nothing -> 0
    Just target ->
      let hasVal = isJust (nodeValue target)
          hasChildren = not (Map.null (nodeChildren target))
      in case (hasVal, hasChildren) of
        (False, False) -> 0
        (True,  False) -> 1
        (False, True)  -> 10
        (True,  True)  -> 11
  where
    findSubNode [] n = Just n
    findSubNode (k : ks) n =
      case Map.lookup (SubscriptKey k) (nodeChildren n) of
        Nothing    -> Nothing
        Just child -> findSubNode ks child

-- | Walk a subscript level ($ORDER, Part I §6).
-- Given a parent path @path@ and a current subscript @curSub@:
--
-- * When @curSub@ is @""@ (empty), returns the first subscript in collation order.
-- * Otherwise, returns the next subscript strictly greater than @curSub@ in collation order.
-- * Returns @Nothing@ (or @Just (mkValue "")@ for end-of-iteration) when no further keys exist.
orderNode :: [PValue] -> PValue -> Node -> Maybe PValue
orderNode path curSub root = do
  parent <- findSubNode path root
  let children = nodeChildren parent
  if unPValue curSub == ""
    then case Map.lookupMin children of
      Nothing           -> Nothing
      Just (minKey, _)  -> Just (unSubscriptKey minKey)
    else case Map.lookupGT (SubscriptKey curSub) children of
      Nothing           -> Nothing
      Just (nextKey, _) -> Just (unSubscriptKey nextKey)
  where
    findSubNode [] n = Just n
    findSubNode (k : ks) n =
      case Map.lookup (SubscriptKey k) (nodeChildren n) of
        Nothing    -> Nothing
        Just child -> findSubNode ks child

-- | Count the number of immediate children at a given subscript path ($CHILDCOUNT).
-- Returns 'Nothing' when the path does not exist (no node at all), so callers
-- can distinguish "node exists but has no children" (Just 0) from "node does
-- not exist" (Nothing). The caller in 'Phase.State.childCountRef' maps both
-- to the integer 0, $CHILDCOUNT is defined to return 0 for either case, since
-- Phase has no "undefined function argument" error analogous to $DATA.
childCountNode :: [PValue] -> Node -> Maybe Int
childCountNode path root = do
  target <- findSubNode path root
  pure (Map.size (nodeChildren target))
  where
    findSubNode [] n = Just n
    findSubNode (k : ks) n =
      case Map.lookup (SubscriptKey k) (nodeChildren n) of
        Nothing    -> Nothing
        Just child -> findSubNode ks child

-- | Merge a source subtree into a target subtree (Part X §1).
-- Reads every live value under @source@ (including root) and writes it into
-- the matching position under @target@, without clearing existing target values.
mergeNode :: [PValue] -> Node -> [PValue] -> Node -> Node
mergeNode dstPath dstNode srcPath srcNode =
  case findSubNode srcPath srcNode of
    Nothing      -> dstNode
    Just srcTree -> updateSubNode dstPath (`mergeTrees` srcTree) dstNode
  where
    findSubNode [] n = Just n
    findSubNode (k : ks) n =
      case Map.lookup (SubscriptKey k) (nodeChildren n) of
        Nothing    -> Nothing
        Just child -> findSubNode ks child

    updateSubNode [] f n = f n
    updateSubNode (k : ks) f n =
      let key = SubscriptKey k
          child = Map.findWithDefault emptyNode key (nodeChildren n)
          child' = updateSubNode ks f child
      in n { nodeChildren = Map.insert key child' (nodeChildren n) }

    mergeTrees :: Node -> Node -> Node
    mergeTrees dst src = Node
      { nodeValue = case nodeValue src of
                      Just v  -> Just v
                      Nothing -> nodeValue dst
      , nodeChildren = Map.unionWith mergeTrees (nodeChildren dst) (nodeChildren src)
      }

-- | Dump all live paths and values from a tree in collation order.
dumpTree :: Node -> [([PValue], PValue)]
dumpTree = go []
  where
    go prefix node =
      let self = case nodeValue node of
                   Just v  -> [(prefix, v)]
                   Nothing -> []
          children = concatMap (\(SubscriptKey k, child) -> go (prefix ++ [k]) child)
                               (Map.toAscList (nodeChildren node))
      in self ++ children
