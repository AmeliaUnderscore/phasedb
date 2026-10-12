{-# LANGUAGE OverloadedStrings #-}

-- | The ^_CATALOG system global repository for namespaces, functions, routines,
-- constants, and primitive version control.
--
-- In Phase, system metadata, routines, and namespace mappings are stored natively
-- in the hierarchical global array ^_CATALOG. Every modification to a function or
-- constant automatically archives the previous version under the HISTORY branch,
-- providing durable, database-backed version tracking and rollback capabilities.
module Phase.Catalog
  ( -- * Types
    ItemKind (..)
  , catalogGlobalName

    -- * Definition and Lookup
  , defineItem
  , lookupItem
  , getItemHistory
  , revertItem
  , parseSignature
  , registerRoutineTags

    -- * Introspection
  , listNamespaces
  , listItems
  ) where

import Data.Text (Text)
import qualified Data.Text as T

import Phase.AST (PValue (..), VarKind (..), mkValue)
import Phase.Auth (Role (..), lookupUserRole, roleFromText, roleToText)
import Phase.Lines (LogicalLine (..), buildLogicalLines)
import Phase.State (Env, EvalError (..), envCurrentUser, envStorage, killRef, orderRef, readRef, writeRef)
import Phase.Stmt (parseLogicalLine)

-- | Subscript under an item recording which role authored its CURRENT
-- version, alongside @"VERSION"@/@"KIND"@/@"PARAMS"@. Checked by
-- 'defineItem' itself (so 'revertItem', which calls it, inherits the
-- same protection) and separately by 'Phase.Auth.checkCatalogAccess'
-- for a RAW SET/KILL/MERGE that bypasses this module entirely.
authorRoleSubscript :: Text
authorRoleSubscript = "AUTHOR_ROLE"

-- | Global array name used for the system catalog.
catalogGlobalName :: Text
catalogGlobalName = "_CATALOG"

-- | The kind of catalog item.
data ItemKind
  = KindFunction
  | KindConstant
  | KindRoutine
  deriving (Eq, Show)

kindToText :: ItemKind -> Text
kindToText KindFunction = "FUNCTION"
kindToText KindConstant = "CONSTANT"
kindToText KindRoutine  = "ROUTINE"

textToKind :: Text -> ItemKind
textToKind "CONSTANT" = KindConstant
textToKind "ROUTINE"  = KindRoutine
textToKind _          = KindFunction

-- | Define or update an item in ^_CATALOG.
-- If the item already exists, its previous content and metadata are archived into
-- ^_CATALOG(namespace, name, "HISTORY", version) and its version counter is incremented.
--
-- Returns 'Either' now that 'Phase.State.writeRef'/'killRef' do (a
-- permission denial partway through this chain, e.g. the acting
-- user's role doesn't permit writing at all, must stop the whole
-- definition, not silently continue with some fields updated and
-- others not). Ordinary routine/constant definitions are NOT
-- admin-gated by this module itself, only @^_USERS@/@^_CONFIG@ are
-- hardcoded admin-only (see 'Phase.Auth'), so any user with plain
-- write access can define a NEW routine, or edit one no admin has
-- ever touched. What IS gated: redefining an item whose CURRENT
-- version was itself authored by an admin (see 'authorRoleSubscript')
-- requires the acting user to be an admin too. A separate, coarser
-- version of this same rule ('Phase.Auth.checkCatalogAccess') covers a
-- raw SET/KILL/MERGE that reaches @^_CATALOG@ directly, bypassing this
-- function entirely.
defineItem :: Text -> Text -> ItemKind -> Maybe [Text] -> Text -> Env -> Either EvalError Env
defineItem ns name kind mParams content env = do
  -- 1. Read existing version number (0 if first time), and who is
  -- allowed to replace it.
  let curVer = case readRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "VERSION"] env of
                 Right (PValue v) -> case reads (T.unpack v) of
                                       ((n, "") : _) -> (n :: Int)
                                       _             -> 0
                 Left _ -> 0
      actingRole = lookupUserRole (envCurrentUser env) (envStorage env)
      isAdminOwned = case readRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue authorRoleSubscript] env of
                       Right (PValue r) -> roleFromText r == RAdmin
                       Left _           -> False
  if curVer > 0 && isAdminOwned && actingRole < RAdmin
    then Left (PermissionDenied (envCurrentUser env)
                 ("^_CATALOG(" <> ns <> "," <> name <> ") was defined by an admin and can only be modified by an admin"))
    else defineItemUnchecked ns name kind mParams content actingRole curVer env

-- | The actual write, once 'defineItem' has decided it is allowed.
defineItemUnchecked :: Text -> Text -> ItemKind -> Maybe [Text] -> Text -> Role -> Int -> Env -> Either EvalError Env
defineItemUnchecked ns name kind mParams content actingRole curVer env = do
  -- 2. If curVer > 0, archive existing head into HISTORY
  envWithHistory <-
    if curVer > 0
      then do
        let oldContent = case readRef Global catalogGlobalName [mkValue ns, mkValue name] env of
                           Right val -> val
                           Left _    -> mkValue ""
            oldKind = case readRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "KIND"] env of
                        Right (PValue k) -> k
                        Left _           -> "FUNCTION"
            oldParams = case readRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "PARAMS"] env of
                          Right val -> Just val
                          Left _    -> Nothing
            verKey = mkValue (T.pack (show curVer))
        e1 <- writeRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "HISTORY", verKey] oldContent env
        e2 <- writeRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "HISTORY", verKey, mkValue "KIND"] (mkValue oldKind) e1
        case oldParams of
          Just p  -> writeRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "HISTORY", verKey, mkValue "PARAMS"] p e2
          Nothing -> pure e2
      else pure env

  -- 3. Write new version and metadata
  let newVer = curVer + 1
      newVerVal = mkValue (T.pack (show newVer))
  eNew1 <- writeRef Global catalogGlobalName [mkValue ns, mkValue name] (mkValue content) envWithHistory
  eNew2 <- writeRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "VERSION"] newVerVal eNew1
  eNew3 <- writeRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "KIND"] (mkValue (kindToText kind)) eNew2
  eNew4 <- case mParams of
             Just params -> writeRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "PARAMS"] (mkValue (T.intercalate "," params)) eNew3
             Nothing     -> killRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "PARAMS"] eNew3
  -- 4. Record who authored this version, so a future 'defineItem' can
  -- tell whether it may be replaced by a non-admin.
  writeRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue authorRoleSubscript] (mkValue (roleToText actingRole)) eNew4

-- | Lookup an item by name, trying the requested namespace first and falling back to "_GLOBAL".
lookupItem :: Text -> Text -> Env -> Maybe (ItemKind, [Text], Text)
lookupItem ns name env =
  case lookupExact ns name env of
    Just item -> Just item
    Nothing ->
      if ns /= "_GLOBAL"
        then lookupExact "_GLOBAL" name env
        else Nothing
  where
    lookupExact nSpace n env' =
      case readRef Global catalogGlobalName [mkValue nSpace, mkValue n] env' of
        Left _ -> Nothing
        Right (PValue content) ->
          let kind = case readRef Global catalogGlobalName [mkValue nSpace, mkValue n, mkValue "KIND"] env' of
                       Right (PValue k) -> textToKind k
                       Left _           -> KindFunction
              params = case readRef Global catalogGlobalName [mkValue nSpace, mkValue n, mkValue "PARAMS"] env' of
                         Right (PValue p) -> if T.null p then [] else T.splitOn "," p
                         Left _           -> []
          in Just (kind, params, content)

-- | Retrieve all previous versions from HISTORY in collation order.
getItemHistory :: Text -> Text -> Env -> [(Int, Text)]
getItemHistory ns name env =
  let getVersions curSub =
        let nextSub = orderRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "HISTORY"] (mkValue curSub) env
            nextSubText = unPValue nextSub
        in if T.null nextSubText
             then []
             else case reads (T.unpack nextSubText) of
                    ((vNum, "") : _) ->
                      let content = case readRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "HISTORY", nextSub] env of
                                      Right (PValue c) -> c
                                      Left _           -> ""
                      in (vNum, content) : getVersions nextSubText
                    _ -> getVersions nextSubText
  in getVersions ""

-- | Revert an item to a historical version, committing it as a new version.
revertItem :: Text -> Text -> Int -> Env -> Either EvalError Env
revertItem ns name targetVer env =
  let verKey = mkValue (T.pack (show targetVer))
  in case readRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "HISTORY", verKey] env of
       Left _ -> Left (CustomError ("Version " <> T.pack (show targetVer) <> " not found in history for " <> ns <> "." <> name))
       Right (PValue histContent) ->
         let histKind = case readRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "HISTORY", verKey, mkValue "KIND"] env of
                          Right (PValue k) -> textToKind k
                          Left _           -> KindFunction
             histParams = case readRef Global catalogGlobalName [mkValue ns, mkValue name, mkValue "HISTORY", verKey, mkValue "PARAMS"] env of
                            Right (PValue p) -> Just (T.splitOn "," p)
                            Left _           -> Nothing
         in defineItem ns name histKind histParams histContent env

-- | List all namespaces present in ^_CATALOG.
listNamespaces :: Env -> [Text]
listNamespaces env =
  let getNS cur =
        let next = orderRef Global catalogGlobalName [] (mkValue cur) env
            nextText = unPValue next
        in if T.null nextText
             then []
             else nextText : getNS nextText
  in getNS ""

-- | List all items (name, kind, current version) under a namespace.
listItems :: Text -> Env -> [(Text, ItemKind, Int)]
listItems ns env =
  let getItems cur =
        let next = orderRef Global catalogGlobalName [mkValue ns] (mkValue cur) env
            nextName = unPValue next
        in if T.null nextName
             then []
             else
               let kind = case readRef Global catalogGlobalName [mkValue ns, next, mkValue "KIND"] env of
                            Right (PValue k) -> textToKind k
                            Left _           -> KindFunction
                   ver = case readRef Global catalogGlobalName [mkValue ns, next, mkValue "VERSION"] env of
                           Right (PValue v) -> case reads (T.unpack v) of
                                                 ((n, "") : _) -> n
                                                 _             -> 1
                           Left _           -> 1
               in (nextName, kind, ver) : getItems nextName
  in getItems ""

-- | Parse a tag signature like "DOUBLE(X,Y)" or "FOO" into (name, mParams).
parseSignature :: Text -> (Text, Maybe [Text])
parseSignature sig =
  let clean = T.strip sig
  in case T.break (== '(') clean of
       (name, "") -> (T.strip name, Nothing)
       (name, rest) ->
         let afterParen = T.drop 1 rest
             paramText = T.takeWhile (/= ')') afterParen
             params = map T.strip (T.splitOn "," paramText)
         in (T.strip name, Just (filter (not . T.null) params))

-- | Ingest a multi-line routine source text and register all column-0
-- tags into ^_CATALOG, but only after each tag's own body has been
-- checked against the REAL Phase statement grammar
-- ('Phase.Stmt.parseLogicalLine'), exactly the same check
-- 'Phase.Eval.invokeTarget' already re-runs on every single call.
--
-- Before this fix, a routine's body text went straight into
-- 'defineItem' unchecked, so genuinely unparseable content (not
-- "recognized but unimplemented", which correctly degrades to
-- 'SUnsupported' per §9, actual GARBAGE, gibberish that isn't valid
-- Phase syntax at all) would be accepted and stored, only to blow up
-- with a confusing runtime error the first time the routine was
-- CALLED, potentially long after it was defined. the team's own
-- reproduction: defining @ITWILLFAIL(X)@ with a body of plain English
-- sentences registered successfully and only failed at
-- @DO ITWILLFAIL(-1)@, with a @<NOTIMPL>@ that doesn't even describe
-- the real problem (it's not "not implemented", it's "not valid Phase
-- at all").
--
-- Returns 'Left' with a 'SyntaxError' naming the failing tag and the
-- underlying parse error, WITHOUT defining ANY of the tags found in
-- 'src', a routine source with several tags is checked and stored
-- as one atomic unit, not tag-by-tag with partial success, so a typo
-- in the third tag can't leave the first two silently registered
-- while the definition attempt as a whole appears to have failed.
registerRoutineTags :: Text -> Text -> Env -> Either EvalError Env
registerRoutineTags ns src env =
  let llines = buildLogicalLines src
      groups = groupByTag llines
  in foldl checkAndDefine (Right env) groups
  where
    checkAndDefine (Left err) _ = Left err
    checkAndDefine (Right e) (tagSig, bodyLines) =
      let (name, mParams) = parseSignature tagSig
          bodyText = T.unlines (map formatLine bodyLines)
      in if T.null (T.strip bodyText)
           then Left (SyntaxError
                  ("Empty body for " <> ns <> "." <> name <>
                   ": a function definition requires at least one statement"))
           else case mapM parseLogicalLine (buildLogicalLines bodyText) of
                  Left parseErr ->
                    Left (SyntaxError ("Error compiling " <> ns <> "." <> name <> ": " <> T.pack (show parseErr)))
                  Right _validAst ->
                    defineItem ns name KindFunction mParams bodyText e

    groupByTag [] = []
    groupByTag (LogicalLine _ (Just tag) txt : rest) =
      let (body, remaining) = span (\ll -> case llTag ll of Nothing -> True; Just _ -> False) rest
          firstLine = if T.null txt then [] else [LogicalLine 0 Nothing txt]
      in (tag, firstLine ++ body) : groupByTag remaining
    groupByTag (LogicalLine _ Nothing _ : rest) =
      groupByTag rest

    formatLine (LogicalLine depth _ txt) =
      if depth > 0
        then T.replicate depth "." <> " " <> txt
        else " " <> txt
