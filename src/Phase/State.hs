{-# LANGUAGE OverloadedStrings #-}

-- | Runtime environment and execution state for Phase.
--
-- This module manages process-global local variables (with MUMPS node duality
-- and collation-ordered subscripts), in-memory globals, call frames, 'NEW' variable
-- restoration scopes, '$TEST', and execution errors.
module Phase.State
  ( -- * Error Handling
    EvalError (..)
  , Condition (..)
  , conditionMessage
  , retryLimit
  , formatError

    -- * Call Frames & State
  , Frame (..)
  , Env (..)
  , initEnv
  , initEnvWithStorage

    -- * Variable & Storage Access
  , readRef
  , writeRef
  , killRef
  , killAllLocals
  , dataRef
  , orderRef
  , childCountRef
  , mergeRef
  , resolveRelativeTx
  , effectivePin
  , enterPinScope
  , readAtTxRef
  , dataAtTxRef
  , orderAtTxRef

    -- * Scoping (NEW) & Call Frames
  , newVar
  , newAllVars
  , pushFrame
  , popFrame
  , resolveLocalName
  , aliasVar

    -- * Output
  , appendOutput
  , getOutputText
  , clearOutput
  , OutputResult (..)
  ) where

import Control.Monad (mplus)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)

import Phase.AST (Line, PValue (..), VarKind (..), mkValue)
import Phase.Auth
  ( checkCatalogAccess
  , checkReadAccess
  , checkUsersKillSafety
  , checkUsersSetSafety
  , checkWriteAccess
  , lookupUserRole
  )
import Phase.Storage.Engine
  ( StorageEngine (..)
  , commitKill
  , commitTransaction
  , commitSet
  , dataAtTx
  , orderAtTx
  , dataLive
  , initStorageEngine
  , orderLive
  , readAtTx
  , readLive
  )
import Phase.Storage.Lock (LockTable, emptyLockTable)
import Phase.Storage.Transaction (TxHandle (..), newTxHandle, stageKill, stageSet)
import Phase.Storage.Types (Revision (..), TxId, formatStorageError)
import Phase.Tree (Node (..), childCountNode, dataNode, dumpTree, emptyNode, findSubNode, getNode, killNode, mergeNode, orderNode, setNode, toSubKey)

-- --------------------------------------------------------------------
-- Error Handling
-- --------------------------------------------------------------------

-- | Phase execution errors (handbook Appendices).
data EvalError
  = UndefinedVariable Text [PValue]
  | UndefinedFunction Text
  | UndefinedLabel Text (Maybe Text)
  | UndefinedRoutine Text
  | DivideByZero
  | ConflictError Text
  | SetOnReadOnly
  | PermissionDenied Text Text
  -- ^ The acting username and the global name they were denied
  -- access to (or, for a role-level denial rather than a per-global
  -- one, the action attempted, e.g. @"write"@, @"compact"@). Follows
  -- the real M/MUMPS @<PROTECT>@ error name, the handbook does not
  -- itself specify a permission-error name (this is a gap in the
  -- spec being filled here, same as @>=@/@<=@ and case-insensitive
  -- keywords were earlier), so this matches the language's own
  -- heritage rather than inventing something unrelated.
  | NotImplemented Text Text
  -- ^ The offending token/word (e.g. @"TRANSACTION"@, @"EXPECTING"@)
  -- and the full original statement text it came from, for context.
  -- See 'Phase.AST.SUnsupported', which carries exactly these same
  -- two pieces of information from parse time through to here.
  | SyntaxError Text
  | CustomError Text
  | NullSubscript Text [PValue]
  -- ^ A SET/MERGE targeted a subscript list containing an empty-string
  -- component. The null string is $ORDER's own exhaustion sentinel,
  -- so real M never accepts it as a subscript VALUE; this reuses the
  -- handbook's own @<SUBSCRIPT>@ error name (already used for calling
  -- $ORDER against a bare, unsubscripted local) rather than inventing
  -- a second, unrelated one.
  | NakedReference
  -- ^ A naked reference (@^(subs)@) was used before any NAMED global
  -- had been referenced yet in this session, so there is nothing for
  -- it to mean. Real M's own error name for exactly this.
  | BindScope Text
  -- ^ A nested @DO WITH@/@CALL WITH@ @@BIND(...)@ list named a global
  -- (qualified name) that the enclosing block's own bind list did not
  -- bind. A nested bind list must be a subset of the one around it; the
  -- only block that may bind any global is one with no enclosing bind
  -- list at all (docs/tx-pinning-design.md, "Explicit global bindings").
  | Signaled !Condition
  -- ^ A condition raised by @SIGNAL@ (or by a built-in that signals
  -- while a handler is active, Part III §1.1). Travels as an ordinary
  -- error until a @HANDLE@/@CATCH@ clause claims it; with none, it
  -- halts like any other error (Part III §1.2).
  | OutputLimitExceeded
  -- ^ 'appendOutput' was called when the total buffered output already
  -- equalled or exceeded 'envMaxOutputBytes'.  Raised instead of the
  -- @OUTPUT LIMIT@ error code described in the HTTP spec (§4.1); the
  -- server maps this constructor to that code.
  deriving (Eq, Show)

-- | A raised condition (Part III §1): its name, upper-cased and without
-- the angle brackets (@"DIVIDE-BY-ZERO"@, @"CONFLICT"@, a user's
-- @"OOPS"@), and its fields in the order they were supplied.
data Condition = Condition
  { condName   :: !Text
  , condFields :: ![(Text, PValue)]
  } deriving (Eq, Show)

-- | The automatic @message@ field every condition has: its name and
-- fields as one human-readable line, @<NAME> f1=v1, f2=v2@. A field
-- literally named @message@ that the signaller supplied wins over this
-- (it is the more specific text), see 'Phase.Eval''s handler binding.
conditionMessage :: Condition -> Text
conditionMessage c =
  "<" <> condName c <> ">"
    <> (if null (condFields c)
          then ""
          else " " <> T.intercalate ", " [k <> "=" <> unPValue v | (k, v) <- condFields c])

-- | Format an error according to Phase's error convention (<ERROR_NAME>).
formatError :: EvalError -> Text
formatError (UndefinedVariable name subs) =
  let subTxt = if null subs then "" else "(" <> T.intercalate "," (map unPValue subs) <> ")"
  in "<UNDEFINED VARIABLE> " <> name <> subTxt
formatError (UndefinedFunction name) =
  "<UNDEFINED FUNCTION> " <> name
formatError (UndefinedLabel tag mRoutine) =
  let rTxt = case mRoutine of
               Just r  -> "^" <> r
               Nothing -> ""
  in "<UNDEFINED LABEL> " <> tag <> rTxt
formatError (UndefinedRoutine routine) =
  "<UNDEFINED ROUTINE> " <> routine
formatError DivideByZero =
  "<DIVIDE BY ZERO>"
formatError (ConflictError detail) =
  "<CONFLICT> " <> detail
formatError SetOnReadOnly =
  "<SET ON READONLY> Cannot write to a time-traveled reference"
formatError (PermissionDenied user detail) =
  "<PROTECT> " <> user <> ": " <> detail
formatError (NotImplemented token fullText) =
  if token == fullText
    then "<NOTIMPL> " <> token
    else "<NOTIMPL> " <> token <> " (in: " <> fullText <> ")"
formatError (SyntaxError msg) =
  "<SYNTAX> " <> msg
formatError (CustomError msg) =
  "<ERROR> " <> msg
formatError (NullSubscript name subs) =
  let subTxt = T.intercalate "," (map unPValue subs)
  in "<SUBSCRIPT> " <> name <> "(" <> subTxt <> ") -- the null string is not a valid subscript"
formatError NakedReference =
  "<NAKED> no global reference has been established yet"
formatError (BindScope name) =
  "<BIND SCOPE> ^" <> name <> " is not bound by the enclosing DO WITH / CALL WITH"
formatError (Signaled c) =
  case lookup "message" (condFields c) of
    Just m  -> "<" <> condName c <> "> " <> unPValue m
    Nothing -> conditionMessage c
formatError OutputLimitExceeded =
  "<OUTPUT LIMIT> maximum output size exceeded"

-- --------------------------------------------------------------------
-- Call Frames
-- --------------------------------------------------------------------

-- | A call frame on the execution stack.
data Frame = Frame
  { frameTag       :: !(Maybe Text)
  , frameRoutine   :: !(Maybe Text)
  , frameReturnPC  :: !(Int, Int) -- ^ (Line index, Statement index) to resume after call
  , frameRestores  :: ![(Text, Maybe Node)] -- ^ Variables saved by 'NEW' to restore on frame exit
  , frameAliases   :: ![(Text, Maybe Text)] -- ^ Aliases saved to restore on frame exit
  } deriving (Eq, Show)

-- --------------------------------------------------------------------
-- Environment
-- --------------------------------------------------------------------

-- | The complete Phase runtime environment for an interpreter session.
data Env = Env
  { envLocals           :: !(Map Text Node)
  , envAliases          :: !(Map Text Text) -- ^ Local variable aliases (&ref, Part II §2)
  , envGlobals          :: !(Map Text Node)
  , envStorage          :: !StorageEngine
  , envLocks            :: !LockTable
  , envActiveTx         :: !(Maybe TxHandle)
  , envFrames           :: ![Frame]
  , envTest             :: !Bool       -- ^ Current value of $TEST (last IF or postcondition)
  , envOutput           :: ![Text]     -- ^ Buffer of output strings from WRITE
  , envMaxOutputBytes   :: !(Maybe Int)
    -- ^ Optional cap on total output. When set, 'appendOutput' raises
    -- 'OutputLimitExceeded' instead of growing the buffer past this many
    -- UTF-8 bytes. 'Nothing' = unlimited (REPL default). The HTTP server
    -- sets this per request from the @maxOutputBytes@ config value.
  , envFileIOEnabled    :: !Bool
    -- ^ Whether Phase statements that perform host-filesystem I/O
    -- (BLOBSAVE, $BLOBWRITE with a path, etc.) are permitted. 'True'
    -- for the REPL; 'False' for API sessions (§6.5 of the HTTP spec).
    -- Not yet enforced by any built-in (the blob store does not exist),
    -- but the field is here so the server can set it 'False' and future
    -- blob work does not need to touch 'Env' at all.
  , envRoutines         :: !(Map (Maybe Text) [Line]) -- ^ Loaded routines (Nothing = current script)
  , envCurrentRoutine   :: !(Maybe Text)
  , envCurrentNamespace :: !Text       -- ^ Active namespace (defaults to "_GLOBAL", Part V)
  , envPC               :: !(Int, Int) -- ^ Current (Line index, Statement index)
  , envJobId            :: !Text       -- ^ $JOB string
  , envTxId             :: !Text       -- ^ $TX string
  , envNow              :: !UTCTime    -- ^ Real wall-clock time, captured once in IO by the
                                        -- caller (e.g. the REPL driver) before running a
                                        -- top-level statement or routine, and used as the
                                        -- commit timestamp for any auto-committed global
                                        -- write ('writeRef'/'killRef' below). 'Eval' itself
                                        -- is a pure state monad with no 'IO' access, so it
                                        -- cannot call 'getCurrentTime', this field is how a
                                        -- real timestamp reaches pure code instead of a
                                        -- hardcoded placeholder.
  , envCurrentUser      :: !Text       -- ^ The logged-in username for this session (the team's
                                        -- user/role system). Set once, at REPL login, and
                                        -- threaded through exactly like 'envNow', every
                                        -- read/write permission check ('Phase.Auth') and
                                        -- every WAL commit's attribution consults this
                                        -- rather than some separate identity mechanism.
                                        -- Defaults to empty in 'initEnv', which has no
                                        -- login flow of its own (tests / non-interactive
                                        -- use), see 'initEnv''s own docs for the same
                                        -- caveat 'envNow' already carries.
  , envPinnedTx         :: !(Maybe TxId) -- ^ Set only inside a @DO WITH @TX(...)@ block
                                        -- (docs/tx-pinning-design.md Part 2): every bare
                                        -- GLOBAL read is answered as of this transaction
                                        -- instead of live. Process-wide like '$TEST' and
                                        -- 'envLastGlobalRef', NOT frame-scoped, so a pin
                                        -- carries through @DO@/@CALL@ made inside the
                                        -- block. 'Nothing' everywhere else. An error
                                        -- inside the block discards the whole 'Env' (see
                                        -- 'Phase.Eval.Eval'), so a pin cannot leak out of
                                        -- a failed block.
  , envPinOverrides     :: !(Map Text (Maybe TxId))
                                        -- ^ Per-global pins installed by a @DO WITH@ /
                                        -- @CALL WITH@ block that carries an @@BIND(...)@
                                        -- list, keyed by qualified global name. A global
                                        -- listed here is read as of its own entry
                                        -- ('Nothing' = explicitly LIVE, which beats a
                                        -- non-live 'envPinnedTx'); every other global
                                        -- falls back to 'envPinnedTx'. Empty outside such
                                        -- a block. See 'effectivePin' / 'enterPinScope'.
  , envBindScope        :: !(Maybe (Set Text))
                                        -- ^ The bind list of the innermost enclosing
                                        -- block that has one: the only globals a nested
                                        -- @@BIND(...)@ may name, and the globals a nested
                                        -- block WITHOUT a bind list re-pins. 'Nothing' =
                                        -- no enclosing bind list (so anything may be
                                        -- bound, and a bind-less block re-pins every
                                        -- global). Kept apart from the overrides so an
                                        -- empty list (@@BIND()@) stays distinct from none.
  , envLastGlobalRef    :: !(Maybe Text) -- ^ The fully-qualified name (namespace already
                                        -- folded in) of the last NAMED global referenced,
                                        -- read, written, killed, locked, or passed to an
                                        -- intrinsic like $DATA/$ORDER. Powers a naked
                                        -- reference, @^(subs)@: same global, entirely new
                                        -- subscript list. 'Nothing' until the first such
                                        -- reference; a naked reference itself does not
                                        -- change it. See 'Phase.Eval.resolveNakedRef'.
  , envHandlerDepth     :: !Int
    -- ^ How many 'Phase.Eval.runHandledBlock' bodies are active in the
    -- CURRENT call stack (dynamic, not lexical: a routine called from
    -- inside a handled block sees it). Above zero means "a handler is
    -- present" for the built-ins whose signalling is conditional on one
    -- (Part III §1.1).
  , envCaught           :: !(Maybe Condition)
    -- ^ Set by the per-line guard of a handled block when it catches a
    -- signal, and read-and-cleared by that block's runner right after
    -- the body stops. Never set outside that handshake.
  , envRetry            :: !(Maybe [(Text, PValue)])
    -- ^ A @RETRY@ that has been requested and is unwinding to its
    -- block: the already-evaluated @WITH@ seeds. Set by @RETRY@,
    -- noticed by every block level on the way out, cleared by the
    -- runner of the block being re-entered.
  , envInHandle         :: !Bool
    -- ^ Is the statement being run lexically inside a @HANDLE@ clause
    -- body? @RETRY@ anywhere else is a @<SYNTAX>@ error.
  , envRetryCount       :: !Int
    -- ^ How many times the innermost handled block has been
    -- re-entered so far; what @RETRY n TIMES@ compares against.
  , envConditionDecls   :: !(Map Text [Text])
    -- ^ The @CONDITION <NAME>(fields)@ declarations of the routine
    -- being run (collected up front, so a declaration may appear after
    -- its first @SIGNAL@ in the source, as a compile-time registry
    -- would allow). Used only to reject an undeclared @SIGNAL@ field.
  } deriving (Eq, Show)

-- | Create an initial, empty execution environment.
--
-- NOTE: this sets 'envNow' to the Unix epoch, since 'initEnv' is a
-- pure value with no way to call 'getCurrentTime' itself. Any caller
-- that will actually commit auto-committed global writes (i.e. any
-- real driver, the REPL, a future script runner) MUST replace
-- 'envNow' with a real captured time before doing so; see
-- 'Phase.State' users like @app/Main.hs@, which is expected to set
-- @env { envNow = now }@ once per top-level statement/block via
-- 'Data.Time.getCurrentTime' in 'IO'. Tests that don't care about
-- real wall-clock time (most of them) can use this default as-is.
--
-- Similarly sets 'envCurrentUser' to a fixed, unprivileged-by-default
-- placeholder ("_nobody") rather than an empty string, an empty
-- username is exactly the kind of value that could accidentally
-- collide with something meaningful elsewhere (e.g. a lookup that
-- silently treats "no user set" as "the default user"), whereas a
-- name that can never be a REAL registered account name (usernames
-- come from @^_USERS@'s own subscripts, which a real login always
-- populates with something a person typed) fails safe: every
-- permission check simply denies it, exactly as it should for a
-- session nobody has actually authenticated. A real interactive
-- driver (the REPL) MUST replace this with the actual logged-in
-- username after a successful login, the same way it replaces
-- 'envNow'.
initEnv :: Env
initEnv = initEnvWithStorage initStorageEngine

-- | Create an execution environment initialized with a specific storage engine.
initEnvWithStorage :: StorageEngine -> Env
initEnvWithStorage storage = Env
  { envLocals           = Map.empty
  , envAliases          = Map.empty
  , envGlobals          = seLive storage
  , envStorage          = storage
  , envLocks            = emptyLockTable
  , envActiveTx         = Nothing
  , envFrames           = []
  , envTest             = True
  , envOutput           = []
  , envMaxOutputBytes   = Nothing
  , envFileIOEnabled    = True
  , envRoutines         = Map.empty
  , envCurrentRoutine   = Nothing
  , envCurrentNamespace = "_GLOBAL"
  , envPC               = (0, 0)
  , envJobId            = "1"
  , envTxId             = T.pack (show (seLastTx storage))
  , envNow              = posixSecondsToUTCTime 0
  , envCurrentUser      = "_nobody"
  , envPinnedTx         = Nothing
  , envPinOverrides     = Map.empty
  , envBindScope        = Nothing
  , envLastGlobalRef    = Nothing
  , envHandlerDepth     = 0
  , envCaught           = Nothing
  , envRetry            = Nothing
  , envInHandle         = False
  , envRetryCount       = 0
  , envConditionDecls   = Map.empty
  }

-- | The ceiling on how many times one handled block may be re-entered by
-- RETRY, a guard against a handler that retries forever on an error that
-- never goes away. Read from @^_CONFIG("retry","max")@, an ordinary
-- global like the checkpoint thresholds (so it is set with Phase's own
-- SET, by an administrator); 1000 when unset or not a positive integer.
retryLimit :: Env -> Int
retryLimit env =
  case readLive "_CONFIG" [mkValue "retry", mkValue "max"] (envStorage env) of
    Right v | [(n, "")] <- reads (T.unpack (unPValue v)), n > 0 -> n
    _                                                           -> 1000

-- --------------------------------------------------------------------
-- Variable & Storage Access
-- --------------------------------------------------------------------

-- | Resolve a local variable name through any active reference parameter aliases.
resolveLocalName :: Text -> Env -> Text
resolveLocalName name env =
  case Map.lookup name (envAliases env) of
    Nothing     -> name
    Just target -> resolveLocalName target env

-- | Read a value from a local or global variable at a given subscript path.
-- | Read a local or global variable. Global reads go through
-- 'Phase.Auth.checkReadAccess' first (role level + per-global
-- denylist + the hardcoded @^_USERS@/@^_CONFIG@ admin-only rule),
-- local reads need no such check, since locals are per-session and
-- never shared/protected data.
readRef :: VarKind -> Text -> [PValue] -> Env -> Either EvalError PValue
readRef kind rawName subs env =
  case kind of
    Local ->
      let name = resolveLocalName rawName env
      in case Map.lookup name (envLocals env) of
        Nothing   -> Left (UndefinedVariable name subs)
        Just node ->
          case getNode subs node of
            Nothing  -> Left (UndefinedVariable name subs)
            Just val -> Right val
    Global ->
      case checkReadAccess (envCurrentUser env) rawName (envStorage env) of
        Just reason -> Left (PermissionDenied (envCurrentUser env) reason)
        Nothing ->
          case readLive rawName subs (envStorage env) of
            Right val -> Right val
            Left _    -> Left (UndefinedVariable ("^" <> rawName) subs)

-- | Read a global variable as of a past transaction ID (Part VII Time Travel).
-- Now 'IO'-based: 'Phase.Storage.Engine.readAtTx' may need to
-- re-read the @.db@ file for a deep time-travel target further back
-- than what's currently resident in memory, see its own docs.
-- | The transaction a bare read of this (qualified) global is pinned
-- to right now: its own entry in 'envPinOverrides' if a @@BIND(...)@
-- list gave it one, otherwise the block-wide 'envPinnedTx'. 'Nothing'
-- means the read is live. Only meaningful for globals, locals have no
-- history to pin to, and callers never ask about them.
effectivePin :: Text -> Env -> Maybe TxId
effectivePin name env = Map.findWithDefault (envPinnedTx env) name (envPinOverrides env)

-- | Enter a @DO WITH@ / @CALL WITH@ pin scope: the pure state change,
-- given the transaction the scope pins to ('Nothing' = @@TX(LAST)@,
-- release) and its bind list ('Nothing' = no @@BIND@ clause). The caller
-- saves ('envPinnedTx', 'envPinOverrides', 'envBindScope') beforehand
-- and restores them afterwards.
--
-- With a bind list, only the listed globals take the new pin; every
-- other global keeps whatever pin the enclosing scope gave it (or stays
-- live), so an inner block never silently releases an outer pin, only
-- an explicit @@TX(LAST)@ does. The list must be a subset of the
-- enclosing bind list ('BindScope' otherwise); with no enclosing list
-- (no enclosing bind at all) any global may be named.
--
-- Without a bind list, the scope re-pins the globals the enclosing
-- scope had bound: the enclosing bind list if there is one, otherwise
-- every global (a block-wide pin).
enterPinScope :: Maybe TxId -> Maybe (Set Text) -> Env -> Either EvalError Env
enterPinScope newTx (Just names) env =
  case envBindScope env of
    Just scope
      | (bad : _) <- Set.toList (names `Set.difference` scope) -> Left (BindScope bad)
    _ -> Right env
           { envPinOverrides = Set.foldr (\n -> Map.insert n newTx) (envPinOverrides env) names
           , envBindScope    = Just names
           }
enterPinScope newTx Nothing env =
  case envBindScope env of
    Nothing    -> Right env { envPinnedTx = newTx }
    Just scope -> Right env
                    { envPinOverrides = Set.foldr (\n -> Map.insert n newTx) (envPinOverrides env) scope }

-- | Resolve a time-travel bracket's RELATIVE form (Part 1 of relative
-- revision addressing, @docs/tx-pinning-design.md@, @^[-N]@/@LAST@/
-- @LAST-N@, all of which 'Phase.Eval.resolveTimeTravelExpr' reduces to
-- "N revisions back") to a concrete, ABSOLUTE transaction number, so
-- every ordinary absolute-tx reader ('readAtTxRef', 'dataAtTxRef',
-- below) can be reused completely unchanged.
--
-- @N = 0@ (@LAST@) is this path's own most recent revision, NOT
-- necessarily 'StorageEngine''s overall latest transaction, since this
-- path may not be the most recently written thing. "N revisions back"
-- past this path's recorded history (including when it has none at
-- all) resolves to transaction 0, "before this path existed", which
-- 'readAtTxRef'/'dataAtTxRef' already treat as undefined/absent, so
-- callers need no separate "out of range" case of their own. A
-- tombstone revision (a KILL, see 'Phase.Storage.Engine.
-- killTombstonePaths') counts as one step back like any other
-- revision, so @^[-1]@ right after a KILL correctly resolves to the
-- transaction that removed the path, not silently past it.
--
-- Always a GLOBAL concern: the only syntax that reaches this
-- ('Phase.Parser.pTimeTravelExpr', inside a @^[...]@ bracket) is
-- syntactically unreachable for a local reference in the first place
-- ('Phase.Parser.pLocal' has no time-travel bracket at all).
resolveRelativeTx :: Bool -> Text -> [PValue] -> Int -> Env -> TxId
resolveRelativeTx fromLive targetName subs n env =
  let allRevs = Map.findWithDefault [] (targetName, map toSubKey subs) (seHistory (envStorage env))
      -- Inside a pinned block the IMPLICIT baseline is the revision in
      -- effect at the pin, so a plain @^[-N]@ counts back from THAT; the
      -- explicit @LAST@ spelling ('fromLive' = True) always counts from
      -- the true live latest, which is what makes @LAST@ the escape
      -- hatch back out of a pin (docs/tx-pinning-design.md Part 2).
      revs = case (fromLive, effectivePin targetName env) of
               (False, Just pin) -> dropWhile (\r -> revTx r > pin) allRevs
               _                 -> allRevs
  in case drop (max 0 n) revs of
       (Revision tx _ _ : _) -> tx
       _                     -> 0

-- Subject to the same read-permission check as 'readRef', time
-- travel does not bypass access control.
readAtTxRef :: TxId -> Text -> [PValue] -> Env -> IO (Either EvalError PValue)
readAtTxRef tx name subs env =
  case checkReadAccess (envCurrentUser env) name (envStorage env) of
    Just reason -> pure (Left (PermissionDenied (envCurrentUser env) reason))
    Nothing -> do
      result <- readAtTx tx name subs (envStorage env)
      pure $ case result of
        Right val -> Right val
        Left _    -> Left (UndefinedVariable ("^[" <> T.pack (show tx) <> "]" <> name) subs)

-- | @$ORDER@ as of a past transaction ID (see
-- 'Phase.Storage.Engine.orderAtTx'). A global the user may not read
-- yields the empty string, same as the live 'orderRef' does for a
-- denied read, time travel does not bypass access control.
orderAtTxRef :: TxId -> Text -> [PValue] -> PValue -> Env -> IO PValue
orderAtTxRef tx name parentSubs curSub env =
  case checkReadAccess (envCurrentUser env) name (envStorage env) of
    Just _  -> pure (mkValue "")
    Nothing -> orderAtTx tx name parentSubs curSub (envStorage env)

-- | 4-state $DATA check as of a past transaction ID. 'IO'-based for
-- the same reason as 'readAtTxRef'. A permission denial here reports
-- as "not present" (0) rather than surfacing an error through $DATA's
-- own numeric-only return convention, $DATA's contract is a plain
-- number, not something that fails with an error branch, so a denied
-- user simply sees the global as if it didn't exist.
dataAtTxRef :: TxId -> Text -> [PValue] -> Env -> IO Int
dataAtTxRef tx name subs env =
  case checkReadAccess (envCurrentUser env) name (envStorage env) of
    Just _  -> pure 0
    Nothing -> dataAtTx tx name subs (envStorage env)

-- | Extra, subscript-aware checks for reserved globals that
-- 'Phase.Auth.checkWriteAccess' cannot express on its own (it only
-- sees the global's NAME, never which subscripts are being touched).
-- Runs AFTER the ordinary role/denylist check has already passed.
-- 'Nothing' for the new value means this is a KILL, not a SET.
checkReservedGlobalWrite :: Text -> [PValue] -> Maybe PValue -> Env -> Maybe Text
checkReservedGlobalWrite rawName subs mNewVal env
  | rawName == "_CATALOG" =
      checkCatalogAccess (lookupUserRole (envCurrentUser env) (envStorage env)) subs (envStorage env)
  | rawName == "_USERS" =
      case mNewVal of
        Just newVal -> checkUsersSetSafety subs newVal (envStorage env)
        Nothing     -> checkUsersKillSafety subs (envStorage env)
  | otherwise = Nothing

-- | Write a value to a local or global variable at a given subscript
-- path. Global writes go through 'Phase.Auth.checkWriteAccess' (and,
-- for @^_CATALOG@/@^_USERS@, 'checkReservedGlobalWrite') first;
-- denial returns 'Left' with no change to 'Env' at all. Rejects a
-- null-string subscript anywhere in the path, for either kind.
writeRef :: VarKind -> Text -> [PValue] -> PValue -> Env -> Either EvalError Env
writeRef _ rawName subs _ _
  | any (T.null . unPValue) subs = Left (NullSubscript rawName subs)
writeRef kind rawName subs val env =
  case kind of
    Local ->
      let name = resolveLocalName rawName env
          node = Map.findWithDefault emptyNode name (envLocals env)
          node' = setNode subs val node
      in Right env { envLocals = Map.insert name node' (envLocals env) }
    Global ->
      case checkWriteAccess (envCurrentUser env) rawName (envStorage env)
             `mplus` checkReservedGlobalWrite rawName subs (Just val) env of
        Just reason -> Left (PermissionDenied (envCurrentUser env) reason)
        Nothing ->
          case envActiveTx env of
            Just tx ->
              -- Stage write in active transaction
              let tx' = stageSet rawName subs val Nothing tx
                  node = Map.findWithDefault emptyNode rawName (envGlobals env)
                  node' = setNode subs val node
              in Right env { envActiveTx = Just tx', envGlobals = Map.insert rawName node' (envGlobals env) }
            Nothing ->
              -- Auto-commit single mutation. 'envNow' carries the real
              -- wall-clock time, captured in IO by the caller before this
              -- statement ran (see 'Env''s own doc on the field), 'Eval'
              -- itself has no IO access to call 'getCurrentTime' directly.
              -- 'envCurrentUser' is threaded down to attribute the
              -- resulting commit record to whoever made it.
              case commitSet (envCurrentUser env) rawName subs val Nothing (envNow env) (envStorage env) of
                   Right (newTx, storage') ->
                     Right env
                       { envStorage = storage'
                       , envGlobals = seLive storage'
                       , envTxId    = T.pack (show newTx)
                       }
                   Left _ ->
                     -- Fallback in-memory update
                     let node = Map.findWithDefault emptyNode rawName (envGlobals env)
                         node' = setNode subs val node
                     in Right env { envGlobals = Map.insert rawName node' (envGlobals env) }

-- | Delete a variable or subscripted subtree. Same permission
-- treatment as 'writeRef'.
killRef :: VarKind -> Text -> [PValue] -> Env -> Either EvalError Env
killRef kind rawName subs env =
  case kind of
    Local ->
      let name = resolveLocalName rawName env
      in case Map.lookup name (envLocals env) of
        Nothing   -> Right env
        Just node ->
          case killNode subs node of
            Nothing    -> Right env { envLocals = Map.delete name (envLocals env) }
            Just node' -> Right env { envLocals = Map.insert name node' (envLocals env) }
    Global ->
      case checkWriteAccess (envCurrentUser env) rawName (envStorage env)
             `mplus` checkReservedGlobalWrite rawName subs Nothing env of
        Just reason -> Left (PermissionDenied (envCurrentUser env) reason)
        Nothing ->
          case envActiveTx env of
            Just tx ->
              let tx' = stageKill rawName subs tx
                  node = Map.findWithDefault emptyNode rawName (envGlobals env)
                  node' = killNode subs node
                  globals' = case node' of
                               Nothing -> Map.delete rawName (envGlobals env)
                               Just n  -> Map.insert rawName n (envGlobals env)
              in Right env { envActiveTx = Just tx', envGlobals = globals' }
            Nothing ->
              let (newTx, storage') = commitKill (envCurrentUser env) rawName subs (envNow env) (envStorage env)
              in Right env
                   { envStorage = storage'
                   , envGlobals = seLive storage'
                   , envTxId    = T.pack (show newTx)
                   }

-- | Bare KILL: kills all process-global local variables.
killAllLocals :: Env -> Env
killAllLocals env = env { envLocals = Map.empty, envAliases = Map.empty }

-- | Check the existence state of a node ($DATA, Part I §6).
-- | 4-state $DATA check (Part I §3.1). A permission denial reports as
-- "not present" (0), same treatment as 'dataAtTxRef', $DATA has no
-- error branch of its own to report a denial through.
dataRef :: VarKind -> Text -> [PValue] -> Env -> Int
dataRef kind rawName subs env =
  case kind of
    Local ->
      let name = resolveLocalName rawName env
      in case Map.lookup name (envLocals env) of
        Nothing   -> 0
        Just node -> dataNode subs node
    Global ->
      case checkReadAccess (envCurrentUser env) rawName (envStorage env) of
        Just _  -> 0
        Nothing -> dataLive rawName subs (envStorage env)

-- | Order traversal over a subscript level ($ORDER, Part I §6). A
-- permission denial reports as "no next subscript" (empty string),
-- same reasoning as 'dataRef'.
orderRef :: VarKind -> Text -> [PValue] -> PValue -> Env -> PValue
orderRef kind rawName parentSubs curSub env =
  case kind of
    Local ->
      let name = resolveLocalName rawName env
      in case Map.lookup name (envLocals env) of
        Nothing   -> mkValue ""
        Just node -> case orderNode parentSubs curSub node of
                       Nothing      -> mkValue ""
                       Just nextSub -> nextSub
    Global ->
      case checkReadAccess (envCurrentUser env) rawName (envStorage env) of
        Just _  -> mkValue ""
        Nothing -> orderLive rawName parentSubs curSub (envStorage env)

-- | Count the immediate children of a node ($CHILDCOUNT). Returns 0 when the
-- path does not exist, when the node has no children, or when read access is
-- denied, same "silent 0" contract as 'dataRef' for a permission denial, since
-- $CHILDCOUNT has no error-return branch of its own.
childCountRef :: VarKind -> Text -> [PValue] -> Env -> Int
childCountRef kind rawName subs env =
  case kind of
    Local ->
      let name = resolveLocalName rawName env
      in case Map.lookup name (envLocals env) of
        Nothing   -> 0
        Just node -> maybe 0 id (childCountNode subs node)
    Global ->
      case checkReadAccess (envCurrentUser env) rawName (envStorage env) of
        Just _  -> 0
        Nothing ->
          case Map.lookup rawName (seLive (envStorage env)) of
            Nothing   -> 0
            Just node -> maybe 0 id (childCountNode subs node)

-- | MERGE a source subtree into a destination subtree (Part X §1): every
-- value under the source path is written to the matching path under the
-- destination, and nothing already at the destination is cleared.
--
-- Checks READ access on the source (if global) and WRITE access on the
-- destination (if global), a MERGE is simultaneously "read this" and
-- "write that", so both sides of the team's permission model apply.
--
-- A MERGE into a LOCAL is a plain in-memory tree merge ('mergeNode').
-- A MERGE into a GLOBAL is exactly the sequence of 'writeRef' SETs it
-- stands for, one per merged path, and is treated that way throughout:
--
-- * every merged path goes through the same per-value reserved-global
--   checks a SET does ('checkReservedGlobalWrite', the last-admin
--   safeguard on @^_USERS@, ownership on @^_CATALOG@), ALL of them
--   before anything is written, so a refused leaf means nothing at all
--   was merged;
-- * inside an active transaction the paths are STAGED, like 'writeRef'
--   does, and 'envGlobals' is updated incrementally, committed
--   storage ('envStorage') is not touched, so rollback still means
--   something and a SET already staged against the same global survives;
-- * outside one, ALL the paths commit together through
--   'commitTransaction': ONE 'TxId', one @WalSet@ per path plus one
--   @WalCommit@ in the WAL queue, and a revision per path, so the merge
--   is atomic, durable, and visible to time travel, @^[-N]@ and
--   @EXPECTING@ at a single point in history.
--
-- The source is read from the live state ('seLive'), the same view the
-- ordinary read functions use, in particular it does NOT follow a
-- @DO WITH@ pin. Merging from nothing (an absent source) is a no-op and
-- commits nothing.
mergeRef :: (VarKind, Text, [PValue]) -> (VarKind, Text, [PValue]) -> Env -> Either EvalError Env
mergeRef (_, dstRawName0, dstSubs0) _ _
  | any (T.null . unPValue) dstSubs0 = Left (NullSubscript dstRawName0 dstSubs0)
mergeRef (dstKind, dstRawName, dstSubs) (srcKind, srcRawName, srcSubs) env = do
  case srcKind of
    Global -> case checkReadAccess (envCurrentUser env) srcRawName (envStorage env) of
      Just reason -> Left (PermissionDenied (envCurrentUser env) reason)
      Nothing -> Right ()
    Local -> Right ()
  let srcName = case srcKind of
                  Local  -> resolveLocalName srcRawName env
                  Global -> srcRawName
      srcRoot = case srcKind of
                  Local  -> Map.findWithDefault emptyNode srcName (envLocals env)
                  Global -> Map.findWithDefault emptyNode srcName (seLive (envStorage env))
  case dstKind of
    Local ->
      let dstName = resolveLocalName dstRawName env
          dstRoot = Map.findWithDefault emptyNode dstName (envLocals env)
          dstRoot' = mergeNode dstSubs dstRoot srcSubs srcRoot
      in Right env { envLocals = Map.insert dstName dstRoot' (envLocals env) }
    Global -> do
      -- Every value under the source path, as a destination path + value.
      let leaves = [ (dstSubs ++ rel, val)
                   | (rel, val) <- maybe [] dumpTree (findSubNode srcSubs srcRoot) ]
      case checkWriteAccess (envCurrentUser env) dstRawName (envStorage env) of
        Just reason -> Left (PermissionDenied (envCurrentUser env) reason)
        Nothing -> Right ()
      case [ r | (subs, val) <- leaves
               , Just r <- [checkReservedGlobalWrite dstRawName subs (Just val) env] ] of
        (reason : _) -> Left (PermissionDenied (envCurrentUser env) reason)
        []           -> Right ()
      case [ subs | (subs, _) <- leaves, any (T.null . unPValue) subs ] of
        (subs : _) -> Left (NullSubscript dstRawName subs)
        []         -> Right ()
      if null leaves
        then Right env
        else case envActiveTx env of
          Just tx ->
            let tx' = foldl' (\t (subs, val) -> stageSet dstRawName subs val Nothing t) tx leaves
                node = Map.findWithDefault emptyNode dstRawName (envGlobals env)
                node' = foldl' (\n (subs, val) -> setNode subs val n) node leaves
            in Right env { envActiveTx = Just tx', envGlobals = Map.insert dstRawName node' (envGlobals env) }
          Nothing ->
            let tx = foldl' (\t (subs, val) -> stageSet dstRawName subs val Nothing t)
                            (newTxHandle 0 (envNow env)) leaves
            in case commitTransaction (envCurrentUser env) tx (envNow env) (envStorage env) of
                 Right (newTx, storage') ->
                   Right env
                     { envStorage = storage'
                     , envGlobals = seLive storage'
                     , envTxId    = T.pack (show newTx)
                     }
                 -- No expected-tx assertions are staged, so this cannot
                 -- fire today; surface it rather than swallow it like
                 -- the SET fallback does.
                 Left err -> Left (ConflictError (formatStorageError err))

-- --------------------------------------------------------------------
-- Variable Scoping (NEW) and Frame Management
-- --------------------------------------------------------------------

-- | Bind a formal parameter name as an alias to a caller's local variable name.
aliasVar :: Text -> Text -> Env -> Env
aliasVar param target env =
  let target' = resolveLocalName target env
      oldAlias = Map.lookup param (envAliases env)
      -- If the caller's variable already resolves to the parameter's
      -- own name (e.g. @DO F(&x)@ where F's formal is also called
      -- @x@), the parameter simply IS that variable: mapping it to
      -- itself would make 'resolveLocalName' loop forever, so drop any
      -- alias for it instead (restored on frame exit via 'frameAliases'
      -- like any other).
      aliases'
        | target' == param = Map.delete param (envAliases env)
        | otherwise        = Map.insert param target' (envAliases env)
      env' = env { envAliases = aliases' }
  in case envFrames env' of
       [] -> env'
       (f : fs) ->
         let f' = f { frameAliases = (param, oldAlias) : frameAliases f }
         in env' { envFrames = f' : fs }

-- | Push a variable restoration record onto the current call frame, and clear the local variable.
newVar :: Text -> Env -> Env
newVar rawName env =
  let name = resolveLocalName rawName env
      oldNode = Map.lookup name (envLocals env)
      env' = env { envLocals = Map.delete name (envLocals env) }
  in case envFrames env of
       [] -> env' -- At toplevel, NEW simply clears the variable
       (f : fs) ->
         let f' = f { frameRestores = (name, oldNode) : frameRestores f }
         in env' { envFrames = f' : fs }

-- | Bare NEW: scope all currently existing local variables.
newAllVars :: Env -> Env
newAllVars env =
  let currentNames = Map.keys (envLocals env)
  in foldr newVar env currentNames

-- | Push a new call frame when executing DO TAG / GOTO / CALL.
pushFrame :: Frame -> Env -> Env
pushFrame frame env = env { envFrames = frame : envFrames env }

-- | Pop the current call frame and restore any variables scoped by NEW and aliases in reverse order.
popFrame :: Env -> (Maybe Frame, Env)
popFrame env =
  case envFrames env of
    [] -> (Nothing, env)
    (f : fs) ->
      let restoredLocals = foldl restoreVar (envLocals env) (frameRestores f)
          restoredAliases = foldl restoreAlias (envAliases env) (frameAliases f)
          env' = env { envFrames = fs, envLocals = restoredLocals, envAliases = restoredAliases }
      in (Just f, env')
  where
    restoreVar locals (name, Nothing)      = Map.delete name locals
    restoreVar locals (name, Just oldNode) = Map.insert name oldNode locals
    restoreAlias aliases (param, Nothing)      = Map.delete param aliases
    restoreAlias aliases (param, Just oldTarg) = Map.insert param oldTarg aliases

-- --------------------------------------------------------------------
-- Output Handling
-- --------------------------------------------------------------------

-- | Result of 'appendOutput': either the updated 'Env', or the error
-- that the output limit was exceeded.  Using a dedicated type instead
-- of @Either EvalError Env@ avoids pulling the full 'EvalError' type
-- into callers that only care about the success case.
data OutputResult = OutputOk !Env | OutputLimitHit

-- | Append a string fragment to the environment's output buffer.
--
-- When 'envMaxOutputBytes' is set and the current buffer already
-- equals or exceeds the limit, returns 'OutputLimitHit' without
-- modifying the environment.  The caller ('Phase.Eval') is responsible
-- for turning this into an 'OutputLimitExceeded' error.
appendOutput :: Text -> Env -> OutputResult
appendOutput txt env =
  case envMaxOutputBytes env of
    Nothing -> OutputOk env { envOutput = envOutput env ++ [txt] }
    Just cap ->
      let used  = sum (map (BS.length . TE.encodeUtf8) (envOutput env))
          added = BS.length (TE.encodeUtf8 txt)
      in if used + added > cap
           then OutputLimitHit
           else OutputOk env { envOutput = envOutput env ++ [txt] }

-- | Retrieve all accumulated output as a single Text.
getOutputText :: Env -> Text
getOutputText env = T.concat (envOutput env)

-- | Clear the output buffer.
clearOutput :: Env -> Env
clearOutput env = env { envOutput = [] }
