{-# LANGUAGE OverloadedStrings #-}

-- | The Phase evaluation and execution engine.
--
-- Implements expression evaluation (arithmetic with 34 digits of precision,
-- comparisons, logical operations, slices, intrinsics) and statement execution
-- with line-level control flow, MUMPS node duality, '$TEST' tracking, and
-- 'NEW' variable restoration scoping.
module Phase.Eval
  ( -- * Evaluation Monad
    Eval (..)
  , runEval
  , evalState
  , execState
  , liftIO

    -- * Expression Evaluation
  , evalExpr
  , compareValues

    -- * Statement & Line Evaluation
  , Flow (..)
  , evalStmt
  , evalPredStmt
  , evalLine
  , evalRoutine
  , runRoutine
  ) where

import Control.Monad (forM_, when)
import qualified Data.Map.Strict as Map
import Data.Scientific (Scientific)
import qualified Data.Scientific as Sci
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (diffDays, fromGregorian)
import Data.Time.Clock (utctDay, utctDayTime)

import Phase.AST
import Phase.Catalog
import Phase.Lines (buildLogicalLines)
import Phase.State
import Phase.Stmt (parseLogicalLine)
import Phase.Storage.Lock (acquireLocks, releaseAllOwnerLocks, releaseLocks)
import Phase.Storage.Types (GlobalPath (..), TxId)
import Phase.Value

-- --------------------------------------------------------------------
-- Evaluation Monad
-- --------------------------------------------------------------------

-- | The Phase evaluation monad: stateful execution over 'Env', with
-- 'EvalError' handling, ALSO able to perform real 'IO' actions.
--
-- This used to be a purely pure @Env -> Either EvalError (a, Env)@
-- function (equivalent to @ExceptT EvalError (State Env)@ restated by
-- hand). It is now @Env -> IO (Either EvalError (a, Env))@
-- (equivalent to @ExceptT EvalError (StateT Env IO)@), because deep
-- time-travel queries, @$ORDER@/@$DATA@/reads targeting a moment
-- further back than whatever the resident 'StorageEngine' already has
-- loaded in memory, genuinely need to re-read the @.db@ file from
-- disk to answer correctly (see 'Phase.Storage.Engine''s dedicated
-- deep-replay functions). There is no way to serve that from a pure
-- function; the alternative (surfacing a distinct "needs deeper
-- history" error and pushing the retry logic out to the IO-level
-- caller) was considered and rejected as bad for usability, a
-- database feature that silently can't reach its own stored data
-- defeats the point of having it. So 'Eval' itself gained 'IO' access
-- via 'liftIO', following exactly the same "thread a real capability
-- through rather than work around its absence" precedent as
-- 'Env''s own 'envNow' field did for wall-clock time earlier.
--
-- This changes the type of every existing 'Eval' action from a pure
-- value to an 'IO' action, 'runEval'/'evalState'/'execState' now
-- return 'IO (Either EvalError ...)' instead of a bare 'Either', and
-- every call site that previously pattern-matched on a plain 'Either'
-- result (in @app/Main.hs@ and @test/Main.hs@) now needs to bind that
-- 'IO' action first.
newtype Eval a = Eval { unEval :: Env -> IO (Either EvalError (a, Env)) }

instance Functor Eval where
  fmap f (Eval g) = Eval $ \env -> do
    r <- g env
    case r of
      Left err        -> pure (Left err)
      Right (a, env') -> pure (Right (f a, env'))

instance Applicative Eval where
  pure a = Eval $ \env -> pure (Right (a, env))
  Eval f <*> Eval x = Eval $ \env -> do
    rf <- f env
    case rf of
      Left err         -> pure (Left err)
      Right (fn, env') -> do
        rx <- x env'
        case rx of
          Left err'          -> pure (Left err')
          Right (val, env'') -> pure (Right (fn val, env''))

instance Monad Eval where
  Eval m >>= k = Eval $ \env -> do
    r <- m env
    case r of
      Left err        -> pure (Left err)
      Right (a, env') -> unEval (k a) env'

-- | Run an evaluation computation on an initial environment.
runEval :: Eval a -> Env -> IO (Either EvalError (a, Env))
runEval = unEval

-- | Evaluate and return only the value.
evalState :: Eval a -> Env -> IO (Either EvalError a)
evalState m env = fmap fst <$> runEval m env

-- | Evaluate and return only the resulting environment.
execState :: Eval a -> Env -> IO (Either EvalError Env)
execState m env = fmap snd <$> runEval m env

-- | Lift an arbitrary 'IO' action into 'Eval'. This is the ONLY way
-- real IO (disk access, wall-clock reads, anything outside pure
-- computation) enters evaluation, used by deep time-travel replay
-- ('Phase.Storage.Engine') to re-read the @.db@ file when a query
-- targets a point further back than what's already resident in
-- memory.
liftIO :: IO a -> Eval a
liftIO action = Eval $ \env -> do
  a <- action
  pure (Right (a, env))

getEnv :: Eval Env
getEnv = Eval $ \env -> pure (Right (env, env))

modifyEnv :: (Env -> Env) -> Eval ()
modifyEnv f = Eval $ \env -> pure (Right ((), f env))

-- | Like 'modifyEnv', but for state-transforming functions that can
-- now fail, 'Phase.State.writeRef'/'killRef'/'mergeRef' return
-- 'Either EvalError Env' rather than a bare 'Env' since gaining
-- permission checks (the team's user/role system): a denied write must
-- stop evaluation with a real error, not silently either apply or
-- skip the mutation. Every call site that used to pass one of those
-- three functions straight to 'modifyEnv' now goes through this
-- instead.
modifyEnvE :: (Env -> Either EvalError Env) -> Eval ()
modifyEnvE f = Eval $ \env -> pure $ case f env of
  Left err   -> Left err
  Right env' -> Right ((), env')

-- | Like 'modifyEnv', but for 'appendOutput' which now returns
-- 'OutputResult' instead of a plain 'Env' (so it can signal the output
-- limit being exceeded without adding IO to 'Phase.State').
modifyEnvOutput :: (Env -> OutputResult) -> Eval ()
modifyEnvOutput f = Eval $ \env -> pure $ case f env of
  OutputLimitHit -> Left OutputLimitExceeded
  OutputOk env'  -> Right ((), env')

throwError :: EvalError -> Eval a
throwError err = Eval $ \_ -> pure (Left err)

-- | Render a 'Ref' back into roughly the source form it came from, for
-- use ONLY in error messages (e.g. the reference-parameter '<SYNTAX>'
-- check). Not a general pretty-printer, subscript expressions are
-- rendered as a fixed placeholder rather than fully reconstructed,
-- since an error message just needs to show enough to identify what
-- was rejected and why (global sigil present? subscripted?), not
-- reproduce the exact original text.
formatRefForError :: Ref -> Text
formatRefForError (Ref kind mNs name subs _mTT) =
  let sigil = case kind of
                Global -> "^"
                Local  -> ""
      nsPrefix = maybe "" (<> "^") mNs
      subsTxt = if null subs then "" else "(" <> T.intercalate "," (map (const "...") subs) <> ")"
  in nsPrefix <> sigil <> name <> subsTxt
formatRefForError (RefDynamic _) = "REF(...)"
formatRefForError (RefNaked _) = "^(...)"

-- | Resolve a 'RefDynamic' (or ordinary 'Ref') to the @(VarKind, name, subs)@
-- triple used by every write operation, 'writeRef', 'killRef', etc.
-- For 'Ref': evaluates subscripts and strips the namespace into the name
-- (same as the inline logic in 'evalStmt'). For 'RefDynamic': evaluates
-- all argument expressions, uses the first as the variable name (a leading
-- @^@ → global, stripped; anything else → local), and the rest as
-- subscripts.
-- Throws @<SYNTAX>@ for a 'RefDynamic' with zero arguments (the name is
-- mandatory). Throws 'SetOnReadOnly' if called on a time-traveled 'Ref'
-- (time-travel targets are read-only; the caller should use this helper
-- only for write targets and let 'evalExpr' handle time-traveled reads
-- through its own path).
-- | Resolve a time-travel bracket's EXPRESSION to a concrete, ABSOLUTE
-- transaction number, not a generic 'evalExpr', since 'TimeTravelLast'
-- ("LAST") is not an ordinary, independently-evaluable value; it only
-- means something as a reference point in this one syntactic position.
-- An ordinary expression that evaluates NEGATIVE is ALSO relative
-- (Part 1 of relative revision addressing, @docs/tx-pinning-design.md@):
-- there is no such thing as a negative transaction number, so that
-- range was free to repurpose rather than reject. Delegates the actual
-- "N revisions back" lookup to 'resolveRelativeTx', so every
-- downstream reader keeps dealing in plain absolute 'TxId's exactly as
-- before this feature existed.
resolveTimeTravelExpr :: Text -> [PValue] -> Expr -> Eval TxId
resolveTimeTravelExpr targetName subs ttExpr = do
  env <- getEnv
  case ttExpr of
    TimeTravelLast -> pure (resolveRelativeTx True targetName subs 0 env)
    BinExpr Sub TimeTravelLast nExpr -> do
      nVal <- evalExpr nExpr
      let n = round (Sci.toRealFloat (toNumber nVal) :: Double) :: Int
      pure (resolveRelativeTx True targetName subs n env)
    _ -> do
      val <- evalExpr ttExpr
      let num = round (Sci.toRealFloat (toNumber val) :: Double) :: Integer
      pure $ if num < 0
               then resolveRelativeTx False targetName subs (fromIntegral (negate num)) env
               else fromIntegral num

-- | A plain (no explicit bracket) read of a variable. Inside a
-- @DO WITH @TX(...)@ block ('envPinnedTx') every bare GLOBAL read is
-- answered as of the pinned transaction instead of live
-- (docs/tx-pinning-design.md Part 2); locals are never pinned, only
-- globals have a history to pin to.
--
-- Which globals are pinned is per global ('effectivePin'): a block with
-- an @@BIND(...)@ list pins only the bound ones and reads the rest live.
readPinnedOrLive :: VarKind -> Text -> [PValue] -> Env -> Eval (Either EvalError PValue)
readPinnedOrLive Global name subs env
  | Just pin <- effectivePin name env = liftIO (readAtTxRef pin name subs env)
readPinnedOrLive kind name subs env = pure (readRef kind name subs env)

-- | The pin a plain intrinsic reference ('$DATA'/'$GET'/'$ORDER') with
-- no bracket of its own inherits from an enclosing pinned block.
implicitPin :: VarKind -> Text -> Eval (Maybe TxId)
implicitPin Global name = effectivePin name <$> getEnv
implicitPin Local _     = pure Nothing

resolveWriteTarget :: Ref -> Eval (VarKind, Text, [PValue])
resolveWriteTarget (Ref kind mNS name subExprs mTimeTravel) = do
  case mTimeTravel of
    Just _  -> throwError SetOnReadOnly
    Nothing -> do
      let targetName = case kind of
                         Global -> qualifyGlobalName mNS name
                         Local  -> name
      rememberGlobalRef kind targetName
      subs <- mapM evalExpr subExprs
      pure (kind, targetName, subs)
resolveWriteTarget (RefDynamic []) =
  throwError (SyntaxError "REF() requires at least one argument (the variable name)")
resolveWriteTarget (RefDynamic (nameExpr : subExprs)) = do
  nameVal <- evalExpr nameExpr
  let raw = unPValue nameVal
      (kind, name) = if T.isPrefixOf "^" raw
                       then (Global, T.drop 1 raw)
                       else (Local,  raw)
  rememberGlobalRef kind name
  subs <- mapM evalExpr subExprs
  pure (kind, name, subs)
resolveWriteTarget (RefNaked subExprs) = resolveNakedRef subExprs

-- | Record this NAMED reference as the target a later naked reference
-- (@^(subs)@) should mean, only a GLOBAL counts, matching real M
-- (there is no such thing as a naked local). Called from every place
-- that resolves an explicitly-named 'Ref' (never from a 'RefNaked'
-- itself, which just reuses whatever this last recorded, see
-- 'resolveNakedRef').
rememberGlobalRef :: VarKind -> Text -> Eval ()
rememberGlobalRef Global name = modifyEnv (\e -> e { envLastGlobalRef = Just name })
rememberGlobalRef Local _     = pure ()

-- | Resolve a naked reference, @^(subs)@: the same global as the last
-- one referenced by name (see 'rememberGlobalRef'), keeping that name
-- but replacing the ENTIRE subscript list with @subs@, not a merge or
-- an append, exactly as real M defines it. 'NakedReference' (@<NAKED>@)
-- if nothing has established one yet this session.
resolveNakedRef :: [Expr] -> Eval (VarKind, Text, [PValue])
resolveNakedRef subExprs = do
  env <- getEnv
  case envLastGlobalRef env of
    Nothing   -> throwError NakedReference
    Just name -> do
      subs <- mapM evalExpr subExprs
      pure (Global, name, subs)

-- --------------------------------------------------------------------
-- Scientific Number Formatting & Decimal Division
-- --------------------------------------------------------------------

-- | Format a 'Scientific' as canonical or decimal text.
formatSci :: Scientific -> Text
formatSci s =
  let norm = Sci.normalize s
      (c, e) = (Sci.coefficient norm, Sci.base10Exponent norm)
  in if e >= 0
       then T.pack (show c ++ replicate e '0')
       else
         let isNeg = c < 0
             absC = abs c
             digits = show absC
             numDigs = length digits
             dotPos = numDigs + e
         in if dotPos <= 0
              then
                let leadingZeros = replicate (-dotPos) '0'
                    res = "0." ++ leadingZeros ++ digits
                in T.pack (if isNeg then "-" ++ res else res)
              else
                let (intP, fracP) = splitAt dotPos digits
                    res = intP ++ (if null fracP then "" else "." ++ fracP)
                in T.pack (if isNeg then "-" ++ res else res)

-- | Decimal division with 34 digits of precision and HALF_UP rounding.
scientificDiv :: Scientific -> Scientific -> Either EvalError Scientific
scientificDiv num den
  | den == 0 = Left DivideByZero
  | otherwise =
      let c1 = Sci.coefficient num
          e1 = Sci.base10Exponent num
          c2 = Sci.coefficient den
          e2 = Sci.base10Exponent den
          scaleExtra = 40 :: Int
          scaledNum = c1 * (10 ^ scaleExtra)
          q = scaledNum `div` c2
          effExp = e1 - e2 - scaleExtra
      in Right (Sci.normalize (Sci.scientific q effExp))

-- | Exact truncated-toward-zero integer quotient of two 'Scientific'
-- values, used by '\' (IntDiv) and '#' (Mod) so both stay on the same
-- 34-digit-precision @BigDecimal@ footing the handbook promises for
-- ALL SIX arithmetic operators, instead of round-tripping through
-- 'Double' (~15-17 significant digits) the way this used to. Unlike
-- 'scientificDiv' (which deliberately approximates '/' to a fixed
-- number of extra digits, since '/' can be a genuinely non-terminating
-- decimal), this is exact: scaling one operand's coefficient by a
-- WHOLE power of ten to match the other's exponent introduces no
-- rounding at all, so 'quot' (Haskell's own truncate-toward-zero
-- integer division -- matching the truncation this module already
-- applied via 'truncate' on a 'Double') gives back the exact
-- mathematical answer.
exactIntQuotient :: Scientific -> Scientific -> Either EvalError Integer
exactIntQuotient num den
  | den == 0 = Left DivideByZero
  | otherwise =
      let c1 = Sci.coefficient num
          e1 = Sci.base10Exponent num
          c2 = Sci.coefficient den
          e2 = Sci.base10Exponent den
      in Right $ if e1 >= e2
                   then (c1 * 10 ^ (e1 - e2)) `quot` c2
                   else c1 `quot` (c2 * 10 ^ (e2 - e1))

-- | Comparison helper: compares canonically if both sides are canonical,
-- otherwise compares lexicographically as raw string text.
compareValues :: PValue -> PValue -> Ordering
compareValues a b =
  case (canonicalNumber a, canonicalNumber b) of
    (Just na, Just nb) -> compare na nb
    _                  -> compare (unPValue a) (unPValue b)

-- --------------------------------------------------------------------
-- Expression Evaluation
-- --------------------------------------------------------------------

-- | Evaluate a Phase expression to a 'PValue'.
-- | Helper to format global names qualified by optional namespace.
qualifyGlobalName :: Maybe Text -> Text -> Text
qualifyGlobalName Nothing name = name
qualifyGlobalName (Just ns) name = ns <> "^" <> name

evalExpr :: Expr -> Eval PValue
evalExpr (Lit val) = pure val
evalExpr TimeTravelLast =
  -- Reachable only if 'LAST'/'LAST-N' somehow end up somewhere other
  -- than a time-travel bracket's own expression slot, where
  -- 'resolveTimeTravelExpr' handles it specially BEFORE generic
  -- 'evalExpr' ever sees it. A real, catchable Phase error instead of
  -- a Haskell pattern-match crash if that invariant is ever broken,
  -- GHC's own non-exhaustive-pattern warning on this function is what
  -- turned this up; this clause is what actually closes it.
  throwError (SyntaxError "LAST is only valid inside a time-travel bracket, ^[...]")

evalExpr (Call target) = do
  vals <- invokeTarget target
  case vals of
    (v : _) -> pure v
    []      -> pure (mkValue "")

evalExpr (CallWith spec target) = do
  vals <- withPinScope spec (invokeTarget target)
  case vals of
    (v : _) -> pure v
    []      -> pure (mkValue "")

evalExpr (Var (Ref kind mNS name subExprs mTimeTravel)) = do
  subs <- mapM evalExpr subExprs
  let targetName = case kind of
                     Global -> qualifyGlobalName mNS name
                     Local  -> name
  rememberGlobalRef kind targetName
  case mTimeTravel of
    Nothing -> do
      env <- getEnv
      readResult <- readPinnedOrLive kind targetName subs env
      case readResult of
        Right val -> pure val
        Left err ->
          if kind == Local && null subs
            then case lookupItem (envCurrentNamespace env) targetName env of
                   Just (KindConstant, _, constVal) -> pure (mkValue constVal)
                   _ -> throwError err
            else throwError err
    Just ttExpr -> do
      txNum <- resolveTimeTravelExpr targetName subs ttExpr
      env <- getEnv
      result <- liftIO (readAtTxRef txNum targetName subs env)
      case result of
        Right val -> pure val
        Left err  -> throwError err

evalExpr (Var (RefDynamic [])) =
  throwError (SyntaxError "REF() requires at least one argument (the variable name)")
evalExpr (Var (RefDynamic (nameExpr : subExprs))) = do
  nameVal <- evalExpr nameExpr
  let raw = unPValue nameVal
      (kind, name) = if T.isPrefixOf "^" raw
                       then (Global, T.drop 1 raw)
                       else (Local,  raw)
  rememberGlobalRef kind name
  subs <- mapM evalExpr subExprs
  env <- getEnv
  readResult <- readPinnedOrLive kind name subs env
  case readResult of
    Right val -> pure val
    Left err  -> throwError err

evalExpr (Var (RefNaked subExprs)) = do
  (kind, name, subs) <- resolveNakedRef subExprs
  env <- getEnv
  readResult <- readPinnedOrLive kind name subs env
  case readResult of
    Right val -> pure val
    Left err  -> throwError err

evalExpr (UnExpr op expr) = do
  val <- evalExpr expr
  case op of
    Neg ->
      let n = toNumber val
      in pure (mkValue (formatSci (-n)))
    Pos ->
      let n = toNumber val
      in pure (mkValue (formatSci n))
    Not ->
      if isTruthy val
        then pure (mkValue "0")
        else pure (mkValue "1")
    Indirect ->
      throwError (NotImplemented "@" "@ indirection is not implemented")

evalExpr (BinExpr op leftExpr rightExpr) = do
  case op of
    -- Logical AND / OR: non-short-circuiting per handbook §4 (evaluates both)
    And -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      if isTruthy l && isTruthy r
        then pure (mkValue "1")
        else pure (mkValue "0")

    Or -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      if isTruthy l || isTruthy r
        then pure (mkValue "1")
        else pure (mkValue "0")

    -- String concatenation
    Concat -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (unPValue l <> unPValue r))

    -- Equality & Comparisons
    Eq -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if phaseEq l r then "1" else "0"))

    NotEq -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if not (phaseEq l r) then "1" else "0"))

    Lt -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if compareValues l r == LT then "1" else "0"))

    NotLt -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if compareValues l r /= LT then "1" else "0"))

    Gt -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if compareValues l r == GT then "1" else "0"))

    NotGt -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if compareValues l r /= GT then "1" else "0"))

    Contains -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if unPValue r `T.isInfixOf` unPValue l then "1" else "0"))

    NotContains -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if not (unPValue r `T.isInfixOf` unPValue l) then "1" else "0"))

    SortsAfter -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if collationCompare l r == GT then "1" else "0"))

    NotSortsAfter -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (if collationCompare l r /= GT then "1" else "0"))

    Match -> throwError (NotImplemented "?" "? pattern matching is not implemented")
    NotMatch -> throwError (NotImplemented "'?" "'? pattern matching is not implemented")

    -- Arithmetic
    Add -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (formatSci (toNumber l + toNumber r)))

    Sub -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (formatSci (toNumber l - toNumber r)))

    Mul -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      pure (mkValue (formatSci (toNumber l * toNumber r)))

    Div -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      case scientificDiv (toNumber l) (toNumber r) of
        Left err  -> divisionError err l r
        Right res -> pure (mkValue (formatSci res))

    IntDiv -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      case exactIntQuotient (toNumber l) (toNumber r) of
        Left err -> divisionError err l r
        Right q  -> pure (mkValue (T.pack (show q)))

    Mod -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      let nl = toNumber l
          nr = toNumber r
      case exactIntQuotient nl nr of
        Left err -> divisionError err l r
        Right q  ->
          -- Exact: 'Scientific' addition/multiplication never rounds
          -- (only division does), and 'q' itself is already the exact
          -- truncated quotient, so this remainder is exact too.
          let remVal = nl - Sci.scientific q 0 * nr
          in pure (mkValue (formatSci remVal))

    Pow -> do
      l <- evalExpr leftExpr
      r <- evalExpr rightExpr
      let nl = Sci.toRealFloat (toNumber l) :: Double
          nr = Sci.toRealFloat (toNumber r) :: Double
          res = nl ** nr
      pure (mkValue (formatSci (Sci.fromFloatDigits res)))

evalExpr (Slice baseExpr mStartExpr mEndExpr) = do
  baseVal <- evalExpr baseExpr
  let rawText = unPValue baseVal
      len = T.length rawText
  startIdx <- case mStartExpr of
                Nothing -> pure 1
                Just se -> fromIntegral . (truncate :: Double -> Integer) . Sci.toRealFloat . toNumber <$> evalExpr se
  endIdx <- case mEndExpr of
              Nothing -> pure len
              Just ee -> fromIntegral . (truncate :: Double -> Integer) . Sci.toRealFloat . toNumber <$> evalExpr ee
  -- 1-indexed, inclusive substring
  if startIdx > endIdx || startIdx > len || endIdx < 1
    then pure (mkValue "")
    else
      let actualStart = max 1 startIdx
          actualEnd = min len endIdx
          count = actualEnd - actualStart + 1
          sliceText = T.take count (T.drop (actualStart - 1) rawText)
      in pure (mkValue sliceText)

evalExpr (FuncCall name argExprs) =
  evalIntrinsic name argExprs

-- | Helper to resolve a reference for intrinsics (handles Ref and RefDynamic).
-- Returns (kind, qualified name, evaluated subscripts, optional
-- ABSOLUTE time-travel transaction number, already resolved via
-- 'resolveTimeTravelExpr', so a relative/LAST-based bracket looks
-- identical to an ordinary one by the time a caller sees it).
resolveIntrinsicRef :: Ref -> Eval (VarKind, Text, [PValue], Maybe TxId)
resolveIntrinsicRef (Ref kind mNS name subExprs mTimeTravel) = do
  subs <- mapM evalExpr subExprs
  let targetName = case kind of
                     Global -> qualifyGlobalName mNS name
                     Local  -> name
  rememberGlobalRef kind targetName
  mTx <- case mTimeTravel of
           Nothing     -> implicitPin kind targetName
           Just ttExpr -> Just <$> resolveTimeTravelExpr targetName subs ttExpr
  pure (kind, targetName, subs, mTx)
resolveIntrinsicRef (RefDynamic []) =
  throwError (SyntaxError "REF() requires at least one argument (the variable name)")
resolveIntrinsicRef (RefDynamic (nameExpr : subExprs)) = do
  nameVal <- evalExpr nameExpr
  let raw = unPValue nameVal
      (kind, name) = if T.isPrefixOf "^" raw
                       then (Global, T.drop 1 raw)
                       else (Local,  raw)
  rememberGlobalRef kind name
  subs <- mapM evalExpr subExprs
  mTx <- implicitPin kind name
  pure (kind, name, subs, mTx)
resolveIntrinsicRef (RefNaked subExprs) = do
  (kind, name, subs) <- resolveNakedRef subExprs
  mTx <- implicitPin kind name
  pure (kind, name, subs, mTx)

evalIntrinsic :: Text -> [Expr] -> Eval PValue
evalIntrinsic "$DATA" [Var ref] = do
  (kind, targetName, subs, mTx) <- resolveIntrinsicRef ref
  case mTx of
    Nothing -> do
      env <- getEnv
      pure (mkValue (T.pack (show (dataRef kind targetName subs env))))
    Just txNum -> do
      env <- getEnv
      dataResult <- liftIO (dataAtTxRef txNum targetName subs env)
      pure (mkValue (T.pack (show dataResult)))

evalIntrinsic "$ORDER" [Var ref] = do
  (kind, targetName, subs, mTx) <- resolveIntrinsicRef ref
  if null subs
    then throwError (SyntaxError "$ORDER requires a subscripted reference")
    else do
      env <- getEnv
      case mTx of
        -- An explicit @^[tx]@/@^[-N]@ bracket, or the pin of an
        -- enclosing @DO WITH @TX(...)@ block: iterate the children as
        -- they stood at that transaction.
        Just txNum -> liftIO (orderAtTxRef txNum targetName (init subs) (last subs) env)
        Nothing    -> pure (orderRef kind targetName (init subs) (last subs) env)

evalIntrinsic "$GET" [Var ref] = do
  (kind, targetName, subs, mTx) <- resolveIntrinsicRef ref
  env <- getEnv
  case mTx of
    Nothing ->
      case readRef kind targetName subs env of
        Right val -> pure val
        Left _    -> pure (mkValue "")
    Just txNum -> do
      result <- liftIO (readAtTxRef txNum targetName subs env)
      case result of
        Right val -> pure val
        Left _    -> pure (mkValue "")

evalIntrinsic "$GET" [Var ref, defExpr] = do
  (kind, targetName, subs, mTx) <- resolveIntrinsicRef ref
  env <- getEnv
  case mTx of
    Nothing ->
      case readRef kind targetName subs env of
        Right val -> pure val
        Left _    -> evalExpr defExpr
    Just txNum -> do
      result <- liftIO (readAtTxRef txNum targetName subs env)
      case result of
        Right val -> pure val
        Left _    -> evalExpr defExpr

evalIntrinsic "$GET" [nonVarExpr] = evalExpr nonVarExpr
evalIntrinsic "$GET" [nonVarExpr, _] = evalExpr nonVarExpr

evalIntrinsic "$LENGTH" [expr] = do
  val <- evalExpr expr
  pure (mkValue (T.pack (show (T.length (unPValue val)))))

evalIntrinsic "$EXTRACT" [expr] = evalIntrinsic "$EXTRACT" [expr, Lit (mkValue "1")]

evalIntrinsic "$EXTRACT" [expr, startExpr] = do
  val <- evalExpr expr
  sVal <- evalExpr startExpr
  let rawText = unPValue val
      len = T.length rawText
      startIdx = truncate (Sci.toRealFloat (toNumber sVal) :: Double) :: Int
  if startIdx < 1 || startIdx > len
    then pure (mkValue "")
    else pure (mkValue (T.singleton (T.index rawText (startIdx - 1))))

evalIntrinsic "$EXTRACT" [expr, startExpr, endExpr] = do
  val <- evalExpr expr
  sVal <- evalExpr startExpr
  eVal <- evalExpr endExpr
  let rawText = unPValue val
      len = T.length rawText
      startIdx = truncate (Sci.toRealFloat (toNumber sVal) :: Double) :: Int
      endIdx = truncate (Sci.toRealFloat (toNumber eVal) :: Double) :: Int
  if startIdx > endIdx || startIdx > len || endIdx < 1
    then pure (mkValue "")
    else
      let actualStart = max 1 startIdx
          actualEnd = min len endIdx
          count = actualEnd - actualStart + 1
          extracted = T.take count (T.drop (actualStart - 1) rawText)
      in pure (mkValue extracted)

evalIntrinsic "$PIECE" [expr, delimExpr] = evalIntrinsic "$PIECE" [expr, delimExpr, Lit (mkValue "1")]

evalIntrinsic "$PIECE" [expr, delimExpr, whichExpr] = do
  val <- evalExpr expr
  dVal <- evalExpr delimExpr
  wVal <- evalExpr whichExpr
  let rawText = unPValue val
      delim = unPValue dVal
      which = truncate (Sci.toRealFloat (toNumber wVal) :: Double) :: Int
  if T.null delim
    then pure (mkValue "")
    else
      let pieces = T.splitOn delim rawText
      in if which < 1 || which > length pieces
           then pure (mkValue "")
           else pure (mkValue (pieces !! (which - 1)))

evalIntrinsic "$TEST" [] = do
  env <- getEnv
  pure (mkValue (if envTest env then "1" else "0"))

evalIntrinsic "$JOB" [] = do
  env <- getEnv
  pure (mkValue (envJobId env))

evalIntrinsic "$TX" [] = do
  env <- getEnv
  pure (mkValue (envTxId env))

evalIntrinsic "$HOROLOG" [] = do
  env <- getEnv
  let now = envNow env
      days = diffDays (utctDay now) (fromGregorian 1840 12 31)
      secs = truncate (utctDayTime now) :: Integer
  pure (mkValue (T.pack (show days <> "," <> show secs)))

evalIntrinsic "$CHILDCOUNT" [Var ref] = do
  (kind, targetName, subs, mTTText) <- resolveIntrinsicRef ref
  case mTTText of
    Just _ -> throwError (NotImplemented "$CHILDCOUNT" "time-traveled $CHILDCOUNT is not implemented")
    Nothing -> do
      env <- getEnv
      pure (mkValue (T.pack (show (childCountRef kind targetName subs env))))
-- Explicitly stubbed intrinsics (Part XIII)
evalIntrinsic "$TEXT" _ = throwError (NotImplemented "$TEXT" "$TEXT is not implemented")
evalIntrinsic "$TRANSLATE" _ = throwError (NotImplemented "$TRANSLATE" "$TRANSLATE is not implemented")
evalIntrinsic "$JUSTIFY" _ = throwError (NotImplemented "$JUSTIFY" "$JUSTIFY is not implemented")
evalIntrinsic "$SELECT" _ = throwError (NotImplemented "$SELECT" "$SELECT is not implemented")

evalIntrinsic unknown _ = do
  env <- getEnv
  let u = T.toUpper unknown
  if not (envFileIOEnabled env) && (u == "$BLOBWRITE" || u == "BLOBSAVE")
    then throwError FileAccessDisabled
    else throwError (UndefinedFunction unknown)

-- --------------------------------------------------------------------
-- Statement & Control Flow
-- --------------------------------------------------------------------

-- | Control flow directive resulting from a statement execution.
data Flow
  = NextStmt
  | SkipToElse
  | ExitLine
  | JumpTo !(Int, Int)
  | QuitFrame !(Maybe [PValue])
  deriving (Eq, Show)

-- | Evaluate a statement paired with its optional postcondition.
evalPredStmt :: PredStmt -> [PredStmt] -> Int -> Eval Flow
evalPredStmt (PredStmt mPostCond stmt) stmts stmtIdx = do
  shouldRun <- case mPostCond of
    Nothing -> pure True
    Just condExpr -> do
      condVal <- evalExpr condExpr
      let isTrue = isTruthy condVal
      modifyEnv (\e -> e { envTest = isTrue })
      pure isTrue
  if shouldRun
    then evalStmt stmt stmts stmtIdx
    else pure NextStmt

-- | Splice a replacement into a fixed piece of a delimited string, for
-- @SET $PIECE(ref,delim,which)=val@ (Part XIII: not just an
-- expression-position read anymore). Matches real M: a 'which' beyond
-- the CURRENT piece count pads with empty pieces up to it rather than
-- erroring, exactly as the expression-position @$PIECE@ read already
-- treats an out-of-range 'which' permissively (returns @\"\"@ instead
-- of erroring). An empty delimiter has no real-M-documented SET
-- behavior (the read form already short-circuits to @\"\"@ for one);
-- here it treats the whole string as a single piece, 'which'@<=1@
-- replaces it outright, any other 'which' is a no-op (nothing to pad
-- WITH when there is no delimiter to pad separators between).
splicePiece :: Text -> Text -> Int -> Text -> Text
splicePiece original delim which0 replacement
  | T.null delim = if which <= 1 then replacement else original
  | otherwise =
      let pieces = T.splitOn delim original
          extended = pieces ++ replicate (max 0 (which - length pieces)) ""
          updated = [ if i == which then replacement else p | (i, p) <- zip [1 ..] extended ]
      in T.intercalate delim updated
  where
    which = max 1 which0

-- | Splice a replacement into a 1-indexed, inclusive character range,
-- for @SET $EXTRACT(ref,start,end)=val@ (Part XIII). Matches real M:
-- 'start' beyond the current length pads with spaces; 'end' < 'start'
-- inserts 'replacement' at 'start' without removing any existing
-- characters, rather than erroring.
spliceExtract :: Text -> Int -> Int -> Text -> Text
spliceExtract original start0 end replacement =
  let start = max 1 start0
      before = T.take (start - 1) original
      padding = T.replicate (max 0 (start - 1 - T.length before)) " "
      after = T.drop (max (start - 1) end) original
  in before <> padding <> replacement <> after

-- | Evaluate an individual statement.
evalStmt :: Stmt -> [PredStmt] -> Int -> Eval Flow
evalStmt (SSet assignments) _ _ = do
  forM_ assignments $ \(target, valExpr) -> case target of
    TargetRef ref -> do
      (kind, targetName, subs) <- resolveWriteTarget ref
      val <- evalExpr valExpr
      modifyEnvE (writeRef kind targetName subs val)
    TargetPiece ref delimExpr whichExpr -> do
      (kind, targetName, subs) <- resolveWriteTarget ref
      val <- evalExpr valExpr
      delimVal <- evalExpr delimExpr
      whichVal <- evalExpr whichExpr
      env <- getEnv
      case readRef kind targetName subs env of
        Left err -> throwError err
        Right originalVal -> do
          let which = truncate (Sci.toRealFloat (toNumber whichVal) :: Double) :: Int
              spliced = splicePiece (unPValue originalVal) (unPValue delimVal) which (unPValue val)
          modifyEnvE (writeRef kind targetName subs (mkValue spliced))
    TargetExtract ref startExpr endExpr -> do
      (kind, targetName, subs) <- resolveWriteTarget ref
      val <- evalExpr valExpr
      startVal <- evalExpr startExpr
      endVal <- evalExpr endExpr
      env <- getEnv
      case readRef kind targetName subs env of
        Left err -> throwError err
        Right originalVal -> do
          let start = truncate (Sci.toRealFloat (toNumber startVal) :: Double) :: Int
              end = truncate (Sci.toRealFloat (toNumber endVal) :: Double) :: Int
              spliced = spliceExtract (unPValue originalVal) start end (unPValue val)
          modifyEnvE (writeRef kind targetName subs (mkValue spliced))
  pure NextStmt

evalStmt (SCallAssign targets target) _ _ = do
  vals <- invokeTarget target
  if length vals /= length targets
    then throwError (SyntaxError ("CALL returned " <> T.pack (show (length vals)) <> " value(s), " <> T.pack (show (length targets)) <> " target(s) expected"))
    else do
      forM_ (zip targets vals) $ \(ref, val) -> do
        (kind, targetName, subs) <- resolveWriteTarget ref
        modifyEnvE (writeRef kind targetName subs val)
      pure NextStmt

evalStmt (SMerge pairs) _ _ = do
  forM_ pairs $ \(dstRef, srcRef) -> do
    (dstKind, dstTarget, dstSubs) <- resolveWriteTarget dstRef
    (srcKind, srcTarget, srcSubs) <- resolveWriteTarget srcRef
    modifyEnvE (mergeRef (dstKind, dstTarget, dstSubs) (srcKind, srcTarget, srcSubs))
  pure NextStmt

evalStmt (SKill mTargets) _ _ = do
  case mTargets of
    Nothing -> modifyEnv killAllLocals
    Just targets ->
      forM_ targets $ \ref -> do
        (kind, targetName, subs) <- resolveWriteTarget ref
        modifyEnvE (killRef kind targetName subs)
  pure NextStmt

evalStmt (SWrite items) _ _ = do
  forM_ items $ \item ->
    case item of
      Left expr -> do
        val <- evalExpr expr
        modifyEnvOutput (appendOutput (unPValue val))
      Right WNewline ->
        modifyEnvOutput (appendOutput "\n")
      Right WFormFeed ->
        modifyEnvOutput (appendOutput "\f")
      Right (WTabTo col) ->
        modifyEnvOutput (appendOutput (T.replicate col " "))
  pure NextStmt

evalStmt (SIf conds) _ _ = do
  -- Comma = AND; short-circuits on first false condition
  let checkConds [] = pure True
      checkConds (c : cs) = do
        val <- evalExpr c
        if isTruthy val
          then checkConds cs
          else pure False
  isTrue <- checkConds conds
  modifyEnv (\e -> e { envTest = isTrue })
  if isTrue
    then pure NextStmt
    else pure SkipToElse

evalStmt SElse _ _ = do
  env <- getEnv
  -- If previous IF was true, ELSE skips rest of line; if false, ELSE runs
  if envTest env
    then pure ExitLine
    else pure NextStmt

evalStmt (SNew mNames) _ _ = do
  case mNames of
    Nothing    -> modifyEnv newAllVars
    Just names -> forM_ names (\name -> modifyEnv (newVar name))
  pure NextStmt

evalStmt (SQuit mVal) _ _ = do
  mPVal <- case mVal of
             Nothing -> pure Nothing
             Just e  -> Just <$> evalExpr e
  pure (QuitFrame (case mPVal of
                     Nothing -> Nothing
                     Just v  -> Just [v]))

evalStmt (SReturn vals) _ _ = do
  pVals <- mapM evalExpr vals
  pure (QuitFrame (Just pVals))

evalStmt (SExpr expr) _ _ = do
  _ <- evalExpr expr
  pure NextStmt

evalStmt (SFor mSpec) stmts stmtIdx = do
  let remainingStmts = drop (stmtIdx + 1) stmts
  case mSpec of
    Nothing -> do
      -- Bare infinite loop over the rest of the line.
      -- In M/Phase, QUIT inside a FOR exits the FOR loop only, it does NOT
      -- propagate as a QuitFrame (frame-exit) to the surrounding routine, because
      -- FOR never pushes a call frame of its own. QuitFrame here means "stop
      -- iterating"; the routine continues with the statement after the FOR's line.
      -- A QUIT with a value (QUIT expr) inside a bare-FOR is treated identically
      -- by this rule: the value is discarded, and the loop exits cleanly. If the
      -- intent is to return a value from the surrounding DO/CALL, a QUIT after
      -- the FOR (not inside it) should be used instead.
      let loop = do
            flow <- evalStmtSequence remainingStmts
            case flow of
              QuitFrame _ -> pure ExitLine
              ExitLine    -> pure ExitLine
              _           -> loop
      loop

    Just (ForSpec var startExpr mIncExpr mStopExpr) -> do
      startVal <- evalExpr startExpr
      let startNum = toNumber startVal
      incNum <- case mIncExpr of
                  Nothing -> pure 1
                  Just ie -> toNumber <$> evalExpr ie
      mStopNum <- case mStopExpr of
                    Nothing -> pure Nothing
                    Just se -> Just . toNumber <$> evalExpr se

      let loop curNum = do
            -- Check termination condition
            let shouldStop = case mStopNum of
                               Nothing -> False
                               Just stopNum ->
                                 if incNum >= 0
                                   then curNum > stopNum
                                   else curNum < stopNum
            if shouldStop
              then pure ExitLine
              else do
                -- Assign loop variable
                modifyEnvE (writeRef Local var [] (mkValue (formatSci curNum)))
                flow <- evalStmtSequence remainingStmts
                case flow of
                  -- Same rule as bare FOR: QUIT exits the FOR loop only,
                  -- not the surrounding routine. FOR never pushes a call frame.
                  QuitFrame _ -> pure ExitLine
                  ExitLine    -> pure ExitLine
                  _           -> loop (curNum + incNum)

      loop startNum

evalStmt (SForEach var collRef) stmts stmtIdx = do
  let remainingStmts = drop (stmtIdx + 1) stmts
  (kind, targetName, parentSubs) <- resolveWriteTarget collRef

  -- Initial assignment: var = ""
  modifyEnvE (writeRef Local var [] (mkValue ""))

  let loop = do
        env <- getEnv
        curVal <- case readRef Local var [] env of
                    Right v -> pure v
                    Left _  -> pure (mkValue "")
        let nextVal = orderRef kind targetName parentSubs curVal env
        if nextVal == mkValue ""
          then pure ExitLine
          else do
            modifyEnvE (writeRef Local var [] nextVal)
            flow <- evalStmtSequence remainingStmts
            case flow of
              QuitFrame v -> pure (QuitFrame v)
              ExitLine    -> pure ExitLine
              _           -> loop

  loop

evalStmt (SEndDo) _ _ = pure NextStmt

-- EVALUATE ... END EVALUATE (Part II §5). See 'runEvaluate'.
evalStmt (SEvaluate subjects) _ _ = runEvaluate subjects
evalStmt SEndEvaluate _ _ = pure NextStmt

-- CONDITION / SIGNAL / HANDLE / CATCH / RETRY (Part III §1)

-- A declaration does nothing when reached: its fields were collected
-- when the routine started ('collectConditionDecls').
evalStmt (SCondition _ _) _ _ = pure NextStmt

evalStmt (SSignal name fieldExprs) _ _ = do
  decls <- envConditionDecls <$> getEnv
  case Map.lookup name decls of
    Just declared ->
      forM_ fieldExprs $ \(f, _) ->
        when (f `notElem` declared) $
          throwError (SyntaxError ("SIGNAL <" <> name <> "> field '" <> f <> "' is not declared by its CONDITION"))
    Nothing -> pure ()
  vals <- mapM (\(f, e) -> (,) f <$> evalExpr e) fieldExprs
  throwError (Signaled (Condition name vals))

-- A clause is only ever run by the handled block that owns it
-- ('runHandledBlock'); one reached any other way has no block to belong
-- to.
evalStmt (SHandle _ _) _ _ =
  throwError (SyntaxError "HANDLE without an enclosing DO block")
evalStmt (SCatch _ _) _ _ =
  throwError (SyntaxError "CATCH without an enclosing DO block")

-- RETRY asks for the block whose HANDLE clause this is to be re-entered.
-- It cannot jump there directly (it may be nested inside dotted blocks
-- and loops inside the clause), so it records the request in the
-- environment and ends the current frame; every block level on the way
-- out sees the request and keeps unwinding until 'runHandledBlock'
-- claims it.
evalStmt (SRetry mCount seedExprs) _ _ = do
  env <- getEnv
  if not (envInHandle env)
    then throwError (SyntaxError "RETRY outside a HANDLE clause")
    else do
      wanted <- case mCount of
        Nothing -> pure True
        Just e  -> do
          n <- evalExpr e
          pure (fromIntegral (envRetryCount env) < toNumber n)
      if not wanted
        then pure NextStmt    -- RETRY n TIMES, all n used: carry on after it
        else do
          let limit = retryLimit env
          when (envRetryCount env >= limit) $
            throwError (CustomError ("RETRY limit reached: this block has already been re-entered "
                                     <> T.pack (show limit) <> " times (^_CONFIG(\"retry\",\"max\"))"))
          seeds <- mapM (\(f, e) -> (,) f <$> evalExpr e) seedExprs
          modifyEnv (\e -> e { envRetry = Just seeds })
          pure (QuitFrame Nothing)

-- A WHEN clause is only ever run by the EVALUATE that owns it, which
-- finds it among the lines one level deeper than its header; those
-- lines are skipped by the ordinary "indented lines belong to a block"
-- rule. A clause reached any other way (no EVALUATE above it) is a
-- program error, not something to skip silently.
evalStmt (SWhen _ _) _ _ =
  throwError (SyntaxError "WHEN without an enclosing EVALUATE")
evalStmt SWhenOther _ _ =
  throwError (SyntaxError "WHEN OTHER without an enclosing EVALUATE")

evalStmt (SLock mSpecs) _ _ = do
  case mSpecs of
    Nothing ->
      modifyEnv (\e -> e { envLocks = releaseAllOwnerLocks (envJobId e) (envLocks e) })
    Just specs ->
      forM_ specs $ \spec -> do
        -- 'resolveWriteTarget' handles 'Ref'/'RefDynamic'/'RefNaked'
        -- uniformly (this used to bare-field-access 'lsRef spec' as
        -- though it were always the plain 'Ref' constructor, which
        -- would have crashed on a naked or dynamic lock target).
        (_, targetName, subs) <- resolveWriteTarget (lsRef spec)
        let path = GlobalPath targetName subs
        case lsSign spec of
          Just False ->
            modifyEnv (\e ->
              let locks' = releaseLocks (envJobId e) [path] (envLocks e)
              in e { envLocks = locks' })
          _ -> do
            env <- getEnv
            let (acquired, locks') = acquireLocks (envJobId env) [path] (envLocks env)
            if acquired || envHandlerDepth env == 0
              then modifyEnv (\e -> e { envLocks = locks' })
              else do
                -- A handler is present, so a failed acquisition is a
                -- condition rather than a silent failure (Part III §1.1).
                timeoutMs <- maybe (pure (mkValue "0")) evalExpr (lsTimeout spec)
                throwError (Signaled (Condition "LOCK-TIMEOUT"
                  [ ("global", mkValue targetName)
                  , ("path", mkValue (T.intercalate "," (map unPValue subs)))
                  , ("timeoutMs", timeoutMs) ]))
  pure NextStmt

evalStmt (SUnlock mSpecs) _ _ = do
  case mSpecs of
    Nothing ->
      modifyEnv (\e -> e { envLocks = releaseAllOwnerLocks (envJobId e) (envLocks e) })
    Just specs -> do
      forM_ specs $ \spec -> do
        (_, targetName, subs) <- resolveWriteTarget (lsRef spec)
        let path = GlobalPath targetName subs
        modifyEnv (\e ->
          let locks' = releaseLocks (envJobId e) [path] (envLocks e)
          in e { envLocks = locks' })
  pure NextStmt

evalStmt (SNamespace ns) _ _ = do
  modifyEnv (\e -> e { envCurrentNamespace = ns })
  pure NextStmt

evalStmt (SDo mTarget) _ _ =
  case mTarget of
    Nothing -> runDoBlock
    Just target -> do
      _ <- invokeTarget target
      pure NextStmt

evalStmt (SDoWith spec mTarget) _ _ =
  case mTarget of
    Nothing -> withPinScope spec runDoBlock
    Just target -> do
      _ <- withPinScope spec (invokeTarget target)
      pure NextStmt

evalStmt (SGoto (CallTarget mTag mRoutine _)) _ _ = do
  env <- getEnv
  case (mTag, mRoutine) of
    (Nothing, Just routine) ->
      -- Bare `GOTO ^ROUTINE` (entry at line 0 of a whole separate
      -- routine file, handbook §5.3) is not modeled by this rewrite --
      -- there is no concept of a routine's own line 0 distinct from
      -- its tags' individually-catalogued chunks (see 'invokeTarget'
      -- and 'gotoOtherNamespace' below). Same treatment 'invokeTarget'
      -- already gives this shape for DO/CALL.
      throwError (UndefinedRoutine routine)
    (Nothing, Nothing) ->
      throwError (SyntaxError "GOTO requires a target")
    (Just tag, Nothing) ->
      gotoSameRoutine tag Nothing env
    (Just tag, Just routine)
      | Just routine == envCurrentRoutine env ->
          -- Explicitly qualified, but it names the routine/namespace
          -- already running (e.g. `GOTO TAG^MATH` written from inside
          -- MATH itself) -- same as the bare-tag case above.
          gotoSameRoutine tag (Just routine) env
      | otherwise ->
          gotoOtherNamespace tag routine env

evalStmt (SUnsupported token fullText) _ _ = do
  env <- getEnv
  let tokU = T.toUpper token
      fullU = T.toUpper fullText
  if not (envFileIOEnabled env) && (tokU == "BLOBSAVE" || tokU == "$BLOBWRITE" || "BLOBSAVE" `T.isInfixOf` fullU || "$BLOBWRITE" `T.isInfixOf` fullU)
    then throwError FileAccessDisabled
    else throwError (NotImplemented token fullText)

-- | Invoke a function or routine, returning its returned values.
invokeTarget :: CallTarget -> Eval [PValue]
invokeTarget (CallTarget mTag mRoutine mArgs) = do
  env <- getEnv
  let curNS = envCurrentNamespace env
  (params, code, tagText, rText) <- case (mTag, mRoutine) of
    (Just tag, Nothing) ->
      case lookupItem curNS tag env of
        Just (_kind, ps, c) -> pure (ps, c, tag, Nothing)
        Nothing             -> throwError (UndefinedLabel tag Nothing)
    (Just tag, Just routine) ->
      case lookupItem routine tag env of
        Just (_kind, ps, c) -> pure (ps, c, tag, Just routine)
        Nothing             -> throwError (UndefinedLabel tag (Just routine))
    (Nothing, Just routine) ->
      throwError (UndefinedRoutine routine)
    (Nothing, Nothing) ->
      pure ([], "", "", Nothing)

  if T.null code && null params
    then pure []
    else do
      let logicalLines = buildLogicalLines code
      astLines <- case mapM parseLogicalLine logicalLines of
        Left parseErr ->
          let targetDesc = case rText of
                             Just r  -> r <> "." <> tagText
                             Nothing -> tagText
          in throwError (SyntaxError ("Error compiling " <> targetDesc <> ": " <> T.pack (show parseErr)))
        Right ls -> pure ls

      callArgs <- case mArgs of
        Nothing -> pure []
        Just as -> pure as

      let frame = Frame (Just tagText) rText (envPC env) [] []
      modifyEnv (pushFrame frame)

      forM_ (zip params callArgs) $ \(pName, arg) -> do
        case arg of
          CallArgRef refArg -> do
            -- Handbook Part II §2: "the operand of `&` must be a bare
            -- local, never a subscripted local and never a global.
            -- Aliasing a global, or an arbitrary expression, makes no
            -- sense under this mechanism, so Phase rejects it with
            -- `<SYNTAX>`." The PARSER accepts any syntactically valid
            -- reference shape after '&' (see 'Phase.Parser.pCallArg'
            -- for why); this is where that shape is actually checked
            -- against the one form that's semantically valid.
            case refArg of
              Ref Local Nothing callerVar [] Nothing ->
                modifyEnv (aliasVar pName callerVar)
              _ ->
                throwError (SyntaxError ("reference argument &" <> formatRefForError refArg <> " must be a bare local"))
          CallArgVal expr -> do
            val <- evalExpr expr
            modifyEnv (newVar pName)
            modifyEnvE (writeRef Local pName [] val)

      let oldRoutine = envCurrentRoutine env
      let oldRoutines = envRoutines env
      modifyEnv (\e -> e
        { envCurrentRoutine = rText
        , envRoutines = Map.insert rText astLines (envRoutines e)
          -- The callee has its own CONDITION declarations, and is not
          -- lexically inside the caller's HANDLE clause, so a RETRY in
          -- it must not reach back into the caller's handled block.
        , envConditionDecls = collectConditionDecls astLines
        , envInHandle = False
        })

      retVals <- evalRoutineWithReturn astLines

      modifyEnv (\e ->
        let (_, e') = popFrame e
        in e' { envCurrentRoutine = oldRoutine, envRoutines = oldRoutines
              , envConditionDecls = envConditionDecls env, envInHandle = envInHandle env })
      pure retVals

-- --------------------------------------------------------------------
-- Conditions: handled blocks (Part III §1)
-- --------------------------------------------------------------------

-- | The @CONDITION@ declarations of a routine, by upper-cased name.
-- Collected from the whole routine when it starts, wherever in it they
-- appear, as the handbook's compile-time registry would.
collectConditionDecls :: [Line] -> Map.Map Text [Text]
collectConditionDecls ls =
  Map.fromListWith (++)
    [ (n, fs) | Line _ _ ps <- ls, PredStmt _ (SCondition n fs) <- ps ]

-- | Is this line the head of a @HANDLE@ or @CATCH@ clause?
isHandlerClauseLine :: Line -> Bool
isHandlerClauseLine l = case lineStmts l of
  (PredStmt _ (SHandle _ _) : _) -> True
  (PredStmt _ (SCatch _ _) : _)  -> True
  _                              -> False

-- | Does a block's line list contain a clause of its own (as opposed to
-- one belonging to a block nested deeper inside it)? Only then does the
-- handler machinery activate; every other block stays an ordinary block
-- (Part III §1).
hasHandlerClause :: [Line] -> Bool
hasHandlerClause [] = False
hasHandlerClause blockLines@(firstL : _) =
  any (\l -> lineDepth l == lineDepth firstL && isHandlerClauseLine l) blockLines

data ClauseKind = ClauseHandle | ClauseCatch
  deriving (Eq, Show)

-- | One @HANDLE@/@CATCH@ clause of a block: which kind, the condition
-- it names, the optional @AS@ binding, and its body as a line list (the
-- rest of the head's own line, if any, then the lines up to the next
-- clause).
data Clause = Clause
  { clKind    :: ClauseKind
  , clName    :: Text
  , clBinding :: Maybe Text
  , clBody    :: [Line]
  }

-- | Split the part of a block from its first clause head onward into
-- clauses. @depth@ is the block's own depth: only a clause head AT that
-- depth starts a clause, so heads of blocks nested inside a clause body
-- stay part of that body.
buildClauses :: Int -> [Line] -> [Clause]
buildClauses _ [] = []
buildClauses depth (marker : rest) =
  let (bodyRest, next) = break (\l -> lineDepth l == depth && isHandlerClauseLine l) rest
  in case lineStmts marker of
       (PredStmt _ st : trailing) ->
         let (kind, name, binding) = case st of
               SHandle n b -> (ClauseHandle, n, b)
               SCatch n b  -> (ClauseCatch, n, b)
               _           -> (ClauseCatch, "", Nothing)   -- unreachable: only clause heads get here
             headRest = [Line (lineDepth marker) Nothing trailing | not (null trailing)]
         in Clause kind name binding (headRest ++ bodyRest) : buildClauses depth next
       [] -> buildClauses depth rest

-- | The clause that handles a condition: the first one that names it,
-- else the first @<ANYTHING>@. A specific clause therefore wins over a
-- catch-all whichever was written first (Part III §1).
pickClause :: Text -> [Clause] -> Maybe Clause
pickClause name cs =
  case filter ((== name) . clName) cs of
    (c : _) -> Just c
    []      -> case filter ((== "ANYTHING") . clName) cs of
                 (c : _) -> Just c
                 []      -> Nothing

-- | Which errors a handled block turns into a condition. A @SIGNAL@
-- already is one. An undefined variable is the built-in @<UNDEFINED>@;
-- being converted HERE, in the guard, is what makes it happen "only when
-- a handler is present" (Part III §1.1): with no handled block active no
-- guard exists, and the plain error propagates as before. Divide-by-zero
-- and @LOCK@ failure need operands the guard cannot recover, so those
-- are signalled at their own sites ('divisionError', @LOCK@).
asCondition :: EvalError -> Maybe Condition
asCondition (Signaled c) = Just c
asCondition (UndefinedVariable name subs) =
  Just (Condition "UNDEFINED"
         [ ("name", mkValue name)
         , ("path", mkValue (T.intercalate "," (map unPValue subs))) ])
asCondition _ = Nothing

-- | Run one top-level line of a handled block, catching a condition.
-- On a catch the environment is the one from BEFORE the line, so
-- everything earlier lines did survives and everything this line did
-- (including output it wrote and database writes it had pending) is
-- dropped, and the line returns @QuitFrame@ so the block's own line
-- loop stops; the condition is left in 'envCaught' for the runner. That
-- the whole 'Env' is a pure value is what makes this a cheap, complete
-- rollback.
guardLine :: Eval Flow -> Eval Flow
guardLine act = Eval $ \env -> do
  r <- unEval act env
  pure $ case r of
    Left err | Just c <- asCondition err -> Right (QuitFrame Nothing, env { envCaught = Just c })
    other                                -> other

-- | Divide-by-zero. With a handler present it is the built-in
-- @<DIVIDE-BY-ZERO>@ condition with its operands; otherwise the plain
-- error (Part III §1.1).
divisionError :: EvalError -> PValue -> PValue -> Eval a
divisionError err dividend divisor = do
  depth <- envHandlerDepth <$> getEnv
  if depth > 0 && err == DivideByZero
    then throwError (Signaled (Condition "DIVIDE-BY-ZERO" [("dividend", dividend), ("divisor", divisor)]))
    else throwError err

-- | Run a block that has @HANDLE@/@CATCH@ clauses.
--
-- The body (every line before the first clause head) runs line by line
-- under 'guardLine'. If a condition is caught, the clause that handles
-- it runs once, outside the block's own handler scope (a signal raised
-- by a clause goes to the OUTER handlers), with the condition's fields
-- bound as locals under its @AS@ name. A @CATCH@ clause then ends the
-- block. A @HANDLE@ clause may @RETRY@, which re-runs the whole body
-- from its top on the environment the clause left behind, with the
-- @WITH@ fields set as locals; earlier lines' effects are NOT undone
-- (a @TRANSACTION@ block will add that). A condition no clause names
-- goes on outward as an ordinary signal.
runHandledBlock :: [Line] -> Eval Flow
runHandledBlock [] = pure NextStmt
runHandledBlock blockLines@(firstL : _) = do
  env0 <- getEnv
  let depth       = lineDepth firstL
      isHead l    = lineDepth l == depth && isHandlerClauseLine l
      (bodyLines, clauseLines) = break isHead blockLines
      clauses     = buildClauses depth clauseLines
      rName       = envCurrentRoutine env0
      saved       = env0
      -- Put back everything a pass of the body or a clause changes
      -- about where and how we are running.
      restore = modifyEnv (\e -> e
        { envPC = envPC saved, envRoutines = envRoutines saved
        , envHandlerDepth = envHandlerDepth saved, envInHandle = envInHandle saved
        , envRetryCount = envRetryCount saved })
      install ls extra = modifyEnv (\e -> extra (e { envRoutines = Map.insert rName ls (envRoutines e) }))

      attempt n seeds = do
        forM_ seeds $ \(k, v) -> modifyEnvE (writeRef Local k [] v)
        install bodyLines (\e -> e
          { envHandlerDepth = envHandlerDepth saved + 1, envInHandle = False
          , envRetryCount = n, envCaught = Nothing, envRetry = Nothing })
        _ <- evalRoutineWith True bodyLines
        caught <- envCaught <$> getEnv
        modifyEnv (\e -> e { envCaught = Nothing })
        restore
        case caught of
          Nothing -> pure NextStmt
          Just c  -> case pickClause (condName c) clauses of
            Nothing -> throwError (Signaled c)
            Just cl -> do
              bindCondition cl c
              install (clBody cl) (\e -> e
                { envInHandle = clKind cl == ClauseHandle, envRetryCount = n, envRetry = Nothing })
              _ <- evalRoutine (clBody cl)
              requested <- envRetry <$> getEnv
              modifyEnv (\e -> e { envRetry = Nothing })
              restore
              case requested of
                Nothing     -> pure NextStmt
                Just seeds' -> attempt (n + 1) seeds'

  attempt (0 :: Int) []

-- | Bind a caught condition's fields as ordinary subscripted locals
-- under the clause's @AS@ name: @c("field")@, plus the automatic
-- @c("message")@. A @message@ field the signaller supplied itself takes
-- precedence over the generated one. Any earlier binding of the same
-- name is cleared first, so a stale field from a previous condition
-- cannot be read as part of this one.
bindCondition :: Clause -> Condition -> Eval ()
bindCondition cl c =
  case clBinding cl of
    Nothing -> pure ()
    Just b  -> do
      modifyEnv (\e -> either (const e) id (killRef Local b [] e))
      modifyEnvE (writeRef Local b [mkValue "message"] (mkValue (conditionMessage c)))
      forM_ (condFields c) $ \(f, v) ->
        modifyEnvE (writeRef Local b [mkValue f] v)

-- | Run an @EVALUATE@ (Part II §5), in place.
--
-- Unlike 'runDoBlock', this does NOT swap in a sub-list of lines: the
-- clauses are looked up in the routine's own line list, as the lines
-- that follow the header and are exactly one level deeper
-- ('Phase.Lines' gives them that depth in both the dotted and the
-- @END EVALUATE@ spelling). Running in place is what makes
-- @EVALUATE@ not a scope: a @QUIT@, @RETURN@ or @GOTO@ inside a
-- clause body leaves the enclosing routine, because its 'Flow'
-- comes straight back out of here. (A clause body that opens its own
-- dotted block with a trailing @DO@ still gets ordinary @DO@ block
-- semantics for that inner block only.)
--
-- Semantics, in order:
--
--   1. Every subject is evaluated exactly once, left to right.
--   2. Clauses are tried top to bottom; the first whose tests ALL hold
--      (and whose @WHERE@ guard, if any, is true) runs, and no other
--      clause runs: there is no fall-through. @WHEN OTHER@ always
--      holds, so it only ever wins when nothing above it did.
--   3. A clause's tests are checked left to right and stop at the
--      first failure, and the guard is only evaluated once every test
--      has held.
--   4. No matching clause and no @WHEN OTHER@ is not an error.
--
-- A clause with a different number of tests than the header has
-- subjects is a @<SYNTAX>@ error when it is reached.
runEvaluate :: [EvalSubject] -> Eval Flow
runEvaluate subjects = do
  env <- getEnv
  let (curLineIdx, _) = envPC env
  case Map.lookup (envCurrentRoutine env) (envRoutines env) of
    Nothing -> pure NextStmt
    Just linesList
      | curLineIdx >= length linesList -> pure NextStmt
      | otherwise -> do
          let curDepth = lineDepth (linesList !! curLineIdx)
              following = zip [curLineIdx + 1 ..] (drop (curLineIdx + 1) linesList)
              block = takeWhile (\(_, l) -> lineDepth l > curDepth) following
              clauses = [ (i, l) | (i, l) <- block, lineDepth l == curDepth + 1, isClauseLine l ]
          subjectVals <- mapM evalSubject subjects
          chosen <- firstMatching subjectVals clauses
          case chosen of
            Nothing -> pure NextStmt
            Just (idx, clauseLine) -> do
              let oldPC = envPC env
              -- The clause's own line becomes the current line, so a
              -- trailing bare @DO@ finds the dotted body under it.
              modifyEnv (\e -> e { envPC = (idx, 1) })
              flow <- evalStmtSequence (drop 1 (lineStmts clauseLine))
              modifyEnv (\e -> e { envPC = oldPC })
              pure $ case flow of
                ExitLine   -> NextStmt   -- only the clause's own line ended
                SkipToElse -> NextStmt
                other      -> other      -- JumpTo / QuitFrame leave the routine
  where
    isClauseLine l = case lineStmts l of
      (PredStmt _ (SWhen _ _) : _) -> True
      (PredStmt _ SWhenOther : _)  -> True
      _                            -> False

    -- The value the subject has for the rest of the EVALUATE, and
    -- whether it is the condition-chain @TRUE@ subject.
    evalSubject SubjectTrue      = pure (True, mkValue "1")
    evalSubject (SubjectExpr e)  = (,) False <$> evalExpr e

    firstMatching _ [] = pure Nothing
    firstMatching vals ((i, l) : rest) = do
      ok <- clauseHolds vals (psStmt (head (lineStmts l)))
      if ok then pure (Just (i, l)) else firstMatching vals rest

    clauseHolds _ SWhenOther = pure True
    clauseHolds vals (SWhen tests mGuard)
      | length tests /= length vals =
          throwError (SyntaxError
            ("WHEN has " <> T.pack (show (length tests)) <> " test(s) but its EVALUATE has "
              <> T.pack (show (length vals)) <> " subject(s)"))
      | otherwise = do
          testsOk <- allM (uncurry testHolds) (zip vals tests)
          if not testsOk
            then pure False
            else case mGuard of
              Nothing -> pure True
              Just g  -> isTruthy <$> evalExpr g
    clauseHolds _ _ = pure False

    -- One position's test against that position's subject.
    testHolds _ AnyTest = pure True
    testHolds (isChain, subj) (ValueTest e)
      | isChain   = isTruthy <$> evalExpr e                   -- EVALUATE TRUE: a boolean per clause
      | otherwise = isTruthy <$> evalExpr (BinExpr Eq (Lit subj) e)
    testHolds (_, subj) (CompareTest op e) = isTruthy <$> evalExpr (BinExpr op (Lit subj) e)

    allM _ [] = pure True
    allM f (x : xs) = do
      b <- f x
      if b then allM f xs else pure False

-- | Run the dotted block that follows the current line: every
-- following line whose dot depth is strictly greater than the current
-- line's. Shared by bare @DO@ and @DO WITH @TX(...)@, which differ only
-- in what surrounds the block, not in how it is found.
--
-- We look up the current routine's line list and use envPC to find
-- which lines form the block. IMPORTANT: before running the block, we
-- must re-register the current routine name in envRoutines to point at
-- blockLines rather than the full routine. Any nested bare DO inside
-- the block will look up the same routine name and use envPC (which
-- evalRoutine resets to 0-based within blockLines) to find ITS own
-- sub-block. Without this, a nested DO reads envPC=0 against the full
-- routine list, lands on the wrong line, and picks up the same block
-- repeatedly, causing an infinite loop.
runDoBlock :: Eval Flow
runDoBlock = do
  env <- getEnv
  let (curLineIdx, _) = envPC env
  case Map.lookup (envCurrentRoutine env) (envRoutines env) of
    Nothing -> pure NextStmt
    Just linesList ->
      if curLineIdx >= length linesList
        then pure NextStmt
        else do
          let curDepth = lineDepth (linesList !! curLineIdx)
              following = drop (curLineIdx + 1) linesList
              blockLines = takeWhile (\l -> lineDepth l > curDepth) following
          if null blockLines
            then pure NextStmt
            else if hasHandlerClause blockLines
              then runHandledBlock blockLines
              else do
                let oldPC       = envPC env
                    oldRoutines = envRoutines env
                    rName       = envCurrentRoutine env
                modifyEnv (\e -> e { envRoutines = Map.insert rName blockLines (envRoutines e) })
                evalRoutine blockLines
                modifyEnv (\e -> e { envPC = oldPC, envRoutines = oldRoutines })
                -- A RETRY inside this block is unwinding to the handled
                -- block it belongs to: keep going out instead of
                -- carrying on with the lines after this one.
                pending <- envRetry <$> getEnv
                pure (maybe NextStmt (const (QuitFrame Nothing)) pending)

-- | Run an action inside a @DO WITH@ / @CALL WITH@ pin scope: resolve
-- the pin ('resolvePinExpr') and the bind list, install the scope
-- ('Phase.State.enterPinScope', which also enforces that a nested
-- bind list is a subset of the enclosing one), run the action, then
-- restore the enclosing scope.
--
-- The pin argument is resolved THROUGH the enclosing scope (a nested
-- block's own @TX(...) expression reads like any other bare reference
-- inside the outer block), and so are the bind names' qualified names.
-- Restoring on the normal path is enough: an error anywhere inside
-- discards the whole 'Env' (see 'Eval'), so a scope cannot outlive a
-- failed block.
withPinScope :: PinSpec -> Eval a -> Eval a
withPinScope (PinSpec pinExpr mBind) body = do
  newPin <- resolvePinExpr pinExpr
  env0 <- getEnv
  let bound = fmap (Set.fromList . map (\(BindName mNS n) -> qualifyGlobalName mNS n)) mBind
  case enterPinScope newPin bound env0 of
    Left err -> throwError err
    Right env1 -> do
      modifyEnv (\e -> e { envPinnedTx     = envPinnedTx env1
                          , envPinOverrides = envPinOverrides env1
                          , envBindScope    = envBindScope env1 })
      result <- body
      modifyEnv (\e -> e { envPinnedTx     = envPinnedTx env0
                          , envPinOverrides = envPinOverrides env0
                          , envBindScope    = envBindScope env0 })
      pure result

-- | Resolve the argument of @DO WITH @TX(...)@ to the transaction the
-- block should be pinned to ('Nothing' = live, i.e. no pin).
--
--   * @LAST@, unpin: the escape hatch back to live data, for the
--     whole block or just one call made inside another pinned block.
--   * A global reference, e.g. @^[-2]FOO("BAR")@, pin to the
--     transaction of THAT revision of THAT path. With no bracket of its
--     own it means the revision in effect right now (live, or the
--     enclosing pin's, if there is one).
--   * Anything else, an absolute, non-negative transaction number.
--     A bare negative number has no path to count back along, so it is
--     an error rather than a guess.
--
-- Evaluated through any enclosing pin: the subscripts are read like
-- any other bare reference inside the outer block, and an unbracketed
-- reference means the revision in effect AT the outer pin.
resolvePinExpr :: Expr -> Eval (Maybe TxId)
resolvePinExpr TimeTravelLast = pure Nothing
resolvePinExpr (Var (Ref Global mNS name subExprs mTT)) = do
  subs <- mapM evalExpr subExprs
  let targetName = qualifyGlobalName mNS name
  case mTT of
    Just ttExpr -> Just <$> resolveTimeTravelExpr targetName subs ttExpr
    Nothing -> do
      env <- getEnv
      pure (Just (resolveRelativeTx False targetName subs 0 env))
resolvePinExpr expr = do
  val <- evalExpr expr
  let num = round (Sci.toRealFloat (toNumber val) :: Double) :: Integer
  if num < 0
    then throwError (SyntaxError "@TX(...) takes LAST, a global reference, or a transaction number >= 0")
    else pure (Just (fromIntegral num))

-- | Same-routine 'GOTO': a genuine program-counter move within the
-- line list that is CURRENTLY executing (handbook §5.1: "GOTO and DO
-- TAG resolve to a line index, not to a nested scope") -- no frame is
-- pushed, and falling through past whatever tag textually follows the
-- target works exactly as GOTO requires, because the target is found
-- inside the running 'envRoutines' entry itself rather than fetched
-- as an isolated Catalog chunk the way a cross-namespace GOTO must be
-- (see 'gotoOtherNamespace').
gotoSameRoutine :: Text -> Maybe Text -> Env -> Eval Flow
gotoSameRoutine tag mRoutine env =
  case Map.lookup (envCurrentRoutine env) (envRoutines env) of
    Nothing -> throwError (UndefinedLabel tag mRoutine)
    Just linesList ->
      case findTagIndex tag linesList of
        Just idx -> pure (JumpTo (idx, 0))
        Nothing  -> throwError (UndefinedLabel tag mRoutine)
  where
    findTagIndex t = go 0
      where
        go _ [] = Nothing
        go idx (l : ls)
          | lineTag l == Just t = Just idx
          | otherwise           = go (idx + 1) ls

-- | Cross-namespace 'GOTO TAG^NAMESPACE': transfers permanently into
-- NAMESPACE's own copy of TAG, fetched the same way 'invokeTarget'
-- fetches a cross-namespace CALL/DO target. Unlike 'gotoSameRoutine',
-- this CANNOT fall through into whatever tag textually follows TAG
-- inside NAMESPACE's own source: 'Phase.Catalog' stores each tag's
-- body as its own self-contained chunk, up to the next column-0 tag
-- in whatever source originally defined it (registerRoutineTags), not
-- as one flat, indexable file the way the handbook's "line index"
-- model assumes. So this runs exactly that one chunk -- the same
-- granularity a cross-namespace CALL/DO already gives -- and then the
-- GOTO is done. No frame is pushed (a real GOTO never returns to the
-- statement after it), so that chunk's own QUIT/RETURN (or simply
-- falling off its end) also ends the CURRENT routine's own execution,
-- rather than resuming whatever line followed this GOTO.
gotoOtherNamespace :: Text -> Text -> Env -> Eval Flow
gotoOtherNamespace tag routine env =
  case lookupItem routine tag env of
    Nothing -> throwError (UndefinedLabel tag (Just routine))
    Just (_kind, _params, code) -> do
      let logicalLines = buildLogicalLines code
      astLines <- case mapM parseLogicalLine logicalLines of
        Left parseErr ->
          throwError (SyntaxError ("Error compiling " <> routine <> "." <> tag <> ": " <> T.pack (show parseErr)))
        Right ls -> pure ls
      let oldRoutine  = envCurrentRoutine env
          oldRoutines = envRoutines env
      modifyEnv (\e -> e
        { envCurrentRoutine = Just routine
        , envRoutines = Map.insert (Just routine) astLines (envRoutines e)
        })
      _ <- evalRoutineWithReturn astLines
      -- Defensive/inert: the unconditional 'QuitFrame' below ends this
      -- call chain's own stepping loop, so nothing in THIS routine
      -- reads envCurrentRoutine/envRoutines again afterward -- but
      -- this keeps Env consistent for whatever unwinds next, matching
      -- 'invokeTarget''s own bracketing style.
      modifyEnv (\e -> e { envCurrentRoutine = oldRoutine, envRoutines = oldRoutines })
      pure (QuitFrame Nothing)

-- --------------------------------------------------------------------
-- Line & Routine Stepping
-- --------------------------------------------------------------------

-- | Evaluate a sequence of statements belonging to the current logical line.
evalStmtSequence :: [PredStmt] -> Eval Flow
evalStmtSequence stmts = go 0
  where
    go idx
      | idx >= length stmts = pure NextStmt
      | otherwise = do
          let predStmt = stmts !! idx
          flow <- evalPredStmt predStmt stmts idx
          case flow of
            NextStmt -> go (idx + 1)
            SkipToElse ->
              -- Skip forward on this line until the next SElse statement
              case findNextElse (idx + 1) of
                Nothing       -> pure ExitLine
                Just elseIdx  -> go elseIdx
            ExitLine -> pure ExitLine
            JumpTo target -> pure (JumpTo target)
            QuitFrame v -> pure (QuitFrame v)

    findNextElse i
      | i >= length stmts = Nothing
      | SElse <- psStmt (stmts !! i) = Just i
      | otherwise = findNextElse (i + 1)

-- | Evaluate one logical line.
evalLine :: Line -> Eval Flow
evalLine (Line _ _ stmts) = evalStmtSequence stmts

-- | Evaluate a whole routine (list of lines), returning values if returned.
evalRoutineWithReturn :: [Line] -> Eval [PValue]
evalRoutineWithReturn = evalRoutineWith False

-- | The line loop behind 'evalRoutineWithReturn'. With @guarded@ set,
-- each TOP-LEVEL line of the list (a line together with the deeper lines
-- of any block it opens counts as one) runs under 'guardLine', which is
-- how a handled block ('runHandledBlock') catches signals. Nested
-- blocks inside a line are run unguarded by the ordinary loop: the
-- whole outer line is the unit that gets rolled back.
evalRoutineWith :: Bool -> [Line] -> Eval [PValue]
evalRoutineWith guarded linesList = go 0
  where
    totalLines = length linesList
    baseDepth = case linesList of
                  (firstL : _) -> lineDepth firstL
                  []           -> 0

    go lineIdx
      | lineIdx >= totalLines = pure []
      | otherwise = do
          let curLine = linesList !! lineIdx
          -- Skip indented block lines at this outer level (they are executed by bare DO)
          if lineIdx > 0 && lineDepth curLine > baseDepth
            then go (lineIdx + 1)
            else do
              modifyEnv (\e -> e { envPC = (lineIdx, 0) })
              flow <- (if guarded then guardLine else id) (evalLine curLine)
              case flow of
                QuitFrame (Just vs) -> pure vs
                QuitFrame Nothing   -> pure []
                JumpTo (l, _)       -> go l
                _                   -> go (lineIdx + 1)

-- | Evaluate a whole routine (list of lines).
evalRoutine :: [Line] -> Eval ()
evalRoutine linesList = do
  _ <- evalRoutineWithReturn linesList
  pure ()

-- | Helper to run a whole routine starting from an initial environment.
runRoutine :: [Line] -> Env -> IO (Either EvalError Env)
runRoutine routineLines env =
  let envWithRoutine = env { envRoutines = Map.insert (envCurrentRoutine env) routineLines (envRoutines env)
                           , envConditionDecls = collectConditionDecls routineLines }
  in execState (evalRoutine routineLines) envWithRoutine
