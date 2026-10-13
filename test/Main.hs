{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure, exitSuccess)

import qualified Data.Text as T
import Data.Time (getCurrentTime)
import System.Directory (doesFileExist, getFileSize, getTemporaryDirectory, removeFile)
import System.FilePath ((</>))

import Phase.AST
import Phase.Auth (Role (..), bootstrapCreateUser)
import Phase.Catalog
import Phase.Eval
import Phase.Lines
import Phase.Parser
import Phase.State
import Phase.Stmt
import Phase.Storage
import Phase.Storage.Lock (acquireLocks, emptyLockTable)
import Phase.Tree
import Phase.Value

-- | A path for a scratch file in the platform's temp directory (not a
-- hard-coded @/tmp@, which does not exist on Windows).
tmpPath :: FilePath -> IO FilePath
tmpPath name = (</> name) <$> getTemporaryDirectory

-- --------------------------------------------------------------------
-- Simple Test Runner Framework
-- --------------------------------------------------------------------

data TestContext = TestContext
  { tcPassed :: IORef Int
  , tcFailed :: IORef Int
  }

newTestContext :: IO TestContext
newTestContext = do
  p <- newIORef 0
  f <- newIORef 0
  pure (TestContext p f)

runAssert :: TestContext -> String -> Bool -> String -> IO ()
runAssert ctx name cond failMsg = do
  if cond
    then do
      modifyIORef' (tcPassed ctx) (+ 1)
      putStrLn $ "  \ESC[32m[PASS]\ESC[0m " ++ name
    else do
      modifyIORef' (tcFailed ctx) (+ 1)
      putStrLn $ "  \ESC[31m[FAIL]\ESC[0m " ++ name ++ "\n         " ++ failMsg

assertEqual :: (Show a, Eq a) => TestContext -> String -> a -> a -> IO ()
assertEqual ctx name actual expected =
  runAssert ctx name (actual == expected) $
    "Expected: " ++ show expected ++ "\n         Got:      " ++ show actual

-- | Like 'assertEqual', but the actual value is itself an 'IO' action
-- rather than a pure value, added when 'Phase.Eval''s core monad
-- gained real 'IO' access (deep time-travel replay needs to re-read
-- the @.db@ file, which a pure function can't do). Lets call sites
-- that used to read @assertEqual ctx name (pureExpr) expected@ become
-- @assertEqualM ctx name (ioAction) expected@ with a minimal textual
-- change, rather than needing to bind every one of them to a named
-- variable first.
assertEqualM :: (Show a, Eq a) => TestContext -> String -> IO a -> a -> IO ()
assertEqualM ctx name actualIO expected = do
  actual <- actualIO
  assertEqual ctx name actual expected

assertIsLeft :: (Show a, Show e) => TestContext -> String -> Either e a -> IO ()
assertIsLeft ctx name result =
  case result of
    Left _  -> runAssert ctx name True ""
    Right v -> runAssert ctx name False ("Expected Left (parse error), but got Right: " ++ show v)

assertIsRight :: (Show a, Show e, Eq a) => TestContext -> String -> Either e a -> a -> IO ()
assertIsRight ctx name result expected =
  case result of
    Left err -> runAssert ctx name False ("Expected Right " ++ show expected ++ ", but got Left: " ++ show err)
    Right actual -> runAssert ctx name (actual == expected) $
      "Expected: " ++ show expected ++ "\n         Got:      " ++ show actual

runGroup :: TestContext -> String -> (TestContext -> IO ()) -> IO ()
runGroup ctx groupName tests = do
  putStrLn $ "\n\ESC[1m=== " ++ groupName ++ " ===\ESC[0m"
  tests ctx

-- | An 'Env' with a real, ordinary admin account already created,
-- via the SAME 'bootstrapCreateUser' mechanism the REPL's own
-- bootstrap flow uses, not a separate test-only shortcut, and
-- 'envCurrentUser' set to that account. Used throughout this test
-- suite in place of bare 'initEnv' wherever a test exercises global
-- reads/writes, now that those go through the team's permission checks:
-- 'initEnv' on its own has no user with any role at all, so every
-- global access would otherwise be denied.
--
-- Every test using this trusts the SAME code path production actually
-- runs (an admin account existing in @^_USERS@, checked by
-- 'Phase.Auth.checkReadAccess'/'checkWriteAccess'), rather than a
-- parallel "tests always succeed" bypass that could silently diverge
-- from real enforcement.
testAdminEnv :: IO Env
testAdminEnv = do
  now <- getCurrentTime
  result <- bootstrapCreateUser "TESTADMIN" "testpassword123" RAdmin now initStorageEngine
  case result of
    Left err -> error ("testAdminEnv: failed to bootstrap test admin: " ++ T.unpack err)
    Right storage -> pure initEnv { envStorage = storage, envGlobals = seLive storage, envCurrentUser = "TESTADMIN", envNow = now }

-- --------------------------------------------------------------------
-- Test Groups
-- --------------------------------------------------------------------

testValueGroup :: TestContext -> IO ()
testValueGroup ctx = do
  -- 1. Numeric coercion (toNumber)
  assertEqual ctx "toNumber '12abc' -> 12" (toNumber (mkValue "12abc")) 12
  assertEqual ctx "toNumber 'abc' -> 0" (toNumber (mkValue "abc")) 0
  assertEqual ctx "toNumber '  12' -> 0 (leading space not skipped)" (toNumber (mkValue "  12")) 0
  assertEqual ctx "toNumber '+42' -> 42" (toNumber (mkValue "+42")) 42
  assertEqual ctx "toNumber '-42' -> -42" (toNumber (mkValue "-42")) (-42)
  assertEqual ctx "toNumber '-42.75abc' -> -42.75" (toNumber (mkValue "-42.75abc")) (-42.75)
  assertEqual ctx "toNumber '' -> 0" (toNumber (mkValue "")) 0
  assertEqual ctx "toNumber '.' -> 0" (toNumber (mkValue ".")) 0
  assertEqual ctx "toNumber '.5' -> 0.5" (toNumber (mkValue ".5")) 0.5
  assertEqual ctx "toNumber '0.5' -> 0.5" (toNumber (mkValue "0.5")) 0.5

  -- 2. Truthiness (isTruthy)
  assertEqual ctx "isTruthy '1' -> True" (isTruthy (mkValue "1")) True
  assertEqual ctx "isTruthy '-5' -> True" (isTruthy (mkValue "-5")) True
  assertEqual ctx "isTruthy '0.1' -> True" (isTruthy (mkValue "0.1")) True
  assertEqual ctx "isTruthy '0' -> False" (isTruthy (mkValue "0")) False
  assertEqual ctx "isTruthy 'abc' -> False" (isTruthy (mkValue "abc")) False
  assertEqual ctx "isTruthy '' -> False" (isTruthy (mkValue "")) False
  assertEqual ctx "isTruthy '  12' -> False" (isTruthy (mkValue "  12")) False

  -- 3. Canonical numbers
  assertEqual ctx "canonicalNumber '1' is canonical" (canonicalNumber (mkValue "1") /= Nothing) True
  assertEqual ctx "canonicalNumber '23' is canonical" (canonicalNumber (mkValue "23") /= Nothing) True
  assertEqual ctx "canonicalNumber '-4' is canonical" (canonicalNumber (mkValue "-4") /= Nothing) True
  assertEqual ctx "canonicalNumber '1.5' is canonical" (canonicalNumber (mkValue "1.5") /= Nothing) True
  assertEqual ctx "canonicalNumber '0' is canonical" (canonicalNumber (mkValue "0") /= Nothing) True
  assertEqual ctx "canonicalNumber '-0.5' is canonical" (canonicalNumber (mkValue "-0.5") /= Nothing) True

  assertEqual ctx "canonicalNumber '01' not canonical" (canonicalNumber (mkValue "01")) Nothing
  assertEqual ctx "canonicalNumber '+1' not canonical" (canonicalNumber (mkValue "+1")) Nothing
  assertEqual ctx "canonicalNumber '1.50' not canonical" (canonicalNumber (mkValue "1.50")) Nothing
  assertEqual ctx "canonicalNumber '1.0' not canonical" (canonicalNumber (mkValue "1.0")) Nothing
  assertEqual ctx "canonicalNumber '1E2' not canonical" (canonicalNumber (mkValue "1E2")) Nothing
  assertEqual ctx "canonicalNumber '' not canonical" (canonicalNumber (mkValue "")) Nothing
  assertEqual ctx "canonicalNumber '1.' not canonical" (canonicalNumber (mkValue "1.")) Nothing
  assertEqual ctx "canonicalNumber '-01' not canonical" (canonicalNumber (mkValue "-01")) Nothing

  -- 4. Equality (=)
  assertEqual ctx "phaseEq 1 1 == True" (phaseEq (mkValue "1") (mkValue "1")) True
  assertEqual ctx "phaseEq 1 1.0 == False (1.0 not canonical)" (phaseEq (mkValue "1") (mkValue "1.0")) False
  assertEqual ctx "phaseEq '01' '1' == False (01 not canonical)" (phaseEq (mkValue "01") (mkValue "1")) False
  assertEqual ctx "phaseEq '-4' '-4' == True" (phaseEq (mkValue "-4") (mkValue "-4")) True
  assertEqual ctx "phaseEq '-4' '-4.0' == False" (phaseEq (mkValue "-4") (mkValue "-4.0")) False
  assertEqual ctx "phaseEq 'abc' 'abc' == True" (phaseEq (mkValue "abc") (mkValue "abc")) True
  assertEqual ctx "phaseEq 'abc' 'xyz' == False" (phaseEq (mkValue "abc") (mkValue "xyz")) False

  -- 5. Collation ordering
  assertEqual ctx "collation 1 < 1.0 (number before string)" (collationCompare (mkValue "1") (mkValue "1.0")) LT
  assertEqual ctx "collation 100 < 'A' (number before string)" (collationCompare (mkValue "100") (mkValue "A")) LT
  assertEqual ctx "collation -5 < '' (number before string)" (collationCompare (mkValue "-5") (mkValue "")) LT
  assertEqual ctx "collation -10 < -2 (numeric order)" (collationCompare (mkValue "-10") (mkValue "-2")) LT
  assertEqual ctx "collation -2 < 0 (numeric order)" (collationCompare (mkValue "-2") (mkValue "0")) LT
  assertEqual ctx "collation 0 < 1 (numeric order)" (collationCompare (mkValue "0") (mkValue "1")) LT
  assertEqual ctx "collation 1 < 2 (numeric order)" (collationCompare (mkValue "1") (mkValue "2")) LT
  assertEqual ctx "collation 2 < 10 (numeric order)" (collationCompare (mkValue "2") (mkValue "10")) LT
  assertEqual ctx "collation 'A' < 'B' (string order)" (collationCompare (mkValue "A") (mkValue "B")) LT
  assertEqual ctx "collation '1.0' < '1.00' (string order)" (collationCompare (mkValue "1.0") (mkValue "1.00")) LT

testParserGroup :: TestContext -> IO ()
testParserGroup ctx = do
  -- 1. Precedence ladder
  assertIsRight ctx "parseExpr '1+2*3' -> 1+(2*3)"
    (parseExpr "1+2*3")
    (BinExpr Add (Lit (mkValue "1")) (BinExpr Mul (Lit (mkValue "2")) (Lit (mkValue "3"))))

  assertIsRight ctx "parseExpr '2**3**2' -> 2**(3**2) (right assoc)"
    (parseExpr "2**3**2")
    (BinExpr Pow (Lit (mkValue "2")) (BinExpr Pow (Lit (mkValue "3")) (Lit (mkValue "2"))))

  assertIsRight ctx "parseExpr '-2**2' -> (-2)**2 (unary binds tighter than **)"
    (parseExpr "-2**2")
    (BinExpr Pow (UnExpr Neg (Lit (mkValue "2"))) (Lit (mkValue "2")))

  assertIsRight ctx "parseExpr '1+2_3' -> (1+2)_3 (+ tighter than _)"
    (parseExpr "1+2_3")
    (BinExpr Concat (BinExpr Add (Lit (mkValue "1")) (Lit (mkValue "2"))) (Lit (mkValue "3")))

  assertIsRight ctx "parseExpr 'A_B=C_D' -> (A_B)=(C_D) (_ tighter than =)"
    (parseExpr "A_B=C_D")
    (BinExpr Eq
      (BinExpr Concat (Var (mkRef Local "A" [])) (Var (mkRef Local "B" [])))
      (BinExpr Concat (Var (mkRef Local "C" [])) (Var (mkRef Local "D" []))))

  assertIsRight ctx "parseExpr 'A=1&B=2' -> (A=1)&(B=2) (= tighter than &)"
    (parseExpr "A=1&B=2")
    (BinExpr And
      (BinExpr Eq (Var (mkRef Local "A" [])) (Lit (mkValue "1")))
      (BinExpr Eq (Var (mkRef Local "B" [])) (Lit (mkValue "2"))))

  assertIsRight ctx "parseExpr '1*2+3\\4' -> (1*2)+(3\\4)"
    (parseExpr "1*2+3\\4")
    (BinExpr Add
      (BinExpr Mul (Lit (mkValue "1")) (Lit (mkValue "2")))
      (BinExpr IntDiv (Lit (mkValue "3")) (Lit (mkValue "4"))))

  -- 2. Operators and Comparisons
  assertIsRight ctx "parseExpr 'X\'=1' -> X \'= 1"
    (parseExpr "X\'=1")
    (BinExpr NotEq (Var (mkRef Local "X" [])) (Lit (mkValue "1")))

  assertIsRight ctx "parseExpr '\'X=1' -> (\'X) = 1"
    (parseExpr "\'X=1")
    (BinExpr Eq (UnExpr Not (Var (mkRef Local "X" []))) (Lit (mkValue "1")))

  assertIsRight ctx "parseExpr 'A[B' -> A [ B"
    (parseExpr "A[B")
    (BinExpr Contains (Var (mkRef Local "A" [])) (Var (mkRef Local "B" [])))

  assertIsRight ctx "parseExpr 'A\'[B' -> A \'[ B"
    (parseExpr "A\'[B")
    (BinExpr NotContains (Var (mkRef Local "A" [])) (Var (mkRef Local "B" [])))

  assertIsRight ctx "parseExpr 'A]]B' -> A ]] B"
    (parseExpr "A]]B")
    (BinExpr SortsAfter (Var (mkRef Local "A" [])) (Var (mkRef Local "B" [])))

  assertIsRight ctx "parseExpr 'A\']]B' -> A \']] B"
    (parseExpr "A\']]B")
    (BinExpr NotSortsAfter (Var (mkRef Local "A" [])) (Var (mkRef Local "B" [])))

  -- 3. Slice syntax
  assertIsRight ctx "parseExpr 'S[1:5]'"
    (parseExpr "S[1:5]")
    (Slice (Var (mkRef Local "S" [])) (Just (Lit (mkValue "1"))) (Just (Lit (mkValue "5"))))

  assertIsRight ctx "parseExpr 'S[:5]'"
    (parseExpr "S[:5]")
    (Slice (Var (mkRef Local "S" [])) Nothing (Just (Lit (mkValue "5"))))

  assertIsRight ctx "parseExpr 'S[1:]'"
    (parseExpr "S[1:]")
    (Slice (Var (mkRef Local "S" [])) (Just (Lit (mkValue "1"))) Nothing)

  assertIsRight ctx "parseExpr 'S[:]'"
    (parseExpr "S[:]")
    (Slice (Var (mkRef Local "S" [])) Nothing Nothing)

  -- 4. References & Globals
  assertIsRight ctx "parseExpr '^ORDERS(1,\"PRICE\")'"
    (parseExpr "^ORDERS(1,\"PRICE\")")
    (Var (mkRef Global "ORDERS" [Lit (mkValue "1"), Lit (mkValue "PRICE")]))

  assertIsRight ctx "parseExpr '%TEMP(1,2)'"
    (parseExpr "%TEMP(1,2)")
    (Var (mkRef Local "%TEMP" [Lit (mkValue "1"), Lit (mkValue "2")]))

  -- 5. String escaping & Intrinsics
  assertIsRight ctx "parseExpr '\"Hello \"\"World\"\"\"' (escaped quotes)"
    (parseExpr "\"Hello \"\"World\"\"\"")
    (Lit (mkValue "Hello \"World\""))

  assertIsRight ctx "parseExpr '$DATA(X)'"
    (parseExpr "$DATA(X)")
    (FuncCall "$DATA" [Var (mkRef Local "X" [])])

  assertIsRight ctx "parseExpr '$ORDER(^A(1))'"
    (parseExpr "$ORDER(^A(1))")
    (FuncCall "$ORDER" [Var (mkRef Global "A" [Lit (mkValue "1")])])

  -- 6. No-whitespace rule inside expressions
  assertIsLeft ctx "parseExpr '1 + 2' fails due to whitespace" (parseExpr "1 + 2")
  assertIsLeft ctx "parseExpr 'X = 1' fails due to whitespace" (parseExpr "X = 1")
  assertIsLeft ctx "parseExpr '^A( 1 )' fails due to whitespace" (parseExpr "^A( 1 )")

  -- 7. Intrinsic names are case-insensitive at parse time (uppercased in FuncCall)
  assertIsRight ctx "parseExpr '$length(X)' -> FuncCall \"$LENGTH\" [Var X]"
    (parseExpr "$length(X)")
    (FuncCall "$LENGTH" [Var (mkRef Local "X" [])])
  assertIsRight ctx "parseExpr '$order(^A(1))' -> FuncCall \"$ORDER\" [Var ^A(1)]"
    (parseExpr "$order(^A(1))")
    (FuncCall "$ORDER" [Var (mkRef Global "A" [Lit (mkValue "1")])])
  assertIsRight ctx "parseExpr '$Data(X)' -> FuncCall \"$DATA\" [Var X]"
    (parseExpr "$Data(X)")
    (FuncCall "$DATA" [Var (mkRef Local "X" [])])

testLinesGroup :: TestContext -> IO ()
testLinesGroup ctx = do
  -- 1. Tags and dot depths
  assertEqual ctx "buildLogicalLines tag classification"
    (buildLogicalLines "MAIN ; entry point\n WRITE 1")
    [ LogicalLine 0 (Just "MAIN") "; entry point"
    , LogicalLine 0 Nothing "WRITE 1"
    ]

  assertEqual ctx "buildLogicalLines dot depths with bare DO blocks"
    (buildLogicalLines " DO\n. WRITE 2\n. DO\n.. WRITE 3")
    [ LogicalLine 0 Nothing "DO"
    , LogicalLine 1 Nothing "WRITE 2"
    , LogicalLine 1 Nothing "DO"
    , LogicalLine 2 Nothing "WRITE 3"
    ]

  -- 2. Line continuation with default space
  assertEqual ctx "buildLogicalLines continuation joins with default space"
    (buildLogicalLines " FOR EACH K IN ^ORDERS DO\n. MERGE A=B\n.. WRITE 1")
    [ LogicalLine 0 Nothing "FOR EACH K IN ^ORDERS DO"
    , LogicalLine 1 Nothing "MERGE A=B WRITE 1"
    ]

  -- 3. Hard join with '|'
  assertEqual ctx "buildLogicalLines continuation with '|' hard join (no space)"
    (buildLogicalLines " WRITE \"Hello\n.|World\"")
    [ LogicalLine 0 Nothing "WRITE \"HelloWorld\""
    ]

testStmtGroup :: TestContext -> IO ()
testStmtGroup ctx = do
  -- 1. SET
  assertIsRight ctx "SET X=1"
    (parseLogicalLine (LogicalLine 0 Nothing "SET X=1"))
    (Line 0 Nothing [PredStmt Nothing (SSet [(TargetRef (mkRef Local "X" []), Lit (mkValue "1"))])])

  assertIsRight ctx "SET X=1,Y=2,^Z(1)=3"
    (parseLogicalLine (LogicalLine 0 Nothing "SET X=1,Y=2,^Z(1)=3"))
    (Line 0 Nothing [PredStmt Nothing (SSet
      [ (TargetRef (mkRef Local "X" []), Lit (mkValue "1"))
      , (TargetRef (mkRef Local "Y" []), Lit (mkValue "2"))
      , (TargetRef (mkRef Global "Z" [Lit (mkValue "1")]), Lit (mkValue "3"))
      ])])

  assertIsRight ctx "SET ^X EXPECTING 1 = 2 -> SUnsupported"
    (parseLogicalLine (LogicalLine 0 Nothing "SET ^X EXPECTING 1 = 2"))
    (Line 0 Nothing [PredStmt Nothing (SUnsupported "EXPECTING" "SET ^X EXPECTING 1 = 2")])

  assertIsRight ctx "SET A,B=CALL TAG() -> SCallAssign"
    (parseLogicalLine (LogicalLine 0 Nothing "SET A,B=CALL TAG()"))
    (Line 0 Nothing [PredStmt Nothing (SCallAssign [mkRef Local "A" [], mkRef Local "B" []] (CallTarget (Just "TAG") Nothing (Just [])))])

  -- 2. WRITE
  assertIsRight ctx "WRITE \"SUM=\",SUM,!,#,?10"
    (parseLogicalLine (LogicalLine 0 Nothing "WRITE \"SUM=\",SUM,!,#,?10"))
    (Line 0 Nothing [PredStmt Nothing (SWrite
      [ Left (Lit (mkValue "SUM="))
      , Left (Var (mkRef Local "SUM" []))
      , Right WNewline
      , Right WFormFeed
      , Right (WTabTo 10)
      ])])

  -- 3. IF / ELSE
  assertIsRight ctx "IF X>0,Y>0"
    (parseLogicalLine (LogicalLine 0 Nothing "IF X>0,Y>0"))
    (Line 0 Nothing [PredStmt Nothing (SIf
      [ BinExpr Gt (Var (mkRef Local "X" [])) (Lit (mkValue "0"))
      , BinExpr Gt (Var (mkRef Local "Y" [])) (Lit (mkValue "0"))
      ])])

  assertIsRight ctx "ELSE"
    (parseLogicalLine (LogicalLine 0 Nothing "ELSE"))
    (Line 0 Nothing [PredStmt Nothing SElse])

  -- 4. Multi-statement line
  assertIsRight ctx "IF X>0 WRITE \"pos\",! ELSE  WRITE \"neg\",!"
    (parseLogicalLine (LogicalLine 0 Nothing "IF X>0 WRITE \"pos\",! ELSE  WRITE \"neg\",!"))
    (Line 0 Nothing
      [ PredStmt Nothing (SIf [BinExpr Gt (Var (mkRef Local "X" [])) (Lit (mkValue "0"))])
      , PredStmt Nothing (SWrite [Left (Lit (mkValue "pos")), Right WNewline])
      , PredStmt Nothing SElse
      , PredStmt Nothing (SWrite [Left (Lit (mkValue "neg")), Right WNewline])
      ])

  -- 5. FOR
  assertIsRight ctx "FOR (bare)"
    (parseLogicalLine (LogicalLine 0 Nothing "FOR"))
    (Line 0 Nothing [PredStmt Nothing (SFor Nothing)])

  assertIsRight ctx "FOR X=1"
    (parseLogicalLine (LogicalLine 0 Nothing "FOR X=1"))
    (Line 0 Nothing [PredStmt Nothing (SFor (Just (ForSpec "X" (Lit (mkValue "1")) Nothing Nothing)))])

  assertIsRight ctx "FOR X=1:2"
    (parseLogicalLine (LogicalLine 0 Nothing "FOR X=1:2"))
    (Line 0 Nothing [PredStmt Nothing (SFor (Just (ForSpec "X" (Lit (mkValue "1")) (Just (Lit (mkValue "2"))) Nothing)))])

  assertIsRight ctx "FOR X=1:2:10"
    (parseLogicalLine (LogicalLine 0 Nothing "FOR X=1:2:10"))
    (Line 0 Nothing [PredStmt Nothing (SFor (Just (ForSpec "X" (Lit (mkValue "1")) (Just (Lit (mkValue "2"))) (Just (Lit (mkValue "10"))))))])

  assertIsRight ctx "FOR WRITE=1:1:10 (loop var named WRITE with no keyword collision)"
    (parseLogicalLine (LogicalLine 0 Nothing "FOR WRITE=1:1:10"))
    (Line 0 Nothing [PredStmt Nothing (SFor (Just (ForSpec "WRITE" (Lit (mkValue "1")) (Just (Lit (mkValue "1"))) (Just (Lit (mkValue "10"))))))])

  assertIsRight ctx "FOR EACH K IN ^LIST"
    (parseLogicalLine (LogicalLine 0 Nothing "FOR EACH K IN ^LIST"))
    (Line 0 Nothing [PredStmt Nothing (SForEach "K" (mkRef Global "LIST" []))])

  assertIsRight ctx "FOR EACH oid IN ^ORDERS(1)"
    (parseLogicalLine (LogicalLine 0 Nothing "FOR EACH oid IN ^ORDERS(1)"))
    (Line 0 Nothing [PredStmt Nothing (SForEach "oid" (mkRef Global "ORDERS" [Lit (mkValue "1")]))])

  -- 6. DO / GOTO
  assertIsRight ctx "DO (bare)"
    (parseLogicalLine (LogicalLine 0 Nothing "DO"))
    (Line 0 Nothing [PredStmt Nothing (SDo Nothing)])

  assertIsRight ctx "DO TAG"
    (parseLogicalLine (LogicalLine 0 Nothing "DO TAG"))
    (Line 0 Nothing [PredStmt Nothing (SDo (Just (CallTarget (Just "TAG") Nothing Nothing)))])

  assertIsRight ctx "DO TAG(1,2)"
    (parseLogicalLine (LogicalLine 0 Nothing "DO TAG(1,2)"))
    (Line 0 Nothing [PredStmt Nothing (SDo (Just (CallTarget (Just "TAG") Nothing (Just [CallArgVal (Lit (mkValue "1")), CallArgVal (Lit (mkValue "2"))]))))])

  assertIsRight ctx "DO TAG^ROUTINE(1)"
    (parseLogicalLine (LogicalLine 0 Nothing "DO TAG^ROUTINE(1)"))
    (Line 0 Nothing [PredStmt Nothing (SDo (Just (CallTarget (Just "TAG") (Just "ROUTINE") (Just [CallArgVal (Lit (mkValue "1"))]))))])

  assertIsRight ctx "DO ^ROUTINE"
    (parseLogicalLine (LogicalLine 0 Nothing "DO ^ROUTINE"))
    (Line 0 Nothing [PredStmt Nothing (SDo (Just (CallTarget Nothing (Just "ROUTINE") Nothing)))])

  assertIsRight ctx "DO SET(1) (calling tag named SET followed by parens)"
    (parseLogicalLine (LogicalLine 0 Nothing "DO SET(1)"))
    (Line 0 Nothing [PredStmt Nothing (SDo (Just (CallTarget (Just "SET") Nothing (Just [CallArgVal (Lit (mkValue "1"))]))))])

  assertIsRight ctx "GOTO TAG^ROUTINE"
    (parseLogicalLine (LogicalLine 0 Nothing "GOTO TAG^ROUTINE"))
    (Line 0 Nothing [PredStmt Nothing (SGoto (CallTarget (Just "TAG") (Just "ROUTINE") Nothing))])

  -- The root namespace is named with a leading underscore; this used
  -- to be a parse error, so it could not be written explicitly.
  assertIsRight ctx "DO FOOBAR^_GLOBAL(1) (explicit root namespace)"
    (parseLogicalLine (LogicalLine 0 Nothing "DO FOOBAR^_GLOBAL(1)"))
    (Line 0 Nothing [PredStmt Nothing (SDo (Just (CallTarget (Just "FOOBAR") (Just "_GLOBAL") (Just [CallArgVal (Lit (mkValue "1"))]))))])

  -- 7. QUIT / RETURN / NEW / KILL
  assertIsRight ctx "QUIT (bare)"
    (parseLogicalLine (LogicalLine 0 Nothing "QUIT"))
    (Line 0 Nothing [PredStmt Nothing (SQuit Nothing)])

  assertIsRight ctx "QUIT 42"
    (parseLogicalLine (LogicalLine 0 Nothing "QUIT 42"))
    (Line 0 Nothing [PredStmt Nothing (SQuit (Just (Lit (mkValue "42"))))])

  assertIsRight ctx "RETURN (bare)"
    (parseLogicalLine (LogicalLine 0 Nothing "RETURN"))
    (Line 0 Nothing [PredStmt Nothing (SReturn [])])

  assertIsRight ctx "RETURN 1,2"
    (parseLogicalLine (LogicalLine 0 Nothing "RETURN 1,2"))
    (Line 0 Nothing [PredStmt Nothing (SReturn [Lit (mkValue "1"), Lit (mkValue "2")])])

  assertIsRight ctx "NEW (bare)"
    (parseLogicalLine (LogicalLine 0 Nothing "NEW"))
    (Line 0 Nothing [PredStmt Nothing (SNew Nothing)])

  assertIsRight ctx "NEW X,Y"
    (parseLogicalLine (LogicalLine 0 Nothing "NEW X,Y"))
    (Line 0 Nothing [PredStmt Nothing (SNew (Just ["X", "Y"]))])

  assertIsRight ctx "KILL (bare)"
    (parseLogicalLine (LogicalLine 0 Nothing "KILL"))
    (Line 0 Nothing [PredStmt Nothing (SKill Nothing)])

  assertIsRight ctx "KILL X,^Y(1)"
    (parseLogicalLine (LogicalLine 0 Nothing "KILL X,^Y(1)"))
    (Line 0 Nothing [PredStmt Nothing (SKill (Just [mkRef Local "X" [], mkRef Global "Y" [Lit (mkValue "1")]]))])

  -- 8. Single space vs. double space rule
  assertIsLeft ctx "QUIT WRITE (1 space before keyword) -> parse error"
    (parseLogicalLine (LogicalLine 0 Nothing "QUIT WRITE"))

  assertIsRight ctx "QUIT  WRITE 1 (2 spaces) -> 2 separate statements"
    (parseLogicalLine (LogicalLine 0 Nothing "QUIT  WRITE 1"))
    (Line 0 Nothing
      [ PredStmt Nothing (SQuit Nothing)
      , PredStmt Nothing (SWrite [Left (Lit (mkValue "1"))])
      ])

  -- 9. MERGE
  assertIsRight ctx "MERGE ^TARGET=^SOURCE,A=B"
    (parseLogicalLine (LogicalLine 0 Nothing "MERGE ^TARGET=^SOURCE,A=B"))
    (Line 0 Nothing [PredStmt Nothing (SMerge
      [ (mkRef Global "TARGET" [], mkRef Global "SOURCE" [])
      , (mkRef Local "A" [], mkRef Local "B" [])
      ])])

  -- 10. LOCK / UNLOCK
  assertIsRight ctx "LOCK +^X:5000,-^Y,^Z"
    (parseLogicalLine (LogicalLine 0 Nothing "LOCK +^X:5000,-^Y,^Z"))
    (Line 0 Nothing [PredStmt Nothing (SLock (Just
      [ LockSpec (Just True) (mkRef Global "X" []) (Just (Lit (mkValue "5000")))
      , LockSpec (Just False) (mkRef Global "Y" []) Nothing
      , LockSpec Nothing (mkRef Global "Z" []) Nothing
      ]))])

  assertIsRight ctx "UNLOCK (bare)"
    (parseLogicalLine (LogicalLine 0 Nothing "UNLOCK"))
    (Line 0 Nothing [PredStmt Nothing (SUnlock Nothing)])

  -- 11. END DO
  assertIsRight ctx "END DO"
    (parseLogicalLine (LogicalLine 0 Nothing "END DO"))
    (Line 0 Nothing [PredStmt Nothing SEndDo])

  -- 12. Postconditions
  assertIsRight ctx "SET:DONE X=1"
    (parseLogicalLine (LogicalLine 0 Nothing "SET:DONE X=1"))
    (Line 0 Nothing [PredStmt (Just (Var (mkRef Local "DONE" []))) (SSet [(TargetRef (mkRef Local "X" []), Lit (mkValue "1"))])])

  assertIsRight ctx "WRITE:X>0 \"pos\",!"
    (parseLogicalLine (LogicalLine 0 Nothing "WRITE:X>0 \"pos\",!"))
    (Line 0 Nothing [PredStmt
      (Just (BinExpr Gt (Var (mkRef Local "X" [])) (Lit (mkValue "0"))))
      (SWrite [Left (Lit (mkValue "pos")), Right WNewline])])

  assertIsRight ctx "QUIT:I=\"\""
    (parseLogicalLine (LogicalLine 0 Nothing "QUIT:I=\"\""))
    (Line 0 Nothing [PredStmt
      (Just (BinExpr Eq (Var (mkRef Local "I" [])) (Lit (mkValue ""))))
      (SQuit Nothing)])

  -- 13. SExpr and SUnsupported
  assertIsRight ctx "1+2*3 bare expression statement"
    (parseLogicalLine (LogicalLine 0 Nothing "1+2*3"))
    (Line 0 Nothing [PredStmt Nothing (SExpr (BinExpr Add (Lit (mkValue "1")) (BinExpr Mul (Lit (mkValue "2")) (Lit (mkValue "3")))))])

  assertIsRight ctx "TRANSACTION START unsupported statement"
    (parseLogicalLine (LogicalLine 0 Nothing "TRANSACTION START"))
    (Line 0 Nothing [PredStmt Nothing (SUnsupported "TRANSACTION" "TRANSACTION START")])

  -- 14. NAMESPACE statement
  assertIsRight ctx "NAMESPACE Billing"
    (parseLogicalLine (LogicalLine 0 Nothing "NAMESPACE Billing"))
    (Line 0 Nothing [PredStmt Nothing (SNamespace "Billing")])

  assertIsRight ctx "NAMESPACE Billing.Invoices"
    (parseLogicalLine (LogicalLine 0 Nothing "NAMESPACE Billing.Invoices"))
    (Line 0 Nothing [PredStmt Nothing (SNamespace "Billing.Invoices")])

testTreeGroup :: TestContext -> IO ()
testTreeGroup ctx = do
  -- 1. Node duality (root value + child nodes)
  let n0 = setNode [] (mkValue "root") emptyNode
  let n1 = setNode [mkValue "1"] (mkValue "child1") n0
  let n2 = setNode [mkValue "1", mkValue "PRICE"] (mkValue "100") n1
  assertEqual ctx "Tree: getNode root" (getNode [] n2) (Just (mkValue "root"))
  assertEqual ctx "Tree: getNode [1]" (getNode [mkValue "1"] n2) (Just (mkValue "child1"))
  assertEqual ctx "Tree: getNode [1, PRICE]" (getNode [mkValue "1", mkValue "PRICE"] n2) (Just (mkValue "100"))

  -- 2. $DATA four-state check
  assertEqual ctx "Tree: dataNode on empty node -> 0" (dataNode [] emptyNode) 0
  assertEqual ctx "Tree: dataNode value only -> 1" (dataNode [] (setNode [] (mkValue "v") emptyNode)) 1
  assertEqual ctx "Tree: dataNode child only -> 10" (dataNode [] (setNode [mkValue "1"] (mkValue "v") emptyNode)) 10
  assertEqual ctx "Tree: dataNode value + child -> 11" (dataNode [] (setNode [] (mkValue "v") (setNode [mkValue "1"] (mkValue "c") emptyNode))) 11

  -- 3. $ORDER with canonical collation ordering
  let treeWithKeys = foldl (\t k -> setNode [mkValue k] (mkValue "v") t) emptyNode ["B", "10", "1", "2", "A", "1.0"]
  assertEqual ctx "Tree: orderNode start -> '1'" (orderNode [] (mkValue "") treeWithKeys) (Just (mkValue "1"))
  assertEqual ctx "Tree: orderNode '1' -> '2'" (orderNode [] (mkValue "1") treeWithKeys) (Just (mkValue "2"))
  assertEqual ctx "Tree: orderNode '2' -> '10'" (orderNode [] (mkValue "2") treeWithKeys) (Just (mkValue "10"))
  assertEqual ctx "Tree: orderNode '10' -> '1.0'" (orderNode [] (mkValue "10") treeWithKeys) (Just (mkValue "1.0"))
  assertEqual ctx "Tree: orderNode '1.0' -> 'A'" (orderNode [] (mkValue "1.0") treeWithKeys) (Just (mkValue "A"))
  assertEqual ctx "Tree: orderNode 'A' -> 'B'" (orderNode [] (mkValue "A") treeWithKeys) (Just (mkValue "B"))
  assertEqual ctx "Tree: orderNode 'B' -> Nothing" (orderNode [] (mkValue "B") treeWithKeys) Nothing

  -- 4. killNode
  let toKill = setNode [mkValue "1", mkValue "A"] (mkValue "a") (setNode [mkValue "1", mkValue "B"] (mkValue "b") emptyNode)
  let killedA = killNode [mkValue "1", mkValue "A"] toKill
  assertEqual ctx "Tree: killNode leaf" (maybe Nothing (getNode [mkValue "1", mkValue "B"]) killedA) (Just (mkValue "b"))
  assertEqual ctx "Tree: killNode leaf removed" (maybe Nothing (getNode [mkValue "1", mkValue "A"]) killedA) Nothing

  -- 5. mergeNode
  let src = setNode [mkValue "1"] (mkValue "new1") (setNode [mkValue "2"] (mkValue "new2") emptyNode)
  let dst = setNode [mkValue "1"] (mkValue "old1") (setNode [mkValue "3"] (mkValue "old3") emptyNode)
  let merged = mergeNode [] dst [] src
  assertEqual ctx "Tree: mergeNode overrides overlapping key" (getNode [mkValue "1"] merged) (Just (mkValue "new1"))
  assertEqual ctx "Tree: mergeNode adds new key" (getNode [mkValue "2"] merged) (Just (mkValue "new2"))
  assertEqual ctx "Tree: mergeNode preserves distinct key" (getNode [mkValue "3"] merged) (Just (mkValue "old3"))

testEvalExprGroup :: TestContext -> IO ()
testEvalExprGroup ctx = do
  adminEnv <- testAdminEnv
  let runE e = evalState (evalExpr e) adminEnv

  -- 1. Arithmetic
  assertEqualM ctx "evalExpr '1+2*3' -> 7" (runE (BinExpr Add (Lit (mkValue "1")) (BinExpr Mul (Lit (mkValue "2")) (Lit (mkValue "3"))))) (Right (mkValue "7"))
  assertEqualM ctx "evalExpr '2**3**2' -> 512" (runE (BinExpr Pow (Lit (mkValue "2")) (BinExpr Pow (Lit (mkValue "3")) (Lit (mkValue "2"))))) (Right (mkValue "512"))
  assertEqualM ctx "evalExpr '-2**2' -> 4" (runE (BinExpr Pow (UnExpr Neg (Lit (mkValue "2"))) (Lit (mkValue "2")))) (Right (mkValue "4"))
  assertEqualM ctx "evalExpr '10/2' -> 5" (runE (BinExpr Div (Lit (mkValue "10")) (Lit (mkValue "2")))) (Right (mkValue "5"))
  assertEqualM ctx "evalExpr '10/0' -> <DIVIDE BY ZERO>" (runE (BinExpr Div (Lit (mkValue "10")) (Lit (mkValue "0")))) (Left DivideByZero)
  assertEqualM ctx "evalExpr '10\\3' -> 3" (runE (BinExpr IntDiv (Lit (mkValue "10")) (Lit (mkValue "3")))) (Right (mkValue "3"))
  assertEqualM ctx "evalExpr '10#3' -> 1" (runE (BinExpr Mod (Lit (mkValue "10")) (Lit (mkValue "3")))) (Right (mkValue "1"))
  -- Regression: '\' and '#' used to round-trip through Double (~15-17
  -- significant digits), contradicting the handbook's "all six
  -- arithmetic operators run on 34-digit BigDecimal" guarantee. This
  -- operand exceeds Double's exact-integer range (2^53), so a Double
  -- round-trip corrupts the quotient; the exact 'Scientific'-based
  -- path must not.
  assertEqualM ctx "evalExpr '100000000000000000000\\3' -> 33333333333333333333 (exact, not Double-rounded)"
    (runE (BinExpr IntDiv (Lit (mkValue "100000000000000000000")) (Lit (mkValue "3"))))
    (Right (mkValue "33333333333333333333"))
  assertEqualM ctx "evalExpr '100000000000000000000#3' -> 1 (exact remainder)"
    (runE (BinExpr Mod (Lit (mkValue "100000000000000000000")) (Lit (mkValue "3"))))
    (Right (mkValue "1"))
  assertEqualM ctx "evalExpr '-10\\3' -> -3 (truncate toward zero)"
    (runE (BinExpr IntDiv (Lit (mkValue "-10")) (Lit (mkValue "3"))))
    (Right (mkValue "-3"))
  assertEqualM ctx "evalExpr '-10#3' -> -1 (remainder takes dividend's sign)"
    (runE (BinExpr Mod (Lit (mkValue "-10")) (Lit (mkValue "3"))))
    (Right (mkValue "-1"))
  assertEqualM ctx "evalExpr '10\\0' -> <DIVIDE BY ZERO>"
    (runE (BinExpr IntDiv (Lit (mkValue "10")) (Lit (mkValue "0"))))
    (Left DivideByZero)
  assertEqualM ctx "evalExpr '10#0' -> <DIVIDE BY ZERO>"
    (runE (BinExpr Mod (Lit (mkValue "10")) (Lit (mkValue "0"))))
    (Left DivideByZero)

  -- 2. Concat, Eq, Comparisons
  assertEqualM ctx "evalExpr '\"A\"_\"B\"' -> 'AB'" (runE (BinExpr Concat (Lit (mkValue "A")) (Lit (mkValue "B")))) (Right (mkValue "AB"))
  assertEqualM ctx "evalExpr '1=1' -> 1" (runE (BinExpr Eq (Lit (mkValue "1")) (Lit (mkValue "1")))) (Right (mkValue "1"))
  assertEqualM ctx "evalExpr '1=1.0' -> 0" (runE (BinExpr Eq (Lit (mkValue "1")) (Lit (mkValue "1.0")))) (Right (mkValue "0"))
  assertEqualM ctx "evalExpr '\"HELLO\"[\"ELL\"' -> 1" (runE (BinExpr Contains (Lit (mkValue "HELLO")) (Lit (mkValue "ELL")))) (Right (mkValue "1"))
  assertEqualM ctx "evalExpr '\"HELLO\"[\"XYZ\"' -> 0" (runE (BinExpr Contains (Lit (mkValue "HELLO")) (Lit (mkValue "XYZ")))) (Right (mkValue "0"))

  -- 3. Slices
  assertEqualM ctx "evalExpr '\"ABCDEF\"[2:4]' -> 'BCD'" (runE (Slice (Lit (mkValue "ABCDEF")) (Just (Lit (mkValue "2"))) (Just (Lit (mkValue "4"))))) (Right (mkValue "BCD"))
  assertEqualM ctx "evalExpr '\"ABCDEF\"[:3]' -> 'ABC'" (runE (Slice (Lit (mkValue "ABCDEF")) Nothing (Just (Lit (mkValue "3"))))) (Right (mkValue "ABC"))
  assertEqualM ctx "evalExpr '\"ABCDEF\"[4:]' -> 'DEF'" (runE (Slice (Lit (mkValue "ABCDEF")) (Just (Lit (mkValue "4"))) Nothing)) (Right (mkValue "DEF"))

  -- 4. Intrinsics ($LENGTH, $EXTRACT, $PIECE, $GET)
  assertEqualM ctx "evalExpr '$LENGTH(\"HELLO\")' -> 5" (runE (FuncCall "$LENGTH" [Lit (mkValue "HELLO")])) (Right (mkValue "5"))
  assertEqualM ctx "evalExpr '$EXTRACT(\"HELLO\",2,4)' -> 'ELL'" (runE (FuncCall "$EXTRACT" [Lit (mkValue "HELLO"), Lit (mkValue "2"), Lit (mkValue "4")])) (Right (mkValue "ELL"))
  assertEqualM ctx "evalExpr '$PIECE(\"A^B^C^D\",\"^\",3)' -> 'C'" (runE (FuncCall "$PIECE" [Lit (mkValue "A^B^C^D"), Lit (mkValue "^"), Lit (mkValue "3")])) (Right (mkValue "C"))
  assertEqualM ctx "evalExpr '$GET(UNDEF,\"default\")' -> 'default'" (runE (FuncCall "$GET" [Var (mkRef Local "UNDEF" []), Lit (mkValue "default")])) (Right (mkValue "default"))

testEvalStmtGroup :: TestContext -> IO ()
testEvalStmtGroup ctx = do
  adminEnv <- testAdminEnv
  -- Helper to parse and execute a routine from source string
  let execScript src =
        case mapM parseLogicalLine (buildLogicalLines src) of
          Left parseErr  -> pure (Left (show parseErr))
          Right astLines -> do
            result <- runRoutine astLines adminEnv
            case result of
              Left evalErr -> pure (Left (show evalErr))
              Right env    -> pure (Right env)

  -- 1. SET and WRITE
  execResult <- execScript " SET X=5,Y=10 WRITE X+Y,!"
  case execResult of
    Left err -> runAssert ctx "SET and WRITE execution" False (show err)
    Right env -> do
      assertEqual ctx "SET X=5,Y=10 WRITE X+Y,! output" (getOutputText env) "15\n"
      assertEqual ctx "SET read X" (readRef Local "X" [] env) (Right (mkValue "5"))
      assertEqual ctx "SET read Y" (readRef Local "Y" [] env) (Right (mkValue "10"))

  -- 2. Node duality via SET and $DATA
  execResult <- execScript " SET ^ORDERS(1)=\"summary\"\n SET ^ORDERS(1,\"PRICE\")=100\n WRITE $DATA(^ORDERS(1)),!"
  case execResult of
    Left err -> runAssert ctx "Node duality execution" False (show err)
    Right env -> do
      assertEqual ctx "Node duality $DATA(^ORDERS(1)) == 11" (getOutputText env) "11\n"

  -- 3. IF / ELSE branching
  execResult <- execScript " IF 1>0 WRITE \"pos\",! ELSE  WRITE \"neg\",!"
  case execResult of
    Left err -> runAssert ctx "IF/ELSE true branch" False (show err)
    Right env -> assertEqual ctx "IF true branch output" (getOutputText env) "pos\n"

  execResult <- execScript " IF 0>1 WRITE \"pos\",! ELSE  WRITE \"neg\",!"

  case execResult of
    Left err -> runAssert ctx "IF/ELSE false branch" False (show err)
    Right env -> assertEqual ctx "IF false branch output" (getOutputText env) "neg\n"

  -- 4. FOR loop execution
  execResult <- execScript " FOR I=1:1:5 WRITE I"
  case execResult of
    Left err -> runAssert ctx "FOR loop execution" False (show err)
    Right env -> assertEqual ctx "FOR I=1:1:5 output" (getOutputText env) "12345"

  -- 5. FOR loop accumulating sum
  execResult <- execScript " SET SUM=0\n FOR I=1:1:4 SET SUM=SUM+I\n WRITE SUM,!"
  case execResult of
    Left err -> runAssert ctx "FOR accumulating sum" False (show err)
    Right env -> assertEqual ctx "FOR accumulating sum output" (getOutputText env) "10\n"

  -- 6. MERGE subtree copying
  execResult <- execScript " SET ^SRC(1)=\"A\",^SRC(2)=\"B\"\n MERGE ^DST=^SRC"
  case execResult of
    Left err -> runAssert ctx "MERGE execution" False (show err)
    Right env -> do
      assertEqual ctx "MERGE copied ^DST(1)" (readRef Global "DST" [mkValue "1"] env) (Right (mkValue "A"))
      assertEqual ctx "MERGE copied ^DST(2)" (readRef Global "DST" [mkValue "2"] env) (Right (mkValue "B"))

  -- 7. Variable restoration with NEW
  let Right envWithX = writeRef Local "X" [] (mkValue "original") adminEnv
  let frame = Frame Nothing Nothing (0, 0) [] []
  let envFramed = pushFrame frame envWithX
  let envScoped = newVar "X" envFramed
  let Right envModified = writeRef Local "X" [] (mkValue "modified") envScoped
  let (_, envRestored) = popFrame envModified
  assertEqual ctx "NEW restores variable on frame exit" (readRef Local "X" [] envRestored) (Right (mkValue "original"))

  -- 8. NAMESPACE statement changes envCurrentNamespace
  execResult <- execScript " NAMESPACE Billing"
  case execResult of
    Left err -> runAssert ctx "NAMESPACE eval" False (show err)
    Right env -> assertEqual ctx "NAMESPACE sets envCurrentNamespace" (envCurrentNamespace env) "Billing"

  -- 9. Namespace-qualified global SET and READ
  execResult <- execScript " SET Billing^ORDERS(1)=\"Widget\""
  case execResult of
    Left err -> runAssert ctx "NS^GLOBAL SET parse/exec" False (show err)
    Right env -> assertEqual ctx "SET Billing^ORDERS(1) stores under qualified name"
      (readRef Global "Billing^ORDERS" [mkValue "1"] env)
      (Right (mkValue "Widget"))

  -- 10. FOR EACH over global with dotted block
  let forEachGlobalSrc = " SET ^LIST(1)=\"A\",^LIST(2)=\"B\",^LIST(3)=\"C\"\n FOR EACH K IN ^LIST DO\n . WRITE K\n"
  execResult <- execScript forEachGlobalSrc
  case execResult of
    Left err -> runAssert ctx "FOR EACH ^LIST execution" False (show err)
    Right env -> assertEqual ctx "FOR EACH ^LIST output" (getOutputText env) "123"

  -- 11. FOR EACH over local variable
  let forEachLocalSrc = " SET ITEMS(\"A\")=10,ITEMS(\"B\")=20\n FOR EACH I IN ITEMS WRITE I"
  execResult <- execScript forEachLocalSrc
  case execResult of
    Left err -> runAssert ctx "FOR EACH ITEMS execution" False (show err)
    Right env -> assertEqual ctx "FOR EACH ITEMS output" (getOutputText env) "AB"

  -- 12. FOR EACH with postconditioned MERGE (materialized join, Part X §2)
  let forEachJoinSrc = T.unlines
        [ " SET ^ORDERS(1,\"CUSTOMER\")=101,^ORDERS(1,\"TOTAL\")=50"
        , " SET ^ORDERS(2,\"CUSTOMER\")=102,^ORDERS(2,\"TOTAL\")=150"
        , " SET ^CUSTOMERS(101,\"CITY\")=\"London\""
        , " SET ^CUSTOMERS(102,\"CITY\")=\"Paris\""
        , " FOR EACH oid IN ^ORDERS DO"
        , " . MERGE:^CUSTOMERS(^ORDERS(oid,\"CUSTOMER\"),\"CITY\")=\"London\" london(oid)=^ORDERS(oid)"
        ]
  execResult <- execScript forEachJoinSrc
  case execResult of
    Left err -> runAssert ctx "FOR EACH join execution" False (show err)
    Right env -> do
      assertEqual ctx "Join: london(1) captured" (readRef Local "london" [mkValue "1", mkValue "TOTAL"] env) (Right (mkValue "50"))
      assertEqual ctx "Join: london(2) excluded" (dataRef Local "london" [mkValue "2"] env) 0

  -- 13. Expression-position CALL
  let Right envWithDouble = defineItem "_GLOBAL" "DOUBLE" KindFunction (Just ["X"]) " RETURN X*2" adminEnv
  callResult1 <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " WRITE CALL DOUBLE(21),!") of Right ls -> ls; Left _ -> []) envWithDouble
  case callResult1 of
    Left err -> runAssert ctx "CALL DOUBLE(21)" False (show err)
    Right env -> assertEqual ctx "CALL DOUBLE(21) output" (getOutputText env) "42\n"

  -- 14. Multi-value CALL / RETURN destructuring
  let Right envWithTotal = defineItem "_GLOBAL" "CALCTAX" KindFunction (Just ["orders", "rate"]) " RETURN orders,orders*rate" adminEnv
  callResult2 <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " SET total,tax = CALL CALCTAX(100,0.2)") of Right ls -> ls; Left _ -> []) envWithTotal
  case callResult2 of
    Left err -> runAssert ctx "Multi-value CALL destructuring" False (show err)
    Right env -> do
      assertEqual ctx "Multi-value total" (readRef Local "total" [] env) (Right (mkValue "100"))
      assertEqual ctx "Multi-value tax" (readRef Local "tax" [] env) (Right (mkValue "20"))

  -- 15. Multi-value CALL arity mismatch
  callResult3 <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " SET a,b,c = CALL CALCTAX(100,0.2)") of Right ls -> ls; Left _ -> []) envWithTotal
  case callResult3 of
    Left (SyntaxError msg) -> assertEqual ctx "Arity mismatch error message" msg "CALL returned 2 value(s), 3 target(s) expected"
    res -> runAssert ctx "Arity mismatch should fail" False (show res)

  -- 16. Reference parameter &ref
  let Right envWithDoubleIt = defineItem "_GLOBAL" "DOUBLEIT" KindFunction (Just ["n"]) " SET n=n*2\n QUIT" adminEnv
  callResult4 <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " SET x=21\n DO DOUBLEIT(&x)\n WRITE x,!") of Right ls -> ls; Left _ -> []) envWithDoubleIt
  case callResult4 of
    Left err -> runAssert ctx "Reference parameter &ref" False (show err)
    Right env -> do
      assertEqual ctx "Reference parameter modified x" (readRef Local "x" [] env) (Right (mkValue "42"))
      assertEqual ctx "Reference parameter output" (getOutputText env) "42\n"

  -- 17. $ORDER / bare-FOR idiom: QUIT inside FOR exits the loop only, not the routine
  -- This is the canonical M iteration pattern:
  --   SET I=""
  --   FOR  SET I=$ORDER(^LIST(I)) QUIT:I=""  WRITE I,!
  -- After the loop, execution must continue (here: WRITE "done",!)
  let forOrderSrc = T.unlines
        [ " SET ^LIST(1)=\"A\",^LIST(2)=\"B\",^LIST(3)=\"C\""
        , " SET I=\"\""
        , " FOR  SET I=$ORDER(^LIST(I)) QUIT:I=\"\"  WRITE I,!"
        , " WRITE \"done\",!"
        ]
  forOrderResult <- execScript forOrderSrc
  case forOrderResult of
    Left err -> runAssert ctx "$ORDER bare-FOR idiom" False err
    Right env -> assertEqual ctx "$ORDER bare-FOR idiom output" (getOutputText env) "1\n2\n3\ndone\n"

  -- 17b. Same idiom, but across a double-digit key count -- this is
  -- exactly the shape that would expose a STRING-collation bug (a
  -- wrong $ORDER would visit "1","10","11","12","2",...,"9" instead of
  -- 1,2,...,12), which a 1-2-3 test cannot distinguish from correct
  -- numeric collation.
  let forOrderManySrc = T.unlines
        ([ " SET ^MANY(" <> T.pack (show n) <> ")=1" | n <- [1 .. 12 :: Int] ]
         ++ [ " SET I=\"\""
            , " FOR  SET I=$ORDER(^MANY(I)) QUIT:I=\"\"  WRITE I,\",\""
            ])
  forOrderManyResult <- execScript forOrderManySrc
  case forOrderManyResult of
    Left err -> runAssert ctx "$ORDER bare-FOR over 12 numeric keys" False err
    Right env -> assertEqual ctx "$ORDER bare-FOR over 12 numeric keys visits them in numeric order"
      (getOutputText env) "1,2,3,4,5,6,7,8,9,10,11,12,"

  -- 17c. FOR EACH over the same 12-key collection, independently of
  -- the bare-FOR + manual $ORDER idiom above (SForEach has its own,
  -- separate traversal code in Phase.Eval).
  let forEachManySrc = T.unlines
        ([ " SET ^MANY(" <> T.pack (show n) <> ")=1" | n <- [1 .. 12 :: Int] ]
         ++ [ " FOR EACH K IN ^MANY WRITE K,\",\"" ])
  forEachManyResult <- execScript forEachManySrc
  case forEachManyResult of
    Left err -> runAssert ctx "FOR EACH over 12 numeric keys" False err
    Right env -> assertEqual ctx "FOR EACH over 12 numeric keys visits them in numeric order"
      (getOutputText env) "1,2,3,4,5,6,7,8,9,10,11,12,"

  -- 18. Stepped FOR loop: QUIT also exits only the loop
  let steppedForSrc = " FOR I=1:1:5 QUIT:I=3\n WRITE \"after\",!"
  steppedForResult <- execScript steppedForSrc
  case steppedForResult of
    Left err -> runAssert ctx "Stepped FOR QUIT exits loop only" False err
    Right env -> assertEqual ctx "Stepped FOR QUIT exits loop, routine continues" (getOutputText env) "after\n"

  -- 19. Lowercase intrinsics evaluate correctly
  lowercaseResult <- execScript " WRITE $length(\"hello\"),!"
  case lowercaseResult of
    Left err -> runAssert ctx "Lowercase $length evaluates" False err
    Right env -> assertEqual ctx "Lowercase $length output" (getOutputText env) "5\n"

  -- 20. FOR nested inside a DO dot-block (IF 1 DO / . FOR I=1:1:3 DO / .. WRITE)
  -- Regression: bare DO must re-register blockLines in envRoutines so that any
  -- nested bare DO inside the block finds the correct sub-list, not the full
  -- outer routine. Without the fix this produced a stack overflow.
  let nestedForInDoSrc = T.unlines
        [ " IF 1 DO"
        , " . FOR I=1:1:3 DO"
        , " .. WRITE I,!"
        ]
  nestedForResult <- execScript nestedForInDoSrc
  case nestedForResult of
    Left err -> runAssert ctx "FOR nested inside DO block" False err
    Right env -> assertEqual ctx "FOR nested inside DO block output" (getOutputText env) "1\n2\n3\n"

  -- 21. FOR EACH nested inside a DO dot-block (IF 1 DO / . FOR EACH I IN X DO / .. WRITE)
  let nestedForEachSrc = T.unlines
        [ " SET NX(\"A\")=10,NX(\"B\")=20"
        , " IF 1 DO"
        , " . FOR EACH NK IN NX DO"
        , " .. WRITE NK,!"
        ]
  nestedForEachResult <- execScript nestedForEachSrc
  case nestedForEachResult of
    Left err -> runAssert ctx "FOR EACH nested inside DO block" False err
    Right env -> assertEqual ctx "FOR EACH nested inside DO block output" (getOutputText env) "A\nB\n"

  -- 22. GOTO within the running routine (previously a silent no-op:
  -- 'SGoto' discarded its target and fell through to the next
  -- statement, and no test exercised it beyond parsing).
  let gotoSkipSrc = T.unlines
        [ " WRITE \"before\",!"
        , " GOTO SKIP"
        , " WRITE \"skipped\",!"
        , "SKIP WRITE \"after\",!"
        ]
  gotoSkipResult <- execScript gotoSkipSrc
  case gotoSkipResult of
    Left err -> runAssert ctx "GOTO same-routine forward jump" False err
    Right env -> assertEqual ctx "GOTO same-routine forward jump output" (getOutputText env) "before\nafter\n"

  -- 23. GOTO does not stop at the next tag: it is a program-counter
  -- move, so execution falls through subsequent tags exactly as in M.
  let gotoFallSrc = T.unlines
        [ " WRITE \"start\",!"
        , " GOTO BETA"
        , "ALPHA WRITE \"alpha\",!"
        , "BETA WRITE \"beta\",!"
        , "GAMMA WRITE \"gamma\",!"
        ]
  gotoFallResult <- execScript gotoFallSrc
  case gotoFallResult of
    Left err -> runAssert ctx "GOTO falls through subsequent tags" False err
    Right env -> assertEqual ctx "GOTO falls through subsequent tags output" (getOutputText env) "start\nbeta\ngamma\n"

  -- 24. GOTO to an undefined tag is an error, not a silent no-op.
  gotoMissingResult <- execScript " GOTO NOWHERE"
  assertEqual ctx "GOTO undefined tag -> <UNDEFINED LABEL>" (either id (const "ok") gotoMissingResult) (show (UndefinedLabel "NOWHERE" Nothing))

  -- 25. Cross-namespace calls with TAG^NAMESPACE, without switching
  -- the active namespace via NAMESPACE. The text after '^' is a
  -- Catalog namespace ('lookupItem' first argument), not a separate
  -- routine file. Until now only 'lookupItem' was tested directly and
  -- the parser accepted the shape; this drives it through 'invokeTarget'.
  let Right envMathSquare = defineItem "MATH" "SQUARE" KindFunction (Just ["X"]) " RETURN X*X" adminEnv
  crossCall <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " WRITE CALL SQUARE^MATH(6),!") of Right ls -> ls; Left _ -> []) envMathSquare
  case crossCall of
    Left err -> runAssert ctx "Cross-namespace CALL SQUARE^MATH" False (show err)
    Right env -> assertEqual ctx "Cross-namespace CALL SQUARE^MATH output" (getOutputText env) "36\n"

  -- Unqualified lookup from _GLOBAL must NOT see MATH's function.
  crossCallUnqualified <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " WRITE CALL SQUARE(6),!") of Right ls -> ls; Left _ -> []) envMathSquare
  case crossCallUnqualified of
    Left (UndefinedLabel "SQUARE" Nothing) -> runAssert ctx "Unqualified CALL SQUARE from _GLOBAL does not see MATH" True ""
    res -> runAssert ctx "Unqualified CALL SQUARE from _GLOBAL does not see MATH" False (show (fmap getOutputText res))

  -- Qualified lookup still falls back to _GLOBAL when the named
  -- namespace lacks the tag (Part V fallback, via 'lookupItem').
  let Right envGreeting = defineItem "_GLOBAL" "GREETING" KindFunction (Just []) " RETURN \"hi\"" envMathSquare
  crossFallback <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " WRITE CALL GREETING^MATH(),!") of Right ls -> ls; Left _ -> []) envGreeting
  case crossFallback of
    Left err -> runAssert ctx "Qualified CALL falls back to _GLOBAL" False (show err)
    Right env -> assertEqual ctx "Qualified CALL falls back to _GLOBAL output" (getOutputText env) "hi\n"

  -- Explicit _GLOBAL qualification is equivalent to the bare form.
  crossGlobal <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " WRITE CALL GREETING^_GLOBAL(),!") of Right ls -> ls; Left _ -> []) envGreeting
  case crossGlobal of
    Left err -> runAssert ctx "CALL GREETING^_GLOBAL" False (show err)
    Right env -> assertEqual ctx "CALL GREETING^_GLOBAL output" (getOutputText env) "hi\n"

  -- A qualified name that exists nowhere is an error naming the namespace.
  crossMissing <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " WRITE CALL NOPE^MATH(),!") of Right ls -> ls; Left _ -> []) envGreeting
  case crossMissing of
    Left (UndefinedLabel "NOPE" (Just "MATH")) -> runAssert ctx "CALL NOPE^MATH -> <UNDEFINED LABEL> NOPE^MATH" True ""
    res -> runAssert ctx "CALL NOPE^MATH -> <UNDEFINED LABEL> NOPE^MATH" False (show (fmap getOutputText res))

  -- 26. DO TAG^NAMESPACE shares invokeTarget with CALL.
  let Right envShout = defineItem "MATH" "SHOUT" KindFunction (Just ["msg"]) " WRITE msg,\"!!!\",!" envGreeting
  crossDo <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " DO SHOUT^MATH(\"hi\")") of Right ls -> ls; Left _ -> []) envShout
  case crossDo of
    Left err -> runAssert ctx "Cross-namespace DO SHOUT^MATH" False (show err)
    Right env -> assertEqual ctx "Cross-namespace DO SHOUT^MATH output" (getOutputText env) "hi!!!\n"

  -- 27. GOTO TAG^NAMESPACE transfers permanently: the target runs and
  -- the statements after the GOTO never do.
  let Right envFinish = defineItem "MATH" "FINISH" KindFunction Nothing " WRITE \"done\",!" envShout
  crossGoto <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " WRITE \"start\",!\n GOTO FINISH^MATH\n WRITE \"unreachable\",!") of Right ls -> ls; Left _ -> []) envFinish
  case crossGoto of
    Left err -> runAssert ctx "Cross-namespace GOTO FINISH^MATH" False (show err)
    Right env -> assertEqual ctx "Cross-namespace GOTO FINISH^MATH output" (getOutputText env) "start\ndone\n"

  -- Bare GOTO ^ROUTINE (line-0 entry) stays unsupported, matching DO/CALL.
  crossBare <- execScript " GOTO ^MATH"
  assertEqual ctx "GOTO ^MATH -> <UNDEFINED ROUTINE>" (either id (const "ok") crossBare) (show (UndefinedRoutine "MATH"))

  -- 28. Reference parameter whose formal name equals the caller's
  -- variable name. 'aliasVar' used to map it to itself, and
  -- 'resolveLocalName' then looped forever on the first read.
  let Right envSameName = defineItem "_GLOBAL" "BUMP" KindFunction (Just ["x"]) " SET x=x+1\n QUIT" adminEnv
  sameNameResult <- runRoutine (case mapM parseLogicalLine (buildLogicalLines " SET x=41\n DO BUMP(&x)\n WRITE x,!") of Right ls -> ls; Left _ -> []) envSameName
  case sameNameResult of
    Left err -> runAssert ctx "Reference parameter with same name as caller variable" False (show err)
    Right env -> assertEqual ctx "Reference parameter with same name as caller variable output" (getOutputText env) "42\n"

testCatalogGroup :: TestContext -> IO ()
testCatalogGroup ctx = do
  adminEnv <- testAdminEnv
  -- 1. Define function in _GLOBAL (Version 1)
  let Right e1 = defineItem "_GLOBAL" "DOUBLE" KindFunction (Just ["X"]) " WRITE X*2,!" adminEnv
  assertEqual ctx "Catalog: lookup DOUBLE v1"
    (lookupItem "_GLOBAL" "DOUBLE" e1)
    (Just (KindFunction, ["X"], " WRITE X*2,!"))
  assertEqual ctx "Catalog: version 1 counter"
    (readRef Global "_CATALOG" [mkValue "_GLOBAL", mkValue "DOUBLE", mkValue "VERSION"] e1)
    (Right (mkValue "1"))
  assertEqual ctx "Catalog: v1 has empty history"
    (getItemHistory "_GLOBAL" "DOUBLE" e1)
    []

  -- 2. Update function in _GLOBAL (Version 2)
  let Right e2 = defineItem "_GLOBAL" "DOUBLE" KindFunction (Just ["X"]) " WRITE X*3,!" e1
  assertEqual ctx "Catalog: lookup DOUBLE v2"
    (lookupItem "_GLOBAL" "DOUBLE" e2)
    (Just (KindFunction, ["X"], " WRITE X*3,!"))
  assertEqual ctx "Catalog: version 2 counter"
    (readRef Global "_CATALOG" [mkValue "_GLOBAL", mkValue "DOUBLE", mkValue "VERSION"] e2)
    (Right (mkValue "2"))
  assertEqual ctx "Catalog: v2 archives v1 in history"
    (getItemHistory "_GLOBAL" "DOUBLE" e2)
    [(1, " WRITE X*2,!")]

  -- 3. Revert to Version 1 (creates Version 3 with v1 content)
  case revertItem "_GLOBAL" "DOUBLE" 1 e2 of
    Left err -> runAssert ctx "Catalog revertItem" False (show err)
    Right e3 -> do
      assertEqual ctx "Catalog: reverted content matches v1"
        (lookupItem "_GLOBAL" "DOUBLE" e3)
        (Just (KindFunction, ["X"], " WRITE X*2,!"))
      assertEqual ctx "Catalog: version after revert is 3"
        (readRef Global "_CATALOG" [mkValue "_GLOBAL", mkValue "DOUBLE", mkValue "VERSION"] e3)
        (Right (mkValue "3"))
      assertEqual ctx "Catalog: history contains v1 and v2"
        (getItemHistory "_GLOBAL" "DOUBLE" e3)
        [(1, " WRITE X*2,!"), (2, " WRITE X*3,!")]

  -- 4. Namespaces & Fallback Resolution
  let Right eNS = defineItem "Billing" "CALCTAX" KindFunction (Just ["AMT"]) " WRITE AMT*0.2,!" e2
  assertEqual ctx "Catalog: lookup in specific namespace"
    (lookupItem "Billing" "CALCTAX" eNS)
    (Just (KindFunction, ["AMT"], " WRITE AMT*0.2,!"))
  assertEqual ctx "Catalog: fallback from Billing to _GLOBAL for DOUBLE"
    (lookupItem "Billing" "DOUBLE" eNS)
    (Just (KindFunction, ["X"], " WRITE X*3,!"))
  assertEqual ctx "Catalog: lookup in unrelated namespace fails"
    (lookupItem "Inventory" "CALCTAX" eNS)
    Nothing

  -- 5. Constants in Catalog
  let Right eConst = defineItem "_GLOBAL" "PI" KindConstant Nothing "3.14159" e2
  piResult <- evalState (evalExpr (Var (mkRef Local "PI" []))) eConst
  case piResult of
    Left err  -> runAssert ctx "Constant PI evaluation" False (show err)
    Right val -> assertEqual ctx "Constant PI value" val (mkValue "3.14159")

  -- 6. Execute Function via DO dispatch from ^_CATALOG
  let callScript = " DO DOUBLE(21)"
  case mapM parseLogicalLine (buildLogicalLines callScript) of
    Left pErr -> runAssert ctx "DO DOUBLE parse" False (show pErr)
    Right ast -> do
      doResult <- runRoutine ast e1
      case doResult of
        Left evalErr -> runAssert ctx "DO DOUBLE execution" False (show evalErr)
        Right envEnd -> assertEqual ctx "DO DOUBLE(21) output" (getOutputText envEnd) "42\n"

  -- 7. Native Global Queries on ^_CATALOG
  assertEqual ctx "$DATA on ^_CATALOG node duality"
    (dataRef Global "_CATALOG" [mkValue "_GLOBAL", mkValue "DOUBLE"] e2)
    11
  assertEqual ctx "listNamespaces from ^_CATALOG"
    (listNamespaces eNS)
    ["Billing", "_GLOBAL"]
  assertEqual ctx "listItems in Billing namespace"
    (listItems "Billing" eNS)
    [("CALCTAX", KindFunction, 1)]

testStorageGroup :: TestContext -> IO ()
testStorageGroup ctx = do
  adminEnv <- testAdminEnv
  let dummyTime = read "2026-01-01 00:00:00 UTC"

  -- 1. In-memory MVCC Storage & Revision Chain
  let eng0 = initStorageEngine
  let Right (tx1, eng1) = commitSet "TESTADMIN" "ORDERS" [mkValue "1", mkValue "PRICE"] (mkValue "100") Nothing dummyTime eng0
  let Right (tx2, eng2) = commitSet "TESTADMIN" "ORDERS" [mkValue "1", mkValue "PRICE"] (mkValue "120") Nothing dummyTime eng1
  let Right (tx3, eng3) = commitSet "TESTADMIN" "ORDERS" [mkValue "1", mkValue "PRICE"] (mkValue "150") Nothing dummyTime eng2

  assertEqual ctx "Storage: Tx sequence 1" tx1 1
  assertEqual ctx "Storage: Tx sequence 2" tx2 2
  assertEqual ctx "Storage: Tx sequence 3" tx3 3

  -- Live vs Time Travel reads
  assertEqual ctx "Storage: Live read is latest (150)"
    (readLive "ORDERS" [mkValue "1", mkValue "PRICE"] eng3)
    (Right (mkValue "150"))
  tt1 <- readAtTx 1 "ORDERS" [mkValue "1", mkValue "PRICE"] eng3
  assertEqual ctx "Storage: Time-travel read at tx 1 (100)" tt1 (Right (mkValue "100"))
  tt2 <- readAtTx 2 "ORDERS" [mkValue "1", mkValue "PRICE"] eng3
  assertEqual ctx "Storage: Time-travel read at tx 2 (120)" tt2 (Right (mkValue "120"))
  tt3 <- readAtTx 3 "ORDERS" [mkValue "1", mkValue "PRICE"] eng3
  assertEqual ctx "Storage: Time-travel read at tx 3 (150)" tt3 (Right (mkValue "150"))
  tt0 <- readAtTx 0 "ORDERS" [mkValue "1", mkValue "PRICE"] eng3
  assertIsLeft ctx "Storage: Time-travel read before creation (tx 0) is undefined" tt0

  -- 2. Evaluator Time Travel Integration (^[tx]GLOBAL(subs))
  -- NOTE: transaction numbers here are RELATIVE to whatever
  -- 'adminEnv' already starts at (bootstrap itself consumes a few
  -- transactions writing the test admin's role/hash/salt), they are
  -- NOT hardcoded absolute literals, since "the first SET in this
  -- script is tx 1" stopped being true the moment bootstrap became
  -- part of every test env's setup.
  let baseTx = seLastTx (envStorage adminEnv)
      tx1 = baseTx + 1
      tx2 = baseTx + 2
      script = unlines
        [ " SET ^ITEM(1)=\"Alpha\""
        , " SET ^ITEM(1)=\"Beta\""
        , " SET ^ITEM(1)=\"Gamma\""
        , " WRITE ^[" ++ show tx1 ++ "]ITEM(1),\",\",^[" ++ show tx2 ++ "]ITEM(1),\",\",^ITEM(1),!"
        ]
  case mapM parseLogicalLine (buildLogicalLines (T.pack script)) of
    Left pErr -> runAssert ctx "Time Travel Script Parse" False (show pErr)
    Right ast -> do
      result <- runRoutine ast adminEnv
      case result of
        Left eErr  -> runAssert ctx "Time Travel Script Exec" False (show eErr)
        Right env' -> assertEqual ctx "Time Travel Expression Evaluation" (getOutputText env') "Alpha,Beta,Gamma\n"

  -- 2b. Relative revision addressing (docs/tx-pinning-design.md Part
  -- 1): ^[-N]/^[LAST]/^[LAST-N], reusing the SAME Alpha/Beta/Gamma
  -- history from the absolute-tx test just above. Negative numbers are
  -- otherwise meaningless as a transaction id, so they were free to
  -- repurpose as "N revisions back" rather than reject outright.
  let relScript = unlines
        [ " SET ^ITEM(1)=\"Alpha\""
        , " SET ^ITEM(1)=\"Beta\""
        , " SET ^ITEM(1)=\"Gamma\""
        , " WRITE ^[LAST]ITEM(1),\",\",^[-1]ITEM(1),\",\",^[-2]ITEM(1),\",\",^[LAST-1]ITEM(1),!"
        ]
  case mapM parseLogicalLine (buildLogicalLines (T.pack relScript)) of
    Left pErr -> runAssert ctx "Relative revision addressing parse" False (show pErr)
    Right ast -> do
      result <- runRoutine ast adminEnv
      case result of
        Left eErr  -> runAssert ctx "Relative revision addressing exec" False (show eErr)
        Right env' -> assertEqual ctx "^[LAST]/^[-1]/^[-2]/^[LAST-1] against Alpha/Beta/Gamma"
          (getOutputText env') "Gamma,Beta,Alpha,Beta\n"

  -- Going back further than this path's recorded history (only 3
  -- revisions exist here) resolves to transaction 0 -- "before this
  -- path existed" -- which an ordinary absolute-tx read already
  -- treats as undefined, so no separate "out of range" error exists.
  let relOutOfRangeScript = " SET ^ITEM(1)=\"Alpha\"\n SET ^ITEM(1)=\"Beta\"\n WRITE ^[-5]ITEM(1),!"
  case mapM parseLogicalLine (buildLogicalLines (T.pack relOutOfRangeScript)) of
    Left pErr -> runAssert ctx "Relative revision out-of-range parse" False (show pErr)
    Right ast -> do
      result <- runRoutine ast adminEnv
      case result of
        Left (UndefinedVariable _ _) -> runAssert ctx "^[-N] further back than history exists is undefined" True ""
        other -> runAssert ctx "^[-N] further back than history exists is undefined" False (show other)

  -- A KILL counts as one step back, same as any other revision: right
  -- after KILL, ^[-1] is the tombstone (undefined); ^[-2] reaches
  -- behind it to the value that was killed.
  let relKillScript = " SET ^ITEM(2)=\"Before\"\n KILL ^ITEM(2)\n SET ^ITEM(2)=\"After\"\n WRITE ^[LAST]ITEM(2),\",\",^[-2]ITEM(2),!"
  case mapM parseLogicalLine (buildLogicalLines (T.pack relKillScript)) of
    Left pErr -> runAssert ctx "Relative revision through a KILL parse" False (show pErr)
    Right ast -> do
      result <- runRoutine ast adminEnv
      case result of
        Left eErr  -> runAssert ctx "Relative revision through a KILL exec" False (show eErr)
        Right env' -> assertEqual ctx "^[-2] reaches behind a KILL's tombstone to the earlier value"
          (getOutputText env') "After,Before\n"
  let relKillTombstoneScript = " SET ^ITEM(3)=\"Before\"\n KILL ^ITEM(3)\n SET ^ITEM(3)=\"After\"\n WRITE ^[-1]ITEM(3),!"
  case mapM parseLogicalLine (buildLogicalLines (T.pack relKillTombstoneScript)) of
    Left pErr -> runAssert ctx "Relative revision at a KILL tombstone parse" False (show pErr)
    Right ast -> do
      result <- runRoutine ast adminEnv
      case result of
        Left (UndefinedVariable _ _) -> runAssert ctx "^[-1] right after a KILL is the tombstone (undefined)" True ""
        other -> runAssert ctx "^[-1] right after a KILL is the tombstone (undefined)" False (show other)

  -- An ordinary expression that merely evaluates negative is ALSO
  -- relative -- not just the LAST-N spelling.
  let relArithScript = " SET ^ITEM(4)=\"Alpha\"\n SET ^ITEM(4)=\"Beta\"\n WRITE ^[0-1]ITEM(4),!"
  case mapM parseLogicalLine (buildLogicalLines (T.pack relArithScript)) of
    Left pErr -> runAssert ctx "Relative revision via ordinary negative arithmetic parse" False (show pErr)
    Right ast -> do
      result <- runRoutine ast adminEnv
      case result of
        Left eErr  -> runAssert ctx "Relative revision via ordinary negative arithmetic exec" False (show eErr)
        Right env' -> assertEqual ctx "^[0-1] (an ordinary expression evaluating to -1) means one revision back"
          (getOutputText env') "Alpha\n"

  -- 2c. DO WITH @TX(...) pinned blocks (docs/tx-pinning-design.md
  -- Part 2). Alpha/Beta/Gamma again; every bare GLOBAL read inside the
  -- dotted block is answered as of the pinned revision.
  let runPhaseIn env0 src = case mapM parseLogicalLine (buildLogicalLines (T.pack src)) of
        Left pErr -> pure (Left (SyntaxError (T.pack (show pErr))))
        Right ast -> runRoutine ast env0
      abg = " SET ^ITEM(1)=\"Alpha\"\n SET ^ITEM(1)=\"Beta\"\n SET ^ITEM(1)=\"Gamma\"\n"
      expectOut name env0 src expected = do
        r <- runPhaseIn env0 src
        case r of
          Left e     -> runAssert ctx name False (show e)
          Right env' -> assertEqual ctx name (getOutputText env') expected

  expectOut "DO WITH @TX(^[-2]...) pins bare reads inside the block, not after it" adminEnv
    (abg ++ " DO WITH @TX(^[-2]ITEM(1))\n. WRITE ^ITEM(1),!\n WRITE ^ITEM(1),!\n")
    "Alpha\nGamma\n"

  expectOut "^[LAST] inside a pinned block is the escape hatch back to live data" adminEnv
    (abg ++ " DO WITH @TX(^[-2]ITEM(1))\n. WRITE ^ITEM(1),\",\",^[LAST]ITEM(1),!\n")
    "Alpha,Gamma\n"

  -- Inside a pin, the implicit baseline for ^[-N] is the pinned
  -- revision; LAST-N still counts from the true live latest.
  expectOut "^[-N] inside a pin counts from the pin; ^[LAST-N] counts from live" adminEnv
    (abg ++ " DO WITH @TX(^[-1]ITEM(1))\n. WRITE ^[-1]ITEM(1),\",\",^[LAST-1]ITEM(1),!\n")
    "Alpha,Beta\n"

  -- Writes always land live; the pin only governs reads, so you do NOT
  -- see your own write inside the block (snapshot semantics).
  expectOut "SET inside a pinned block lands live and is invisible to the pinned reads" adminEnv
    (abg ++ " DO WITH @TX(^[-2]ITEM(1))\n. SET ^ITEM(1)=\"Delta\"\n. WRITE ^ITEM(1),!\n WRITE ^ITEM(1),!\n")
    "Alpha\nDelta\n"

  -- Locals are never pinned.
  expectOut "locals are unaffected by a pin" adminEnv
    (abg ++ " SET X=\"live\"\n DO WITH @TX(^[-2]ITEM(1))\n. SET X=\"changed\"\n WRITE X,!\n")
    "changed\n"

  expectOut "a nested DO WITH @TX(LAST) un-pins for its own block" adminEnv
    (abg ++ " DO WITH @TX(^[-2]ITEM(1))\n. WRITE ^ITEM(1),\",\"\n. DO WITH @TX(LAST)\n.. WRITE ^ITEM(1),!\n")
    "Alpha,Gamma\n"

  -- A nested pin's own argument is resolved THROUGH the outer pin:
  -- unbracketed ^ITEM(1) there means the revision in effect at the
  -- outer pin (Beta's), not the live one, so the inner pin lands on
  -- Beta and ^[-1] from it is Alpha.
  expectOut "a nested pin's argument is evaluated through the enclosing pin" adminEnv
    (abg ++ " DO WITH @TX(^[-1]ITEM(1))\n. DO WITH @TX(^ITEM(1))\n.. WRITE ^ITEM(1),\",\",^[-1]ITEM(1),!\n")
    "Beta,Alpha\n"

  -- The pin travels through a CALL made inside the block.
  let Right envShow = defineItem "_GLOBAL" "SHOW" KindFunction (Just []) " RETURN ^ITEM(1)" adminEnv
  expectOut "the pin carries through a CALL made inside the block" envShow
    (abg ++ " DO WITH @TX(^[-2]ITEM(1))\n. WRITE CALL SHOW(),!\n WRITE CALL SHOW(),!\n")
    "Alpha\nGamma\n"

  -- $DATA and $GET inherit the pin: ^NEW did not exist yet at Alpha's tx.
  expectOut "$DATA/$GET inside a pinned block see the pinned past" adminEnv
    (" SET ^ITEM(1)=\"Alpha\"\n SET ^ITEM(1)=\"Beta\"\n SET ^NEW(1)=\"later\"\n DO WITH @TX(^[-1]ITEM(1))\n. WRITE $DATA(^NEW(1)),$GET(^ITEM(1)),!\n WRITE $DATA(^NEW(1)),!\n")
    "0Alpha\n1\n"

  -- An absolute transaction number pins too.
  let baseTx2 = seLastTx (envStorage adminEnv)
  expectOut "DO WITH @TX(<absolute tx>) pins to that transaction" adminEnv
    (" SET ^ITEM(1)=\"Alpha\"\n SET ^ITEM(1)=\"Beta\"\n DO WITH @TX(" ++ show (baseTx2 + 1) ++ ")\n. WRITE ^ITEM(1),!\n")
    "Alpha\n"

  -- $ORDER iterates the children as they stood at the pin: ^L(2) is
  -- killed and ^L(4) added AFTER the pinned point, so a pinned walk must
  -- still see 1,2,3 while the live walk sees 1,3,4. The pin is the
  -- transaction of the third SET (^L(3)), by absolute number.
  let orderSrc loopBody = " SET ^L(1)=\"a\"\n SET ^L(2)=\"b\"\n SET ^L(3)=\"c\"\n"
        ++ " KILL ^L(2)\n SET ^L(4)=\"d\"\n"
        ++ loopBody
      walkLoop = "FOR  SET K=$ORDER(^L(K)) QUIT:K=\"\"  WRITE K,\",\"\n"
      txOfL3 = seLastTx (envStorage adminEnv) + 3
  expectOut "live $ORDER walk sees the current children" adminEnv
    (orderSrc (" SET K=\"\"\n " ++ walkLoop ++ " WRITE !\n"))
    "1,3,4,\n"
  expectOut "$ORDER inside a pinned block walks the children as of the pin" adminEnv
    (orderSrc (" DO WITH @TX(" ++ show txOfL3 ++ ")\n. SET K=\"\"\n. " ++ walkLoop ++ " WRITE !\n"))
    "1,2,3,\n"
  expectOut "explicit $ORDER(^[tx]...) walks the children as of that transaction" adminEnv
    (orderSrc (" SET K=\"\"\n FOR  SET K=$ORDER(^[" ++ show txOfL3 ++ "]L(K)) QUIT:K=\"\"  WRITE K,\",\"\n WRITE !\n"))
    "1,2,3,\n"

  -- 2d. DO WITH @TX(...) @BIND(...) (docs/tx-pinning-design.md,
  -- "Explicit global bindings"). A bind list selects WHICH globals the pin
  -- applies to; every other global reads live. Revisions, oldest first:
  --   t+1 ^B=b1   t+2 ^A=a1   t+3 ^B=b2   t+4 ^A=a2   t+5 ^B=b3
  -- so live is a2/b3, and @TX(^[-1]A(1)) (evaluated live) pins to t+2,
  -- where A was a1 and B was b1.
  let pairSrc = " SET ^B(1)=\"b1\"\n SET ^A(1)=\"a1\"\n SET ^B(1)=\"b2\"\n SET ^A(1)=\"a2\"\n SET ^B(1)=\"b3\"\n"
      pinA = "@TX(^[-1]A(1))"
      showAB = " WRITE ^A(1),\",\",^B(1),!\n"
      baseTx3 = seLastTx (envStorage adminEnv)

  expectOut "@BIND(^A): the bound global is pinned, the unbound one reads live" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n." ++ showAB)
    "a1,b3\n"

  expectOut "no @BIND clause pins every global (unchanged behavior)" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ "\n." ++ showAB)
    "a1,b1\n"

  expectOut "@BIND(^B): binding the other global pins that one instead" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^B)\n." ++ showAB)
    "a2,b1\n"

  expectOut "an empty @BIND() binds nothing, so everything reads live" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND()\n." ++ showAB)
    "a2,b3\n"

  -- Nesting: a narrower inner list leaves what only the outer pinned at
  -- the outer pin; @TX(LAST) releases just the globals the inner names.
  expectOut "a narrower inner @BIND keeps the outer pin on globals it does not name" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A,^B)\n. DO WITH @TX(LAST) @BIND(^A)\n.." ++ showAB)
    "a2,b1\n"

  -- An omitted inner list re-pins what the enclosing list bound -- and
  -- nothing else: ^B stays live rather than being swept into the pin.
  expectOut "an omitted inner @BIND re-pins only the previously bound globals" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. DO WITH @TX(" ++ show (baseTx3 + 4) ++ ")\n.." ++ showAB)
    "a2,b3\n"

  expectOut "an omitted inner @BIND with @TX(LAST) releases everything the outer bound" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A,^B)\n. DO WITH @TX(LAST)\n.." ++ showAB)
    "a2,b3\n"

  expectOut "with no enclosing bind list a nested @BIND may name any global" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ "\n. DO WITH @TX(LAST) @BIND(^B)\n.." ++ showAB)
    "a1,b3\n"

  do r <- runPhaseIn adminEnv (pairSrc ++ " DO WITH @TX(LAST) @BIND(^A)\n. DO WITH @TX(LAST) @BIND(^A,^B)\n.. WRITE 1,!\n")
     case r of
       Left (BindScope "B") -> runAssert ctx "a nested @BIND naming a global the outer list did not bind is <BIND SCOPE>" True ""
       other                -> runAssert ctx "a nested @BIND naming a global the outer list did not bind is <BIND SCOPE>" False (show (fmap getOutputText other))
  assertEqual ctx "<BIND SCOPE> error text" (formatError (BindScope "B"))
    "<BIND SCOPE> ^B is not bound by the enclosing DO WITH / CALL WITH"

  -- Relative addressing is per global: ^[-N] counts back from the pin
  -- for a bound global, but from the live latest for an unbound one.
  expectOut "^[-N] counts from the pin for a bound global and from live for an unbound one" adminEnv
    (pairSrc ++ " DO WITH @TX(^A(1)) @BIND(^A)\n. WRITE ^[-1]A(1),\",\",^[-1]B(1),!\n")
    "a1,b2\n"

  -- $DATA / $GET read through the per-global pin too. ^NEW appears after
  -- the pin point: bound, it does not exist yet; unbound, it does.
  let dataSrc = pairSrc ++ " SET ^NEW(1)=\"x\"\n"
  expectOut "$DATA of an unbound global is live inside a bound block" adminEnv
    (dataSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. WRITE $DATA(^NEW(1)),$DATA(^A(1)),!\n")
    "11\n"
  expectOut "$DATA of a bound global is as of the pin" adminEnv
    (dataSrc ++ " DO WITH " ++ pinA ++ " @BIND(^NEW)\n. WRITE $DATA(^NEW(1)),$DATA(^A(1)),!\n")
    "01\n"

  -- Single-call forms: one call inside its own pin scope.
  let Right envBind1 = defineItem "_GLOBAL" "SHOWAB" KindFunction (Just []) " RETURN ^A(1)_\",\"_^B(1)" adminEnv
      Right envBind  = defineItem "_GLOBAL" "SHOWW" KindFunction (Just []) (T.pack (dropWhileEnd' showAB)) envBind1
      dropWhileEnd' = reverse . dropWhile (== '\n') . reverse
  expectOut "DO WITH @TX(...) @BIND(...) TAG() pins one call; the pin carries into it" envBind
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A) SHOWW()\n")
    "a1,b3\n"
  expectOut "DO WITH @TX(LAST) TAG() releases a block's pin for one call only" envBind
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. DO SHOWW()\n. DO WITH @TX(LAST) SHOWW()\n. DO SHOWW()\n")
    "a1,b3\na2,b3\na1,b3\n"
  expectOut "CALL WITH @TX(...) @BIND(...) is an expression-position pinned call" envBind
    (pairSrc ++ " WRITE CALL WITH " ++ pinA ++ " @BIND(^A) SHOWAB(),!\n")
    "a1,b3\n"
  expectOut "CALL WITH @TX(LAST) releases a block-wide pin for that call" envBind
    (pairSrc ++ " DO WITH " ++ pinA ++ "\n. WRITE CALL SHOWAB(),\"|\",CALL WITH @TX(LAST) SHOWAB(),!\n")
    "a1,b1|a2,b3\n"

  -- A bind list must name whole globals.
  do r <- runPhaseIn adminEnv (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A(\"x\"))\n. WRITE 1,!\n")
     case r of
       Left (SyntaxError _) -> runAssert ctx "a subscripted item in @BIND(...) is a parse error" True ""
       other                -> runAssert ctx "a subscripted item in @BIND(...) is a parse error" False (show (fmap getOutputText other))

  -- Phase.Lines: a bind list after the pin still opens a block; a single
  -- pinned call (ending in the call) does not.
  assertEqual ctx "continuation: DO WITH @TX(...) @BIND(...) opens a block"
    (needsContinuation " DO WITH @TX(LAST) @BIND(^A,^B)") True
  assertEqual ctx "continuation: DO WITH @TX(...) @BIND() (empty list) opens a block"
    (needsContinuation " DO WITH @TX(LAST) @BIND()") True
  assertEqual ctx "continuation: DO WITH @TX(...) TAG() is a one-line call, not a block"
    (needsContinuation " DO WITH @TX(LAST) SHOWAB()") False
  assertEqual ctx "continuation: DO WITH @TX(...) @BIND(...) TAG() is a one-line call, not a block"
    (needsContinuation " DO WITH @TX(LAST) @BIND(^A) SHOWAB()") False
  assertEqual ctx "DO WITH @TX(...) @BIND(...) opens a block in Phase.Lines (depths 0 and 1)"
    (map llDepth (buildLogicalLines " DO WITH @TX(^[-2]ITEM(1)) @BIND(^ITEM)\n. WRITE 1"))
    [0, 1]

  -- 2d. DO WITH @TX(...) @BIND(...) (docs/tx-pinning-design.md,
  -- "Explicit global bindings"). A bind list selects WHICH globals the pin
  -- applies to; every other global reads live. Revisions, oldest first:
  --   t+1 ^B=b1   t+2 ^A=a1   t+3 ^B=b2   t+4 ^A=a2   t+5 ^B=b3
  -- so live is a2/b3, and @TX(^[-1]A(1)) (evaluated live) pins to t+2,
  -- where A was a1 and B was b1.
  let pairSrc = " SET ^B(1)=\"b1\"\n SET ^A(1)=\"a1\"\n SET ^B(1)=\"b2\"\n SET ^A(1)=\"a2\"\n SET ^B(1)=\"b3\"\n"
      pinA = "@TX(^[-1]A(1))"
      showAB = " WRITE ^A(1),\",\",^B(1),!\n"
      baseTx3 = seLastTx (envStorage adminEnv)

  expectOut "@BIND(^A): the bound global is pinned, the unbound one reads live" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n." ++ showAB)
    "a1,b3\n"

  expectOut "no @BIND clause pins every global (unchanged behavior)" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ "\n." ++ showAB)
    "a1,b1\n"

  expectOut "@BIND(^B): binding the other global pins that one instead" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^B)\n." ++ showAB)
    "a2,b1\n"

  expectOut "an empty @BIND() binds nothing, so everything reads live" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND()\n." ++ showAB)
    "a2,b3\n"

  -- Nesting: a narrower inner list leaves what only the outer pinned at
  -- the outer pin; @TX(LAST) releases just the globals the inner names.
  expectOut "a narrower inner @BIND keeps the outer pin on globals it does not name" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A,^B)\n. DO WITH @TX(LAST) @BIND(^A)\n.." ++ showAB)
    "a2,b1\n"

  -- An omitted inner list re-pins what the enclosing list bound -- and
  -- nothing else: ^B stays live rather than being swept into the pin.
  expectOut "an omitted inner @BIND re-pins only the previously bound globals" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. DO WITH @TX(" ++ show (baseTx3 + 4) ++ ")\n.." ++ showAB)
    "a2,b3\n"

  expectOut "an omitted inner @BIND with @TX(LAST) releases everything the outer bound" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A,^B)\n. DO WITH @TX(LAST)\n.." ++ showAB)
    "a2,b3\n"

  expectOut "with no enclosing bind list a nested @BIND may name any global" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ "\n. DO WITH @TX(LAST) @BIND(^B)\n.." ++ showAB)
    "a1,b3\n"

  do r <- runPhaseIn adminEnv (pairSrc ++ " DO WITH @TX(LAST) @BIND(^A)\n. DO WITH @TX(LAST) @BIND(^A,^B)\n.. WRITE 1,!\n")
     case r of
       Left (BindScope "B") -> runAssert ctx "a nested @BIND naming a global the outer list did not bind is <BIND SCOPE>" True ""
       other                -> runAssert ctx "a nested @BIND naming a global the outer list did not bind is <BIND SCOPE>" False (show (fmap getOutputText other))
  assertEqual ctx "<BIND SCOPE> error text" (formatError (BindScope "B"))
    "<BIND SCOPE> ^B is not bound by the enclosing DO WITH / CALL WITH"

  -- Relative addressing is per global: ^[-N] counts back from the pin
  -- for a bound global, but from the live latest for an unbound one.
  expectOut "^[-N] counts from the pin for a bound global and from live for an unbound one" adminEnv
    (pairSrc ++ " DO WITH @TX(^A(1)) @BIND(^A)\n. WRITE ^[-1]A(1),\",\",^[-1]B(1),!\n")
    "a1,b2\n"

  -- $DATA / $GET read through the per-global pin too. ^NEW appears after
  -- the pin point: bound, it does not exist yet; unbound, it does.
  let dataSrc = pairSrc ++ " SET ^NEW(1)=\"x\"\n"
  expectOut "$DATA of an unbound global is live inside a bound block" adminEnv
    (dataSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. WRITE $DATA(^NEW(1)),$DATA(^A(1)),!\n")
    "11\n"
  expectOut "$DATA of a bound global is as of the pin" adminEnv
    (dataSrc ++ " DO WITH " ++ pinA ++ " @BIND(^NEW)\n. WRITE $DATA(^NEW(1)),$DATA(^A(1)),!\n")
    "01\n"

  -- $ORDER walks the children as of the pin only for a BOUND global.
  expectOut "$ORDER of a bound global walks the children as of the pin" adminEnv
    (orderSrc (" DO WITH @TX(" ++ show txOfL3 ++ ") @BIND(^L)\n. SET K=\"\"\n. " ++ walkLoop ++ " WRITE !\n"))
    "1,2,3,\n"
  expectOut "$ORDER of an unbound global walks the live children inside a bound block" adminEnv
    (orderSrc (" DO WITH @TX(" ++ show txOfL3 ++ ") @BIND(^OTHER)\n. SET K=\"\"\n. " ++ walkLoop ++ " WRITE !\n"))
    "1,3,4,\n"

  -- Dynamic (REF) and naked references resolve to a name first, and the
  -- per-global pin is looked up by that name.
  expectOut "REF(\"^A\",...) honours the per-global pin of the name it resolves to" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. WRITE REF(\"^A\",1),\",\",REF(\"^B\",1),!\n")
    "a1,b3\n"
  expectOut "a naked reference follows the pin of the global it resolves to (bound)" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^B)\n. WRITE ^B(1),\",\",^(1),!\n")
    "b1,b1\n"
  expectOut "a naked reference follows the pin of the global it resolves to (unbound)" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^B)\n. WRITE ^A(1),\",\",^(1),!\n")
    "a2,a2\n"

  -- Namespace-qualified bind names match the namespace-qualified global
  -- only; a bare ^A names a different global.
  let nsSrc = " SET ^B(1)=\"b1\"\n SET NS^A(1)=\"a1\"\n SET ^B(1)=\"b2\"\n SET NS^A(1)=\"a2\"\n"
      showNS = " WRITE NS^A(1),\",\",^B(1),!\n"
  expectOut "@BIND(NS^A) pins the namespace-qualified global" adminEnv
    (nsSrc ++ " DO WITH @TX(NS^[-1]A(1)) @BIND(NS^A)\n." ++ showNS)
    "a1,b2\n"
  expectOut "@BIND(^A) does not bind the global NS^A" adminEnv
    (nsSrc ++ " DO WITH @TX(NS^[-1]A(1)) @BIND(^A)\n." ++ showNS)
    "a2,b2\n"

  -- The enclosing scope is restored on the way out of every block.
  expectOut "the pin scope ends with the block: later reads are live again" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. WRITE ^A(1),!\n WRITE ^A(1),!\n")
    "a1\na2\n"
  expectOut "leaving a nested block restores the outer scope's pins" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A,^B)\n. DO WITH @TX(LAST) @BIND(^A)\n.. WRITE ^A(1),!\n. WRITE ^A(1),!\n")
    "a2\na1\n"

  -- Single-call forms: one call inside its own pin scope.
  let Right envBind1 = defineItem "_GLOBAL" "SHOWAB" KindFunction (Just []) " RETURN ^A(1)_\",\"_^B(1)" adminEnv
      Right envBind  = defineItem "_GLOBAL" "SHOWW" KindFunction (Just []) (T.pack (dropWhileEnd' showAB)) envBind1
      dropWhileEnd' = reverse . dropWhile (== '\n') . reverse
  expectOut "DO WITH @TX(...) @BIND(...) TAG() pins one call; the pin carries into it" envBind
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A) SHOWW()\n")
    "a1,b3\n"
  expectOut "DO WITH @TX(LAST) TAG() releases a block's pin for one call only" envBind
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. DO SHOWW()\n. DO WITH @TX(LAST) SHOWW()\n. DO SHOWW()\n")
    "a1,b3\na2,b3\na1,b3\n"
  expectOut "CALL WITH @TX(...) @BIND(...) is an expression-position pinned call" envBind
    (pairSrc ++ " WRITE CALL WITH " ++ pinA ++ " @BIND(^A) SHOWAB(),!\n")
    "a1,b3\n"
  expectOut "CALL WITH @TX(LAST) releases a block-wide pin for that call" envBind
    (pairSrc ++ " DO WITH " ++ pinA ++ "\n. WRITE CALL SHOWAB(),\"|\",CALL WITH @TX(LAST) SHOWAB(),!\n")
    "a1,b1|a2,b3\n"

  -- A bind list must name whole globals.
  do r <- runPhaseIn adminEnv (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A(\"x\"))\n. WRITE 1,!\n")
     case r of
       Left (SyntaxError _) -> runAssert ctx "a subscripted item in @BIND(...) is a parse error" True ""
       other                -> runAssert ctx "a subscripted item in @BIND(...) is a parse error" False (show (fmap getOutputText other))

  -- Phase.Lines: a bind list after the pin still opens a block; a single
  -- pinned call (ending in the call) does not.
  assertEqual ctx "continuation: DO WITH @TX(...) @BIND(...) opens a block"
    (needsContinuation " DO WITH @TX(LAST) @BIND(^A,^B)") True
  assertEqual ctx "continuation: DO WITH @TX(...) @BIND() (empty list) opens a block"
    (needsContinuation " DO WITH @TX(LAST) @BIND()") True
  assertEqual ctx "continuation: DO WITH @TX(...) TAG() is a one-line call, not a block"
    (needsContinuation " DO WITH @TX(LAST) SHOWAB()") False
  assertEqual ctx "continuation: DO WITH @TX(...) @BIND(...) TAG() is a one-line call, not a block"
    (needsContinuation " DO WITH @TX(LAST) @BIND(^A) SHOWAB()") False
  assertEqual ctx "DO WITH @TX(...) @BIND(...) opens a block in Phase.Lines (depths 0 and 1)"
    (map llDepth (buildLogicalLines " DO WITH @TX(^[-2]ITEM(1)) @BIND(^ITEM)\n. WRITE 1"))
    [0, 1]

  -- 2e. MERGE into a global is a real commit (audit-notes.md #3). Before
  -- this fix 'mergeRef' wrote the merged tree straight into the live
  -- state: no TxId, no WAL records, no history, and a staged write to the
  -- same global was lost inside a transaction.
  let mergeSrc = " SET src(1)=\"x\",src(2)=\"y\"\n SET src(2,1)=\"z\"\n"   -- three leaves
      baseTxM  = seLastTx (envStorage adminEnv)
      pendingM = length (sePendingWal (envStorage adminEnv))

  do r <- runPhaseIn adminEnv (mergeSrc ++ " MERGE ^DST=src\n")
     case r of
       Left e -> runAssert ctx "MERGE of three leaves into a global" False (show e)
       Right env' -> do
         let st' = envStorage env'
             newRecs = drop pendingM (sePendingWal st')
         assertEqual ctx "MERGE of three leaves into a global is ONE commit (one TxId)"
           (seLastTx st') (baseTxM + 1)
         assertEqual ctx "MERGE queues one WalSet per merged leaf plus one WalCommit, all under that TxId"
           ( length [ () | WalRecord _ WalSet{} <- newRecs ]
           , length [ () | WalRecord _ WalCommit{} <- newRecs ]
           , all ((== baseTxM + 1) . wrTxId) newRecs )
           (3, 1, True)
         assertEqual ctx "MERGE: the merged values are readable afterwards"
           (map (\subs -> readRef Global "DST" (map mkValue subs) env') [["1"], ["2"], ["2", "1"]])
           (map (Right . mkValue) ["x", "y", "z"])

  expectOut "MERGE into a global records history: ^[-1] sees the value before the second MERGE" adminEnv
    (" SET src(1)=\"x\"\n MERGE ^DST=src\n SET src(1)=\"x2\"\n MERGE ^DST=src\n WRITE ^DST(1),\",\",^[-1]DST(1),!\n")
    "x2,x\n"

  do r <- runPhaseIn adminEnv " MERGE ^DST=nosuch\n"
     case r of
       Left e -> runAssert ctx "MERGE from an absent source is a no-op" False (show e)
       Right env' -> assertEqual ctx "MERGE from an absent source commits nothing"
                       (seLastTx (envStorage env'), length (sePendingWal (envStorage env')))
                       (baseTxM, pendingM)

  -- It reaches the disk: write through a real file engine, flush the way
  -- the REPL does, reopen, read it back.
  mergePath <- tmpPath "test_phasedb_merge.db"
  do e <- doesFileExist mergePath
     if e then removeFile mergePath else pure ()
  mOpen <- openStorageEngine mergePath
  case mOpen of
    Left e -> runAssert ctx "MERGE persistence: open a fresh file engine" False (show e)
    Right eng0 -> do
      nowM <- getCurrentTime
      boot <- bootstrapCreateUser "TESTADMIN" "testpassword123" RAdmin nowM eng0
      case boot of
        Left e -> runAssert ctx "MERGE persistence: bootstrap admin" False (T.unpack e)
        Right eng1 -> do
          let fileEnv = initEnv { envStorage = eng1, envGlobals = seLive eng1
                                , envCurrentUser = "TESTADMIN", envNow = nowM }
          r <- runPhaseIn fileEnv (mergeSrc ++ " MERGE ^DST=src\n")
          case r of
            Left e -> runAssert ctx "MERGE persistence: run the MERGE" False (show e)
            Right env' -> do
              _ <- flushPendingWal (envStorage env')
              reopened <- openStorageEngine mergePath
              case reopened of
                Left e -> runAssert ctx "MERGE persistence: reopen" False (show e)
                Right eng2 ->
                  assertEqual ctx "MERGE into a global survives a reopen (it reached the WAL)"
                    (map (\subs -> readLive "DST" (map mkValue subs) eng2) [["1"], ["2"], ["2", "1"]])
                    (map (Right . mkValue) ["x", "y", "z"])
  do e <- doesFileExist mergePath
     if e then removeFile mergePath else pure ()

  -- Inside an active transaction a MERGE stages, like SET does: the
  -- earlier staged write to the same global survives, every merged path
  -- is staged, and nothing reaches committed storage or the WAL queue.
  let txEnv0 = adminEnv { envActiveTx = Just (newTxHandle 0 dummyTime) }
      Right txEnv1 = writeRef Global "DST" [mkValue "a"] (mkValue "A") txEnv0
      Right txEnv2 = writeRef Local "src" [mkValue "1"] (mkValue "x") txEnv1
  case mergeRef (Global, "DST", []) (Local, "src", []) txEnv2 of
    Left e -> runAssert ctx "MERGE inside an active transaction" False (show e)
    Right e3 -> do
      assertEqual ctx "MERGE in a transaction keeps the transaction's earlier staged write to that global"
        (fmap dumpTree (Map.lookup "DST" (envGlobals e3)))
        (Just [([mkValue "1"], mkValue "x"), ([mkValue "a"], mkValue "A")])
      assertEqual ctx "MERGE in a transaction stages the merged path (2 staged writes in all)"
        (fmap (Map.size . txStagedWrites) (envActiveTx e3)) (Just 2)
      assertEqual ctx "MERGE in a transaction commits nothing (no TxId, no WAL records)"
        (seLastTx (envStorage e3), length (sePendingWal (envStorage e3)))
        (seLastTx (envStorage txEnv2), length (sePendingWal (envStorage txEnv2)))
      assertEqual ctx "MERGE in a transaction does not leak uncommitted data into live storage"
        (Map.member "DST" (seLive (envStorage e3))) False

  -- Permissions are checked per merged leaf, exactly as a SET would.
  -- TESTADMIN is the only admin, so demoting it must be refused whether
  -- it arrives by SET or by MERGE.
  do r <- runPhaseIn adminEnv " SET ^_USERS(\"TESTADMIN\",\"role\")=\"read\"\n"
     case r of
       Left (PermissionDenied _ _) -> runAssert ctx "control: SET demoting the last admin is refused" True ""
       other -> runAssert ctx "control: SET demoting the last admin is refused" False (show (fmap getOutputText other))
  do r <- runPhaseIn adminEnv " SET src(\"aaa\")=\"1\",src(\"role\")=\"read\"\n MERGE ^_USERS(\"TESTADMIN\")=src\n"
     case r of
       Left (PermissionDenied _ _) -> runAssert ctx "MERGE demoting the last admin is refused like a SET" True ""
       other -> runAssert ctx "MERGE demoting the last admin is refused like a SET" False (show (fmap getOutputText other))

  -- The MERGE source is read live even inside a pinned block (same as
  -- before; only plain reads follow the pin).
  expectOut "MERGE source reads are live inside a pinned block" adminEnv
    (pairSrc ++ " DO WITH " ++ pinA ++ " @BIND(^A)\n. MERGE m=^A\n. WRITE m(1),\",\",^A(1),!\n")
    "a2,a1\n"

  -- The REPL's multi-line continuation rule (Phase.Lines.needsContinuation).
  -- It used to count every DO word as an unclosed opener, so even a
  -- one-line `DO tag` swallowed the following input until EOF.
  assertEqual ctx "continuation: a plain one-line DO tag is complete" (needsContinuation " DO TAG") False
  assertEqual ctx "continuation: DO tag^ns(args) is complete" (needsContinuation " DO TAG^NS(1,2)") False
  assertEqual ctx "continuation: a bare DO opens a block" (needsContinuation " DO") True
  assertEqual ctx "continuation: DO WITH @TX(...) opens a block" (needsContinuation " DO WITH @TX(^[-2]ITEM(1))") True
  assertEqual ctx "continuation: a dotted block line keeps the block open" (needsContinuation " DO\n. WRITE 1") True
  assertEqual ctx "continuation: a blank line ends the block" (needsContinuation " DO\n. WRITE 1\n") False
  assertEqual ctx "continuation: a non-dotted line after the block ends it" (needsContinuation " DO\n. WRITE 1\n WRITE 2") False
  assertEqual ctx "continuation: an unsupported TRANSACTION line is not a block opener here" (needsContinuation " TRANSACTION START") False

  -- A bare negative number has no path to count back along.
  do r <- runPhaseIn adminEnv (abg ++ " DO WITH @TX(-1)\n. WRITE 1,!\n")
     case r of
       Left (SyntaxError _) -> runAssert ctx "@TX(-1) without a reference is an error" True ""
       other                -> runAssert ctx "@TX(-1) without a reference is an error" False (show (fmap getOutputText other))

  -- Phase.Lines must treat the opener as a block opener: the dotted
  -- line after it is a block (depth 1), not a continuation of it.
  assertEqual ctx "DO WITH @TX(...) opens a block in Phase.Lines (two logical lines, depths 0 and 1)"
    (map llDepth (buildLogicalLines " DO WITH @TX(^[-2]ITEM(1))\n. WRITE 1"))
    [0, 1]
  assertEqual ctx "a ')' inside a string in the @TX argument does not fool the block-opener check"
    (map llDepth (buildLogicalLines " DO WITH @TX(^[-2]ITEM(\")\"))\n. WRITE 1"))
    [0, 1]

  -- 3. Read-Only Time Travel Protection (<SET ON READONLY>)
  let badSetScript = " SET ^[1]ITEM(1)=\"Illegal\""
  case mapM parseLogicalLine (buildLogicalLines (T.pack badSetScript)) of
    Left pErr -> runAssert ctx "Read-Only Set Script Parse" False (show pErr)
    Right ast -> do
      result <- runRoutine ast adminEnv
      case result of
        Left SetOnReadOnly -> runAssert ctx "SET on time-travel reference throws <SET ON READONLY>" True ""
        other -> runAssert ctx "SET on time-travel reference threw expected error" False (show other)

  -- 4. Time Travel $DATA
  let (_, eng4) = commitKill "TESTADMIN" "ORDERS" [mkValue "1", mkValue "PRICE"] dummyTime eng3
  assertEqual ctx "Storage: $DATA live after kill is 0"
    (dataLive "ORDERS" [mkValue "1", mkValue "PRICE"] eng4)
    0
  dataAtTx2 <- dataAtTx 2 "ORDERS" [mkValue "1", mkValue "PRICE"] eng4
  assertEqual ctx "Storage: $DATA at past tx 2 before kill is 1" dataAtTx2 1

  -- 4b. KILL tombstones DESCENDANTS too, not just the killed path
  -- itself: 'killNode' prunes the WHOLE subtree regardless, so a
  -- descendant left untombstoned would make a time-traveled read (or
  -- $DATA) of it still see the pre-kill state at any tx at or after
  -- the kill. Before this fix, 'readAtTx'/'dataAtTx' below for the
  -- DOB child would have fallen back to its last real revision (the
  -- original SET, since no tombstone existed at or after the kill),
  -- i.e. the live-after-kill and "before the kill" checks below would
  -- have already passed either way; it's the two "AT the kill tx"
  -- checks that this regresses to if the fix is ever lost.
  let Right (_txParent, engParent) = commitSet "TESTADMIN" "PATIENT" [mkValue "1", mkValue "NAME"] (mkValue "Alice") Nothing dummyTime eng4
      Right (txChild, engChild)  = commitSet "TESTADMIN" "PATIENT" [mkValue "1", mkValue "DOB"] (mkValue "2000-01-01") Nothing dummyTime engParent
      (txKill, engKilled)        = commitKill "TESTADMIN" "PATIENT" [mkValue "1"] dummyTime engChild
  assertEqual ctx "Storage: killing a parent also clears the live parent"
    (dataLive "PATIENT" [mkValue "1", mkValue "NAME"] engKilled) 0
  assertEqual ctx "Storage: killing a parent also clears the live child"
    (dataLive "PATIENT" [mkValue "1", mkValue "DOB"] engKilled) 0
  ttChildBefore <- readAtTx txChild "PATIENT" [mkValue "1", mkValue "DOB"] engKilled
  assertEqual ctx "Storage: time-travel read of the child BEFORE the kill still works"
    ttChildBefore (Right (mkValue "2000-01-01"))
  ttChildAtKill <- readAtTx txKill "PATIENT" [mkValue "1", mkValue "DOB"] engKilled
  assertIsLeft ctx "Storage: time-travel read of the child AT the kill tx is undefined (descendant tombstone)" ttChildAtKill
  dataAtTxChildBefore <- dataAtTx txChild "PATIENT" [mkValue "1", mkValue "DOB"] engKilled
  assertEqual ctx "Storage: $DATA of the child BEFORE the kill is 1" dataAtTxChildBefore 1
  dataAtTxChildAtKill <- dataAtTx txKill "PATIENT" [mkValue "1", mkValue "DOB"] engKilled
  assertEqual ctx "Storage: $DATA of the child AT the kill tx is 0 (descendant tombstone)" dataAtTxChildAtKill 0
  -- The killed path itself (not just descendants) still gets its own
  -- tombstone too, same as before this fix.
  ttParentAtKill <- readAtTx txKill "PATIENT" [mkValue "1"] engKilled
  assertIsLeft ctx "Storage: time-travel read of the killed path itself at the kill tx is undefined" ttParentAtKill

  -- 5. Transactions & EXPECTING Optimistic Concurrency
  let handleSuccess = newTxHandle 0 dummyTime
      handleSuccess' = stageSet "ORDERS" [mkValue "1", mkValue "PRICE"] (mkValue "200") (Just 4) handleSuccess
  case commitTransaction "TESTADMIN" handleSuccess' dummyTime eng4 of
    Right (tx5, _) -> assertEqual ctx "Transaction: Commit with valid EXPECTING tx succeeds" tx5 5
    Left err       -> runAssert ctx "Transaction: Valid EXPECTING failed" False (show err)

  let handleConflict = newTxHandle 0 dummyTime
      handleConflict' = stageSet "ORDERS" [mkValue "1", mkValue "PRICE"] (mkValue "999") (Just 1) handleConflict
  case commitTransaction "TESTADMIN" handleConflict' dummyTime eng4 of
    Left (StorageWriteConflict "ORDERS" _ 1 4) ->
      runAssert ctx "Transaction: Write conflict detected on stale EXPECTING tx" True ""
    other ->
      runAssert ctx "Transaction: Expected conflict error" False (show other)

  -- 6. Advisory Lock Table
  let lt0 = emptyLockTable
  let path1 = mkGlobalPath "ORDERS" [mkValue "1"]
  let path1Sub = mkGlobalPath "ORDERS" [mkValue "1", mkValue "PRICE"]
  let (acq1, lt1) = acquireLocks "ProcessA" [path1] lt0
  assertEqual ctx "Locks: ProcessA acquires ^ORDERS(1)" acq1 True
  let (acq1Reentrant, lt2) = acquireLocks "ProcessA" [path1] lt1
  assertEqual ctx "Locks: ProcessA reentrantly acquires ^ORDERS(1)" acq1Reentrant True

  let (acqB_fail, _) = acquireLocks "ProcessB" [path1] lt2
  assertEqual ctx "Locks: ProcessB fails to acquire lock held by ProcessA" acqB_fail False

  -- Non-recursive check: path-granular
  let (acqB_sub, _) = acquireLocks "ProcessB" [path1Sub] lt2
  assertEqual ctx "Locks: Path granularity allows ProcessB to lock child ^ORDERS(1, PRICE)" acqB_sub True

  -- Release reentrant lock
  let lt3 = releaseLocks "ProcessA" [path1] lt2
  assertEqual ctx "Locks: Path still locked after 1 release (depth 2->1)" (isPathLocked path1 lt3) True
  let lt4 = releaseLocks "ProcessA" [path1] lt3
  assertEqual ctx "Locks: Path unlocked after 2nd release (depth 1->0)" (isPathLocked path1 lt4) False
  let (acqB_ok, _) = acquireLocks "ProcessB" [path1] lt4
  assertEqual ctx "Locks: ProcessB acquires ^ORDERS(1) once freed" acqB_ok True

  -- 7. WAL Persistence & Crash Recovery
  testWalPath <- tmpPath "test_phasedb.wal"
  testSnapPath <- tmpPath "test_phasedb.snap"
  walExistsOld <- doesFileExist testWalPath
  if walExistsOld then removeFile testWalPath else pure ()
  snapExistsOld <- doesFileExist testSnapPath
  if snapExistsOld then removeFile testSnapPath else pure ()

  let r1 = WalRecord 1 (WalSet "CUSTOMERS" [mkValue "101", mkValue "NAME"] (mkValue "Alice") Nothing)
  let r2 = WalRecord 2 (WalSet "CUSTOMERS" [mkValue "101", mkValue "CITY"] (mkValue "Tokyo") Nothing)
  let r3 = WalRecord 3 (WalSet "CUSTOMERS" [mkValue "101", mkValue "NAME"] (mkValue "Alice Smith") (Just 1))
  let r4 = WalRecord 4 (WalCommit 4 dummyTime "TESTADMIN")

  appendWalRecord testWalPath r1
  appendWalRecord testWalPath r2
  appendWalRecord testWalPath r3
  appendWalRecord testWalPath r4

  recoveredRes <- openStorageEngine testWalPath
  case recoveredRes of
    Left err -> runAssert ctx "Storage: WAL crash recovery open" False (show err)
    Right recoveredEng -> do
      assertEqual ctx "Storage: Recovered lastTx" (seLastTx recoveredEng) 4
      assertEqual ctx "Storage: Recovered live NAME"
        (readLive "CUSTOMERS" [mkValue "101", mkValue "NAME"] recoveredEng)
        (Right (mkValue "Alice Smith"))
      assertEqual ctx "Storage: Recovered live CITY"
        (readLive "CUSTOMERS" [mkValue "101", mkValue "CITY"] recoveredEng)
        (Right (mkValue "Tokyo"))
      recoveredTt1 <- readAtTx 1 "CUSTOMERS" [mkValue "101", mkValue "NAME"] recoveredEng
      assertEqual ctx "Storage: Recovered time travel NAME at tx 1" recoveredTt1 (Right (mkValue "Alice"))

      -- 8. Checkpoint: written into the SAME file, then a fresh
      -- openStorageEngine call should resume from it and see
      -- identical live state (verifying the checkpoint's captured
      -- state round-trips through the ordinary open path, not a
      -- separate snapshot-specific reader).
      _ <- takeCheckpoint recoveredEng
      reopenedRes <- openStorageEngine testWalPath
      case reopenedRes of
        Left reopenErr -> runAssert ctx "Storage: Reopen after checkpoint" False (show reopenErr)
        Right reopenedEng -> do
          assertEqual ctx "Storage: Checkpoint Tx matches lastTx" (seLastTx reopenedEng) (seLastTx recoveredEng)
          assertEqual ctx "Storage: Checkpoint-resumed globals equal pre-checkpoint live globals"
            (seLive reopenedEng) (seLive recoveredEng)
          -- Time travel to a point BEFORE the checkpoint must still
          -- work after resuming from it, this is the core guarantee
          -- checkpointing must not break: a checkpoint is a resume
          -- point, not a truncation. openStorageEngine only replays
          -- records AFTER the checkpoint into seHistory, so this
          -- specifically exercises readAtTx's deep-replay fallback.
          deepTt <- readAtTx 1 "CUSTOMERS" [mkValue "101", mkValue "NAME"] reopenedEng
          assertEqual ctx "Storage: Deep time-travel through a checkpoint still finds pre-checkpoint history" deepTt (Right (mkValue "Alice"))

          -- 8b. CITY was written once (tx 2) and never touched again,
          -- so it survives entirely as a checkpoint entry with no
          -- ordinary WalSet after the checkpoint to re-derive its
          -- history from. Before 'loadCheckpointEntries' carried each
          -- entry's own last-write tx, this path had NOTHING in the
          -- reopened engine's seHistory/seLastTxMap at all: a
          -- resident time-travel read or $DATA at or after the
          -- checkpoint tx would wrongly report it as never written,
          -- and an EXPECTING against it would compare against tx 0.
          resumedTt <- readAtTx (seLastTx reopenedEng) "CUSTOMERS" [mkValue "101", mkValue "CITY"] reopenedEng
          assertEqual ctx "Storage: checkpoint-only path resolves at/after the checkpoint without deep replay"
            resumedTt (Right (mkValue "Tokyo"))
          dataAtCheckpointTx <- dataAtTx (seLastTx reopenedEng) "CUSTOMERS" [mkValue "101", mkValue "CITY"] reopenedEng
          assertEqual ctx "Storage: $DATA of a checkpoint-only path at the checkpoint tx is 1" dataAtCheckpointTx 1
          assertEqual ctx "Storage: checkpoint preserves the path's TRUE last-write tx, not the checkpoint's own tx"
            (Map.lookup ("CUSTOMERS", map toSubKey [mkValue "101", mkValue "CITY"]) (seLastTxMap reopenedEng))
            (Just 2)

  -- 9. Crash recovery. Each scenario builds a .db file by hand, opens
  -- it, and checks BOTH what was recovered and that later commits
  -- survive another reopen.
  let freshFile path = do
        e <- doesFileExist path
        if e then removeFile path else pure ()
      setRec tx g k v = WalRecord tx (WalSet g [mkValue k] (mkValue v) Nothing)
      commitRec tx = WalRecord tx (WalCommit tx dummyTime "TESTADMIN")
      commitAndReopen path eng g k v = do
        case commitSet "TESTADMIN" g [mkValue k] (mkValue v) Nothing dummyTime eng of
          Left e -> pure (Left (show e))
          Right (_, eng') -> do
            _ <- flushPendingWal eng'
            r <- openStorageEngine path
            pure (either (Left . show) Right r)

  -- 9a. A torn trailing record (crash mid-append) is cut off the file,
  -- so the next commit is appended cleanly. Before, the partial bytes
  -- stayed and the following record was silently lost on reopen.
  tornPath <- tmpPath "test_phasedb_torn.db"
  freshFile tornPath
  appendWalRecords tornPath [setRec 1 "T" "a" "one", commitRec 1]
  goodSize <- getFileSize tornPath
  BS.appendFile tornPath (BS.pack [0, 0, 0, 100, 1, 2, 3])  -- claims 100 bytes, only 3 present
  tornRes <- openStorageEngine tornPath
  case tornRes of
    Left err -> runAssert ctx "WAL: open with torn tail" False (show err)
    Right tornEng -> do
      assertEqual ctx "WAL: torn tail keeps earlier commit"
        (readLive "T" [mkValue "a"] tornEng) (Right (mkValue "one"))
      sizeAfterOpen <- getFileSize tornPath
      assertEqual ctx "WAL: torn tail truncated off the file on open" sizeAfterOpen goodSize
      again <- commitAndReopen tornPath tornEng "T" "b" "two"
      case again of
        Left err -> runAssert ctx "WAL: commit after torn-tail recovery" False err
        Right e2 -> do
          assertEqual ctx "WAL: pre-crash value survives reopen"
            (readLive "T" [mkValue "a"] e2) (Right (mkValue "one"))
          assertEqual ctx "WAL: post-recovery commit survives reopen"
            (readLive "T" [mkValue "b"] e2) (Right (mkValue "two"))
  freshFile tornPath

  -- 9b. Records with no WalCommit after them (crash between the writes
  -- and the commit marker) are a torn transaction: never replayed, and
  -- removed so the next real transaction can reuse that tx id safely.
  dangPath <- tmpPath "test_phasedb_dangling.db"
  freshFile dangPath
  appendWalRecords dangPath [setRec 1 "T" "a" "one", commitRec 1]
  appendWalRecords dangPath [setRec 2 "T" "b" "lost"]
  dangRes <- openStorageEngine dangPath
  case dangRes of
    Left err -> runAssert ctx "WAL: open with uncommitted tail" False (show err)
    Right dangEng -> do
      assertEqual ctx "WAL: uncommitted write not replayed"
        (readLive "T" [mkValue "b"] dangEng) (Left (StorageUndefinedVar "T" [mkValue "b"]))
      assertEqual ctx "WAL: lastTx ignores the uncommitted tx" (seLastTx dangEng) 1
      again <- commitAndReopen dangPath dangEng "T" "b" "kept"
      case again of
        Left err -> runAssert ctx "WAL: commit after dropping uncommitted tail" False err
        Right e2 -> do
          assertEqual ctx "WAL: new commit wins over the discarded one"
            (readLive "T" [mkValue "b"] e2) (Right (mkValue "kept"))
          assertEqual ctx "WAL: tx id reused cleanly" (seLastTx e2) 2
  freshFile dangPath

  -- 9c. A flush of several pending commits is one append and reopens whole.
  batchPath <- tmpPath "test_phasedb_batch.db"
  freshFile batchPath
  let batchEng0 = initStorageEngine { seWalPath = Just batchPath }
      Right (_, batchEng1) = commitSet "TESTADMIN" "B" [mkValue "1"] (mkValue "x") Nothing dummyTime batchEng0
      Right (_, batchEng2) = commitSet "TESTADMIN" "B" [mkValue "2"] (mkValue "y") Nothing dummyTime batchEng1
  _ <- flushPendingWal batchEng2
  batchRes <- openStorageEngine batchPath
  case batchRes of
    Left err -> runAssert ctx "WAL: reopen after batched flush" False (show err)
    Right be -> do
      assertEqual ctx "WAL: batched flush first write" (readLive "B" [mkValue "1"] be) (Right (mkValue "x"))
      assertEqual ctx "WAL: batched flush second write" (readLive "B" [mkValue "2"] be) (Right (mkValue "y"))
      assertEqual ctx "WAL: batched flush lastTx" (seLastTx be) 2
  freshFile batchPath

  -- 9d. Checkpointing flushes pending records first, so the checkpoint
  -- never precedes records it already reflects.
  cpPath <- tmpPath "test_phasedb_cpflush.db"
  freshFile cpPath
  let cpEng0 = initStorageEngine { seWalPath = Just cpPath }
      Right (_, cpEng1) = commitSet "TESTADMIN" "C" [mkValue "1"] (mkValue "v") Nothing dummyTime cpEng0
  cpEng2 <- takeCheckpoint cpEng1
  assertEqual ctx "WAL: takeCheckpoint flushed pending records" (null (sePendingWal cpEng2)) True
  cpRecords <- readWalRecords cpPath
  assertEqual ctx "WAL: checkpoint is the last record, after the commit it reflects"
    (map (\(WalRecord _ op) -> case op of { WalSet {} -> "set"; WalCommit {} -> "commit"; WalCheckpoint {} -> "checkpoint"; _ -> "other" }) cpRecords)
    ["set", "commit", "checkpoint" :: String]
  freshFile cpPath

  -- 9e. Unrecognised or corrupt files are refused, not overwritten or
  -- silently emptied.
  badPath <- tmpPath "test_phasedb_bad.db"
  freshFile badPath
  BS.writeFile badPath (BS.pack [0x50, 0x48, 0x44, 0x42, 1])  -- right magic, old version 1
  oldVerRes <- openStorageEngine badPath
  case oldVerRes of
    Left (StorageCorruptWal _) -> runAssert ctx "WAL: unsupported format version is refused" True ""
    other -> runAssert ctx "WAL: unsupported format version is refused" False (show (fmap seLastTx other))

  freshFile badPath
  BS.writeFile badPath (BS.pack [0x50, 0x48])  -- crash while writing the header
  tornHdrRes <- openStorageEngine badPath
  case tornHdrRes of
    Right e -> do
      assertEqual ctx "WAL: torn header opens as an empty database" (seLastTx e) 0
      hdrSize <- getFileSize badPath
      assertEqual ctx "WAL: torn header cut back to empty" hdrSize 0
    Left err -> runAssert ctx "WAL: torn header opens as an empty database" False (show err)

  freshFile badPath
  appendWalRecords badPath [setRec 1 "T" "a" "one", commitRec 1, setRec 2 "T" "b" "two", commitRec 2]
  fileBytes <- BS.readFile badPath
  let tagIdx = 5 + 4 + 8  -- header, length prefix, tx id -> op tag byte of the first record
      damaged = BS.take tagIdx fileBytes <> BS.pack [0xFF] <> BS.drop (tagIdx + 1) fileBytes
  BS.writeFile badPath damaged
  corruptRes <- openStorageEngine badPath
  case corruptRes of
    Left (StorageCorruptWal _) -> runAssert ctx "WAL: mid-file corruption is refused" True ""
    other -> runAssert ctx "WAL: mid-file corruption is refused" False (show (fmap seLastTx other))
  sizeAfterRefusal <- getFileSize badPath
  assertEqual ctx "WAL: refused file is left untouched" sizeAfterRefusal (fromIntegral (BS.length damaged))
  freshFile badPath

  -- Cleanup temporary test files
  walExistsEnd <- doesFileExist testWalPath
  if walExistsEnd then removeFile testWalPath else pure ()

testChildCountGroup :: TestContext -> IO ()
testChildCountGroup ctx = do
  adminEnv <- testAdminEnv

  -- 1. Tree-level: childCountNode directly
  let t0 = emptyNode
  -- emptyNode is a real node with no children, Just 0, not Nothing
  -- (Nothing is only returned when the PATH does not exist, i.e. navigating
  -- into a subscript that has no child node at all)
  assertEqual ctx "childCountNode on empty node -> Just 0"
    (childCountNode [] t0) (Just 0)

  let t1 = setNode [mkValue "A"] (mkValue "1") t0
  assertEqual ctx "childCountNode root with 1 child -> Just 1"
    (childCountNode [] t1) (Just 1)

  let t2 = setNode [mkValue "B"] (mkValue "2") t1
  let t3 = setNode [mkValue "C"] (mkValue "3") t2
  assertEqual ctx "childCountNode root with 3 children -> Just 3"
    (childCountNode [] t3) (Just 3)

  -- Node with value AND children: value doesn't count as a child
  let t4 = setNode [] (mkValue "root") t3
  assertEqual ctx "childCountNode: own value not counted, only children"
    (childCountNode [] t4) (Just 3)

  -- Nested: count children at a sub-path
  let t5 = setNode [mkValue "A", mkValue "X"] (mkValue "x1") t4
  let t6 = setNode [mkValue "A", mkValue "Y"] (mkValue "y1") t5
  assertEqual ctx "childCountNode at sub-path -> Just 2"
    (childCountNode [mkValue "A"] t6) (Just 2)
  assertEqual ctx "childCountNode at leaf (no children) -> Just 0"
    (childCountNode [mkValue "B"] t6) (Just 0)
  assertEqual ctx "childCountNode at nonexistent path -> Nothing"
    (childCountNode [mkValue "Z"] t6) Nothing

  -- 2. Eval-level: $CHILDCOUNT intrinsic via script
  let runScript src = do
        case mapM parseLogicalLine (buildLogicalLines src) of
          Left pErr -> pure (Left (show pErr))
          Right ast -> do
            result <- runRoutine ast adminEnv
            case result of
              Left evalErr -> pure (Left (show evalErr))
              Right env    -> pure (Right env)

  -- $CHILDCOUNT on local variable
  r1 <- runScript " SET X(\"A\")=1,X(\"B\")=2,X(\"C\")=3\n WRITE $CHILDCOUNT(X),!"
  case r1 of
    Left err -> runAssert ctx "$CHILDCOUNT local 3 children" False err
    Right env -> assertEqual ctx "$CHILDCOUNT local 3 children output" (getOutputText env) "3\n"

  -- $CHILDCOUNT on global
  r2 <- runScript " SET ^CC(1)=\"a\",^CC(2)=\"b\"\n WRITE $CHILDCOUNT(^CC),!"
  case r2 of
    Left err -> runAssert ctx "$CHILDCOUNT global 2 children" False err
    Right env -> assertEqual ctx "$CHILDCOUNT global 2 children output" (getOutputText env) "2\n"

  -- $CHILDCOUNT on a node that has a value but no children
  r3 <- runScript " SET Y=42\n WRITE $CHILDCOUNT(Y),!"
  case r3 of
    Left err -> runAssert ctx "$CHILDCOUNT leaf node (no children)" False err
    Right env -> assertEqual ctx "$CHILDCOUNT leaf node -> 0" (getOutputText env) "0\n"

  -- $CHILDCOUNT on undefined variable -> 0
  r4 <- runScript " WRITE $CHILDCOUNT(UNDEF),!"
  case r4 of
    Left err -> runAssert ctx "$CHILDCOUNT undefined var" False err
    Right env -> assertEqual ctx "$CHILDCOUNT undefined -> 0" (getOutputText env) "0\n"

  -- $CHILDCOUNT at a subscripted path
  r5 <- runScript " SET ^TREE(1,\"X\")=10,^TREE(1,\"Y\")=20\n WRITE $CHILDCOUNT(^TREE(1)),!"
  case r5 of
    Left err -> runAssert ctx "$CHILDCOUNT at subscripted path" False err
    Right env -> assertEqual ctx "$CHILDCOUNT subscripted path -> 2" (getOutputText env) "2\n"

-- --------------------------------------------------------------------
-- Security: null subscripts, ^_CATALOG ownership, last-admin safeguard
-- --------------------------------------------------------------------

-- --------------------------------------------------------------------
-- Naked references and $PIECE/$EXTRACT as SET targets (additions: 
-- neither is in the handbook yet)
-- --------------------------------------------------------------------

testNakedAndLvalueGroup :: TestContext -> IO ()
testNakedAndLvalueGroup ctx = do
  adminEnv <- testAdminEnv
  let run src = runRoutine (either (const []) id (mapM parseLogicalLine (buildLogicalLines src))) adminEnv

  -- Naked references: ^(subs) reuses the last NAMED global, keeping
  -- its name but replacing the WHOLE subscript list (not just
  -- appending/replacing the last one) -- so reaching ^PATIENT(1,"DOB")
  -- after ^PATIENT(1,"NAME") means repeating the shared "1".
  r <- run " SET ^PATIENT(1,\"NAME\")=\"Alice\"\n SET ^(1,\"DOB\")=\"2000-01-01\"\n WRITE ^PATIENT(1,\"NAME\"),\"/\",^PATIENT(1,\"DOB\"),!"
  case r of
    Left err -> runAssert ctx "Naked reference keeps the global, replaces the whole subscript list" False (show err)
    Right env -> assertEqual ctx "Naked reference keeps the global, replaces the whole subscript list output"
      (getOutputText env) "Alice/2000-01-01\n"

  -- A read-position naked reference works too, and the LAST named
  -- global wins even when it was only READ, not written.
  r <- run " SET ^A(1)=\"first\"\n SET ^B(1)=\"second\"\n WRITE ^A(1),!\n WRITE ^(1),!"
  case r of
    Left err -> runAssert ctx "Naked reference after a plain read uses the most recently READ global" False (show err)
    Right env -> assertEqual ctx "Naked reference after a plain read uses the most recently read global output"
      (getOutputText env) "first\nfirst\n"

  -- GOTO/KILL/LOCK etc. all resolve through the same 'resolveWriteTarget'
  -- chokepoint as SET, so KILL is enough to confirm naked refs are not
  -- special-cased to SET alone.
  r <- run " SET ^K(1)=\"x\",^K(2)=\"y\"\n KILL ^(1)\n WRITE $DATA(^K(1)),\",\",$DATA(^K(2)),!"
  case r of
    Left err -> runAssert ctx "KILL accepts a naked reference" False (show err)
    Right env -> assertEqual ctx "KILL accepts a naked reference output" (getOutputText env) "0,1\n"

  -- <NAKED> before any global has been named.
  r <- run " WRITE ^(1),!"
  case r of
    Left (NakedReference) -> runAssert ctx "Naked reference before any global is named is <NAKED>" True ""
    other -> runAssert ctx "Naked reference before any global is named is <NAKED>" False (show other)

  -- SET $PIECE(ref,delim,which)=val
  r <- run " SET X=\"A,B,C\"\n SET $PIECE(X,\",\",2)=\"X\"\n WRITE X,!"
  case r of
    Left err -> runAssert ctx "SET $PIECE replaces an existing piece" False (show err)
    Right env -> assertEqual ctx "SET $PIECE replaces an existing piece output" (getOutputText env) "A,X,C\n"

  r <- run " SET X=\"A\"\n SET $PIECE(X,\",\",3)=\"C\"\n WRITE X,!"
  case r of
    Left err -> runAssert ctx "SET $PIECE pads with empty pieces when 'which' is beyond the current count" False (show err)
    Right env -> assertEqual ctx "SET $PIECE pads with empty pieces output" (getOutputText env) "A,,C\n"

  r <- run " SET X=\"A,B\"\n SET $PIECE(X,\",\")=\"Z\"\n WRITE X,!"
  case r of
    Left err -> runAssert ctx "SET $PIECE defaults 'which' to 1" False (show err)
    Right env -> assertEqual ctx "SET $PIECE defaults 'which' to 1 output" (getOutputText env) "Z,B\n"

  -- SET $EXTRACT(ref,start,end)=val
  r <- run " SET X=\"HELLO\"\n SET $EXTRACT(X,2,4)=\"XYZ\"\n WRITE X,!"
  case r of
    Left err -> runAssert ctx "SET $EXTRACT replaces a character range" False (show err)
    Right env -> assertEqual ctx "SET $EXTRACT replaces a character range output" (getOutputText env) "HXYZO\n"

  r <- run " SET X=\"HI\"\n SET $EXTRACT(X,5,5)=\"X\"\n WRITE X,!"
  case r of
    Left err -> runAssert ctx "SET $EXTRACT pads with spaces past the current length" False (show err)
    Right env -> assertEqual ctx "SET $EXTRACT pads with spaces output" (getOutputText env) "HI  X\n"

  r <- run " SET X=\"HELLO\"\n SET $EXTRACT(X,3,2)=\"Z\"\n WRITE X,!"
  case r of
    Left err -> runAssert ctx "SET $EXTRACT with end<start inserts without removing" False (show err)
    Right env -> assertEqual ctx "SET $EXTRACT insert output" (getOutputText env) "HEZLLO\n"

  r <- run " SET X=\"HI\"\n SET $EXTRACT(X)=\"J\"\n WRITE X,!"
  case r of
    Left err -> runAssert ctx "SET $EXTRACT defaults start and end to 1" False (show err)
    Right env -> assertEqual ctx "SET $EXTRACT defaults output" (getOutputText env) "JI\n"

  -- $PIECE/$EXTRACT as a SET target still read-errors on a genuinely
  -- undefined variable, same as reading it plainly would.
  r <- run " SET $PIECE(UNDEF,\",\",1)=\"X\""
  case r of
    Left (UndefinedVariable _ _) -> runAssert ctx "SET $PIECE on an undefined variable errors, doesn't silently default to empty" True ""
    other -> runAssert ctx "SET $PIECE on an undefined variable errors, doesn't silently default to empty" False (show other)

  -- Anything else after '$' is a real parse error, not a silent
  -- fallback to an ordinary reference.
  case parseLogicalLine (LogicalLine 0 Nothing "SET $FOO(X)=1") of
    Left _  -> runAssert ctx "SET $FOO(...) (unrecognized intrinsic as a target) is a parse error" True ""
    Right r -> runAssert ctx "SET $FOO(...) (unrecognized intrinsic as a target) is a parse error" False (show r)

testSecurityGroup :: TestContext -> IO ()
testSecurityGroup ctx = do
  adminEnv <- testAdminEnv

  -- 1. Null subscripts (fix for a real gap: nothing previously rejected
  -- "" as a subscript, though $ORDER already reserves it as the
  -- exhaustion sentinel and can never reach a node keyed by it).
  assertEqual ctx "SET ^G(\"\")=x is refused (global)"
    (either formatError (const "ok") (writeRef Global "G" [mkValue ""] (mkValue "x") adminEnv))
    (formatError (NullSubscript "G" [mkValue ""]))
  assertEqual ctx "SET x(1,\"\")=y is refused (local, not just global)"
    (either formatError (const "ok") (writeRef Local "x" [mkValue "1", mkValue ""] (mkValue "y") adminEnv))
    (formatError (NullSubscript "x" [mkValue "1", mkValue ""]))
  assertEqual ctx "SET ^G(1)=x is unaffected (ordinary subscript still works)"
    (either (const "denied") (const "ok") (writeRef Global "G" [mkValue "1"] (mkValue "x") adminEnv))
    "ok"
  assertEqual ctx "MERGE into ^G(\"\") is refused"
    (either formatError (const "ok") (mergeRef (Global, "G", [mkValue ""]) (Global, "H", []) adminEnv))
    (formatError (NullSubscript "G" [mkValue ""]))

  -- 2. ^_CATALOG: a non-admin can still define their own routines, but
  -- cannot overwrite one an admin has authored.
  let Right envWithWriter = writeRef Global "_USERS" [mkValue "WRITER", mkValue "role"] (mkValue "write") adminEnv
      Right envAdminSquare = defineItem "MATH" "SQUARE" KindFunction (Just ["X"]) " RETURN X*X" envWithWriter
      writerEnv = envAdminSquare { envCurrentUser = "WRITER" }

  case defineItem "MATH" "SQUARE" KindFunction (Just ["X"]) " RETURN X*X*X" writerEnv of
    Left (PermissionDenied "WRITER" _) -> runAssert ctx "Non-admin cannot redefine an admin-authored function" True ""
    other -> runAssert ctx "Non-admin cannot redefine an admin-authored function" False (show (fmap (const "ok") other))

  case defineItem "MATH" "CUBE" KindFunction (Just ["X"]) " RETURN X*X*X" writerEnv of
    Left err -> runAssert ctx "Non-admin can still define a brand-new function" False (show err)
    Right envWithCube -> do
      runAssert ctx "Non-admin can still define a brand-new function" True ""
      case defineItem "MATH" "CUBE" KindFunction (Just ["X"]) " RETURN X*X*X*1" envWithCube of
        Left err -> runAssert ctx "Non-admin can edit their own, not-admin-owned function" False (show err)
        Right envCubeEdited -> do
          runAssert ctx "Non-admin can edit their own, not-admin-owned function" True ""
          -- Admin can take over a non-admin's item...
          case defineItem "MATH" "CUBE" KindFunction (Just ["X"]) " RETURN X**3" (envCubeEdited { envCurrentUser = "TESTADMIN" }) of
            Left err -> runAssert ctx "Admin can redefine a non-admin's function" False (show err)
            Right envCubeAdminOwned -> do
              runAssert ctx "Admin can redefine a non-admin's function" True ""
              -- ...and it becomes protected from that point on.
              case defineItem "MATH" "CUBE" KindFunction (Just ["X"]) " RETURN X*X*X" (envCubeAdminOwned { envCurrentUser = "WRITER" }) of
                Left (PermissionDenied "WRITER" _) -> runAssert ctx "Function becomes protected once an admin has touched it" True ""
                other -> runAssert ctx "Function becomes protected once an admin has touched it" False (show (fmap (const "ok") other))

  -- The same protection applies to a raw SET/KILL that bypasses
  -- 'defineItem' entirely and reaches ^_CATALOG directly.
  case writeRef Global "_CATALOG" [mkValue "MATH", mkValue "SQUARE"] (mkValue "malicious") writerEnv of
    Left (PermissionDenied "WRITER" _) -> runAssert ctx "Raw SET ^_CATALOG cannot overwrite an admin-owned item" True ""
    other -> runAssert ctx "Raw SET ^_CATALOG cannot overwrite an admin-owned item" False (show (fmap (const "ok") other))
  case writeRef Global "_CATALOG" [mkValue "MATH", mkValue "NEWTHING"] (mkValue "body") writerEnv of
    Left err -> runAssert ctx "Raw SET ^_CATALOG can still create a new item" False (show err)
    Right _  -> runAssert ctx "Raw SET ^_CATALOG can still create a new item" True ""
  case killRef Global "_CATALOG" [mkValue "MATH"] writerEnv of
    Left (PermissionDenied "WRITER" _) -> runAssert ctx "Non-admin cannot KILL a whole ^_CATALOG namespace" True ""
    other -> runAssert ctx "Non-admin cannot KILL a whole ^_CATALOG namespace" False (show (fmap (const "ok") other))
  case killRef Global "_CATALOG" [mkValue "MATH"] envAdminSquare of
    Left err -> runAssert ctx "Admin CAN KILL a whole ^_CATALOG namespace" False (show err)
    Right _  -> runAssert ctx "Admin CAN KILL a whole ^_CATALOG namespace" True ""

  -- 3. ^_USERS: an admin can never remove or demote the sole remaining
  -- admin account. (Only an admin can reach ^_USERS writes at all --
  -- 'checkWriteAccess' already hardcodes that -- so this is purely
  -- about protecting an admin from locking themselves out.)
  case killRef Global "_USERS" [mkValue "TESTADMIN"] adminEnv of
    Left (PermissionDenied "TESTADMIN" _) -> runAssert ctx "Cannot KILL the sole admin account" True ""
    other -> runAssert ctx "Cannot KILL the sole admin account" False (show (fmap (const "ok") other))
  case writeRef Global "_USERS" [mkValue "TESTADMIN", mkValue "role"] (mkValue "write") adminEnv of
    Left (PermissionDenied "TESTADMIN" _) -> runAssert ctx "Cannot demote the sole admin account" True ""
    other -> runAssert ctx "Cannot demote the sole admin account" False (show (fmap (const "ok") other))
  case killRef Global "_USERS" [] adminEnv of
    Left (PermissionDenied "TESTADMIN" _) -> runAssert ctx "Cannot KILL ^_USERS entirely while an admin exists" True ""
    other -> runAssert ctx "Cannot KILL ^_USERS entirely while an admin exists" False (show (fmap (const "ok") other))
  -- A non-admin user's own record can still be removed freely...
  case killRef Global "_USERS" [mkValue "WRITER"] envWithWriter of
    Left err -> runAssert ctx "A non-admin user's own record can still be KILLed" False (show err)
    Right _  -> runAssert ctx "A non-admin user's own record can still be KILLed" True ""
  -- ...and once a SECOND admin exists, the first can be removed too.
  let Right envTwoAdmins = writeRef Global "_USERS" [mkValue "ADMIN2", mkValue "role"] (mkValue "admin") adminEnv
  case killRef Global "_USERS" [mkValue "TESTADMIN"] envTwoAdmins of
    Left err -> runAssert ctx "The original admin CAN be removed once a second admin exists" False (show err)
    Right _  -> runAssert ctx "The original admin CAN be removed once a second admin exists" True ""

-- --------------------------------------------------------------------
-- REF(...) Dynamic Reference Tests (Part VI §1)
-- --------------------------------------------------------------------

testRefDynamicGroup :: TestContext -> IO ()
testRefDynamicGroup ctx = do
  adminEnv <- testAdminEnv

  let runScript src = do
        case mapM parseLogicalLine (buildLogicalLines src) of
          Left pErr -> pure (Left (show pErr))
          Right ast -> do
            result <- runRoutine ast adminEnv
            case result of
              Left evalErr -> pure (Left (show evalErr))
              Right env    -> pure (Right env)

  -- SET REF(localName) = value, then read back via plain local
  r1 <- runScript " SET lname=\"X\"\n SET REF(lname)=42\n WRITE X,!"
  case r1 of
    Left err -> runAssert ctx "REF SET local: plain read after REF write" False err
    Right env -> assertEqual ctx "REF SET local: value written" (getOutputText env) "42\n"

  -- SET REF(localName,sub) = value, subscripted local
  r2 <- runScript " SET lname=\"ITEM\"\n SET REF(lname,\"K\")=\"widget\"\n WRITE ITEM(\"K\"),!"
  case r2 of
    Left err -> runAssert ctx "REF SET subscripted local" False err
    Right env -> assertEqual ctx "REF SET subscripted local: value written" (getOutputText env) "widget\n"

  -- SET REF(globalName) = value using ^-prefixed name string
  r3 <- runScript " SET gname=\"^RTEST\"\n SET REF(gname)=\"hello\"\n WRITE ^RTEST,!"
  case r3 of
    Left err -> runAssert ctx "REF SET global via ^-prefix" False err
    Right env -> assertEqual ctx "REF SET global: value written" (getOutputText env) "hello\n"

  -- Read via REF(name) in expression position
  r4 <- runScript " SET SRC=\"world\"\n SET lname=\"SRC\"\n WRITE REF(lname),!"
  case r4 of
    Left err -> runAssert ctx "REF read in expression position" False err
    Right env -> assertEqual ctx "REF read: correct value returned" (getOutputText env) "world\n"

  -- KILL REF(name)
  r5 <- runScript " SET TODEL=99\n SET lname=\"TODEL\"\n KILL REF(lname)\n WRITE $GET(TODEL,\"gone\"),!"
  case r5 of
    Left err -> runAssert ctx "REF KILL local" False err
    Right env -> assertEqual ctx "REF KILL: variable deleted" (getOutputText env) "gone\n"

  -- SET REF(name,sub1,sub2): multi-level subscripted global
  r6 <- runScript " SET gname=\"^MULTI\"\n SET REF(gname,1,\"STATUS\")=\"SHIPPED\"\n WRITE ^MULTI(1,\"STATUS\"),!"
  case r6 of
    Left err -> runAssert ctx "REF SET global multi-level subscripts" False err
    Right env -> assertEqual ctx "REF SET global multi-level: value written" (getOutputText env) "SHIPPED\n"

  -- REF() with zero args is a <SYNTAX> error
  r7 <- runScript " SET REF()=1"
  case r7 of
    Left _    -> runAssert ctx "REF() zero args is a syntax error" True ""
    Right _   -> runAssert ctx "REF() zero args is a syntax error" False "Expected error but succeeded"

  -- MERGE REF(dst) = REF(src)
  r8 <- runScript " SET A(1)=\"x\",A(2)=\"y\"\n SET d=\"^MDST\"\n SET s=\"A\"\n MERGE REF(d)=REF(s)\n WRITE ^MDST(1),!\n WRITE ^MDST(2),!"
  case r8 of
    Left err -> runAssert ctx "MERGE REF(dst)=REF(src)" False err
    Right env -> assertEqual ctx "MERGE REF(dst)=REF(src): values merged" (getOutputText env) "x\ny\n"

-- --------------------------------------------------------------------
-- EVALUATE / WHEN, and the explicit END blocks they share with DO
-- (Part II §5, Part I §5.4)
-- --------------------------------------------------------------------

testEvaluateGroup :: TestContext -> IO ()
testEvaluateGroup ctx = do
  adminEnv <- testAdminEnv
  let depths src = map llDepth (buildLogicalLines src)
      stmtsOf src = case parseLogicalLine (LogicalLine 0 Nothing src) of
        Right (Line _ _ ps) -> Right (map psStmt ps)
        Left e              -> Left (show e)
      run src = case mapM parseLogicalLine (buildLogicalLines src) of
        Left parseErr -> pure (Left (show parseErr))
        Right ls -> do
          r <- runRoutine ls adminEnv
          pure (either (Left . show) (Right . getOutputText) r)
      num n = Lit (mkValue n)
      var n = Var (mkRef Local n [])

  -- ---- Phase.Lines: explicit END blocks get the same depths dotted blocks do ----
  assertEqual ctx "Lines: DO ... END DO body is one level deeper, END keeps the opener's depth"
    (depths " DO\n WRITE 1\n END DO\n WRITE 2") [0, 1, 0, 0]
  assertEqual ctx "Lines: nested DO ... END DO"
    (depths " DO\n DO\n WRITE 1\n END DO\n END DO") [0, 1, 2, 1, 0]
  assertEqual ctx "Lines: an END-style DO with no END DO is left as before (empty body)"
    (depths " DO\n WRITE 1") [0, 0]
  assertEqual ctx "Lines: a stray END DO after a dotted block changes nothing"
    (depths " DO\n. WRITE 1\n END DO") [0, 1, 0]
  assertEqual ctx "Lines: a dotted DO inside an END DO block adds up"
    (depths " DO\n DO\n. WRITE 1\n END DO\n END DO") [0, 1, 2, 0, 0]
  assertEqual ctx "Lines: END DO is not itself a block opener"
    (map (\t -> (endsWithBareDo t, opensBlock t)) ["END DO", "DO", "FOR I=1:1:3 DO"])
    [(False, False), (True, True), (True, True)]
  assertEqual ctx "Lines: END-style EVALUATE gives its WHEN lines one level, END EVALUATE none"
    (depths " EVALUATE X\n WHEN 1 WRITE 1\n WHEN OTHER WRITE 2\n END EVALUATE") [0, 1, 1, 0]
  assertEqual ctx "Lines: dotted EVALUATE is not read as a continuation of its header"
    (depths " EVALUATE X\n. WHEN 1 WRITE 1\n. WHEN OTHER WRITE 2") [0, 1, 1]
  assertEqual ctx "Lines: EVALUATE is an opener, even with a postcondition; EVALUATED is not"
    (map opensBlock ["EVALUATE X", "EVALUATE:A X", "evaluate X", "EVALUATED", "SET EVALUATE=1"])
    [True, True, True, False, False]
  assertEqual ctx "Lines: an END of the wrong kind closes the right opener and leaves the other alone"
    (depths " DO\n EVALUATE X\n WHEN 1 WRITE 1\n END DO") [0, 1, 1, 0]
  assertEqual ctx "Lines: nested EVALUATE with a dotted WHEN body"
    (depths (T.unlines
      [ " EVALUATE A"
      , " WHEN 1 DO"
      , ". EVALUATE B"
      , ". WHEN 1 WRITE \"11\""
      , ". WHEN OTHER WRITE \"1x\""
      , ". END EVALUATE"
      , " WHEN OTHER WRITE \"x\""
      , " END EVALUATE" ]))
    [0, 1, 2, 3, 3, 2, 1, 0]
  assertEqual ctx "Lines: EVALUATE at column 0 is a command, not a tag"
    (map llTag (buildLogicalLines "EVALUATE X\n    WHEN 1 WRITE 1\nEND EVALUATE"))
    [Nothing, Nothing, Nothing]

  -- ---- the REPL's multi-line prompt ----
  assertEqual ctx "continuation: DO then an undotted line keeps prompting for END DO"
    (needsContinuation " DO\n WRITE 1") True
  assertEqual ctx "continuation: END DO completes the block"
    (needsContinuation " DO\n WRITE 1\n END DO") False
  assertEqual ctx "continuation: a blank line still abandons an unclosed END-style block"
    (needsContinuation " DO\n WRITE 1\n") False
  assertEqual ctx "continuation: an EVALUATE header opens a block"
    (needsContinuation " EVALUATE X") True
  assertEqual ctx "continuation: EVALUATE with clauses still waits for END EVALUATE"
    (needsContinuation " EVALUATE X\n WHEN 1 WRITE 1") True
  assertEqual ctx "continuation: END EVALUATE completes it"
    (needsContinuation " EVALUATE X\n WHEN 1 WRITE 1\n END EVALUATE") False
  assertEqual ctx "continuation: a dotted EVALUATE line keeps the block open"
    (needsContinuation " EVALUATE X\n. WHEN 1 WRITE 1") True

  -- ---- Phase.Stmt: parsing ----
  assertEqual ctx "parse: EVALUATE X"
    (stmtsOf "EVALUATE X") (Right [SEvaluate [SubjectExpr (var "X")]])
  assertEqual ctx "parse: EVALUATE TRUE is the condition-chain subject"
    (stmtsOf "EVALUATE TRUE") (Right [SEvaluate [SubjectTrue]])
  assertEqual ctx "parse: EVALUATE A ALSO B"
    (stmtsOf "EVALUATE A ALSO B") (Right [SEvaluate [SubjectExpr (var "A"), SubjectExpr (var "B")]])
  assertEqual ctx "parse: a subject that merely starts with TRUE is an ordinary expression"
    (stmtsOf "EVALUATE TRUEISH") (Right [SEvaluate [SubjectExpr (var "TRUEISH")]])
  assertEqual ctx "parse: WHEN value, then the body statement on the same line"
    (fmap (take 1) (stmtsOf "WHEN 1 WRITE 2")) (Right [SWhen [ValueTest (num "1")] Nothing])
  assertEqual ctx "parse: the clause body is parsed as ordinary following statements"
    (fmap length (stmtsOf "WHEN 1 WRITE 2,!  SET X=1")) (Right 3)
  assertEqual ctx "parse: WHEN OTHER"
    (fmap (take 1) (stmtsOf "WHEN OTHER WRITE 1")) (Right [SWhenOther])
  assertEqual ctx "parse: WHEN ANY ALSO >500"
    (fmap (take 1) (stmtsOf "WHEN ANY ALSO >500 WRITE 1"))
    (Right [SWhen [AnyTest, CompareTest Gt (num "500")] Nothing])
  assertEqual ctx "parse: WHEN '=X is a negated comparison, not a unary NOT"
    (fmap (take 1) (stmtsOf "WHEN '=X WRITE 1"))
    (Right [SWhen [CompareTest NotEq (var "X")] Nothing])
  assertEqual ctx "parse: WHEN >=5 reuses the expression parser's spelling (NotLt)"
    (fmap (take 1) (stmtsOf "WHEN >=5 WRITE 1"))
    (Right [SWhen [CompareTest NotLt (num "5")] Nothing])
  assertEqual ctx "parse: WHEN 'X is a plain value test of a unary NOT"
    (fmap (take 1) (stmtsOf "WHEN 'X WRITE 1"))
    (Right [SWhen [ValueTest (UnExpr Not (var "X"))] Nothing])
  assertEqual ctx "parse: WHEN TRUE WHERE guard"
    (fmap (take 1) (stmtsOf "WHEN TRUE WHERE N#2=0 WRITE 1"))
    (Right [SWhen [ValueTest (num "1")] (Just (BinExpr Eq (BinExpr Mod (var "N") (num "2")) (num "0")))])
  assertEqual ctx "parse: OTHERS and ANYTHING are ordinary identifiers, not OTHER / ANY"
    (fmap (take 1) (stmtsOf "WHEN OTHERS ALSO ANYTHING WRITE 1"))
    (Right [SWhen [ValueTest (var "OTHERS"), ValueTest (var "ANYTHING")] Nothing])
  assertEqual ctx "parse: END EVALUATE"
    (stmtsOf "END EVALUATE") (Right [SEndEvaluate])
  assertIsLeft ctx "parse: WHEN rejects a postcondition (§8)"
    (stmtsOf "WHEN:X 1 WRITE 1")
  assertIsLeft ctx "parse: EVALUATE needs a subject"
    (stmtsOf "EVALUATE")

  -- ---- Phase.Eval: END EVALUATE form ----
  let sw v = T.unlines
        [ " SET S=" <> v
        , " EVALUATE S"
        , " WHEN \"A\" WRITE \"a\""
        , " WHEN \"B\" WRITE \"b\""
        , " WHEN OTHER WRITE \"o\""
        , " END EVALUATE"
        , " WRITE \"!\"" ]
  r1 <- run (sw "\"B\"")
  assertEqual ctx "EVALUATE: a value clause matches" r1 (Right "b!")
  r2 <- run (sw "\"Z\"")
  assertEqual ctx "EVALUATE: WHEN OTHER runs when nothing matched" r2 (Right "o!")
  r3 <- run (T.unlines [" SET S=\"Z\"", " EVALUATE S", " WHEN \"A\" WRITE \"a\"", " END EVALUATE", " WRITE \"!\""])
  assertEqual ctx "EVALUATE: no match and no OTHER is not an error" r3 (Right "!")
  r4 <- run (T.unlines [" SET S=\"B\"", " EVALUATE S", " WHEN \"B\" WRITE \"1\"", " WHEN \"B\" WRITE \"2\"", " WHEN OTHER WRITE \"3\"", " END EVALUATE"])
  assertEqual ctx "EVALUATE: only the first matching clause runs (no fall-through)" r4 (Right "1")
  r5 <- run (T.unlines [" SET S=\"x\"", " EVALUATE S", " WHEN OTHER WRITE \"o\"", " WHEN \"x\" WRITE \"x\"", " END EVALUATE"])
  assertEqual ctx "EVALUATE: clauses are tried in order, so an early OTHER shadows later ones" r5 (Right "o")
  r6 <- run (T.unlines [" SET S=\"B\"", " EVALUATE S", " WHEN \"B\" WRITE \"b\"  WRITE \"2\"", " END EVALUATE"])
  assertEqual ctx "EVALUATE: a clause body can hold several statements" r6 (Right "b2")

  -- dotted spelling: identical behaviour
  r7 <- run (T.unlines [" SET S=\"B\"", " EVALUATE S", ". WHEN \"A\" WRITE \"a\"", ". WHEN \"B\" WRITE \"b\"", ". WHEN OTHER WRITE \"o\"", " WRITE \"!\""])
  assertEqual ctx "EVALUATE: dotted form selects the same clause" r7 (Right "b!")
  r8 <- run (T.unlines [" SET S=\"Z\"", " EVALUATE S", ". WHEN \"A\" WRITE \"a\"", ". WHEN OTHER WRITE \"o\"", " WRITE \"!\""])
  assertEqual ctx "EVALUATE: dotted form, OTHER" r8 (Right "o!")

  -- chain mode, comparisons, ALSO, WHERE
  let chain p = T.unlines
        [ " SET P=" <> p
        , " EVALUATE TRUE"
        , " WHEN P>1000 WRITE \"big\""
        , " WHEN P>100 WRITE \"mid\""
        , " WHEN OTHER WRITE \"small\""
        , " END EVALUATE" ]
  mapM_ (\(p, want) -> do
           r <- run (chain p)
           assertEqual ctx ("EVALUATE TRUE chain, P=" ++ T.unpack p) r (Right want))
    [("5000", "big"), ("500", "mid"), ("1", "small")]

  let cmp p = T.unlines
        [ " SET P=" <> p
        , " EVALUATE P"
        , " WHEN >1000 WRITE \"big\""
        , " WHEN >=100 WRITE \"mid\""
        , " WHEN '=7 WRITE \"not7\""
        , " WHEN OTHER WRITE \"seven\""
        , " END EVALUATE" ]
  mapM_ (\(p, want) -> do
           r <- run (cmp p)
           assertEqual ctx ("EVALUATE operator tests, P=" ++ T.unpack p) r (Right want))
    [("5000", "big"), ("100", "mid"), ("3", "not7"), ("7", "seven")]

  let also t n = T.unlines
        [ " SET T=\"" <> t <> "\",N=" <> n
        , " EVALUATE T ALSO N"
        , " WHEN \"PREMIUM\" ALSO >500 WRITE \"p-big\""
        , " WHEN \"PREMIUM\" ALSO ANY WRITE \"p\""
        , " WHEN ANY ALSO >500 WRITE \"big\""
        , " WHEN OTHER WRITE \"o\""
        , " END EVALUATE" ]
  mapM_ (\(t, n, want) -> do
           r <- run (also t n)
           assertEqual ctx ("EVALUATE ... ALSO, " ++ T.unpack t ++ "/" ++ T.unpack n) r (Right want))
    [("PREMIUM", "600", "p-big"), ("PREMIUM", "10", "p"), ("BASIC", "600", "big"), ("BASIC", "10", "o")]

  r9 <- run (T.unlines
    [ " FOR N=1:1:4 DO"
    , ". EVALUATE TRUE"
    , ". WHEN TRUE WHERE N#2=0 WRITE \"e\""
    , ". WHEN OTHER WRITE \"o\""
    , ". END EVALUATE" ])
  assertEqual ctx "EVALUATE: WHERE guard, inside a FOR body, END form" r9 (Right "oeoe")
  r10 <- run (T.unlines
    [ " FOR N=1:1:4 DO"
    , ". EVALUATE TRUE"
    , ".. WHEN TRUE WHERE N#2=0 WRITE \"e\""
    , ".. WHEN OTHER WRITE \"o\"" ])
  assertEqual ctx "EVALUATE: the same, fully dotted" r10 (Right "oeoe")

  -- clause bodies that open their own blocks, and nesting
  let nested a b = T.unlines
        [ " SET A=" <> a <> ",B=" <> b
        , " EVALUATE A"
        , " WHEN 1 DO"
        , ". EVALUATE B"
        , ". WHEN 1 WRITE \"11\""
        , ". WHEN OTHER WRITE \"1x\""
        , ". END EVALUATE"
        , " WHEN OTHER WRITE \"x\""
        , " END EVALUATE" ]
  mapM_ (\(a, b, want) -> do
           r <- run (nested a b)
           assertEqual ctx ("EVALUATE: nested, A=" ++ T.unpack a ++ " B=" ++ T.unpack b) r (Right want))
    [("1", "1", "11"), ("1", "2", "1x"), ("2", "1", "x")]
  r11 <- run (T.unlines
    [ " SET S=1", " EVALUATE S", " WHEN 1 DO", ". WRITE \"a\"", ". WRITE \"b\"", " WHEN OTHER WRITE \"o\"", " END EVALUATE" ])
  assertEqual ctx "EVALUATE: a WHEN ... DO clause runs its dotted body" r11 (Right "ab")

  -- EVALUATE is not a scope
  r12 <- run (T.unlines [" SET S=1", " EVALUATE S", " WHEN 1 WRITE \"one\"  QUIT", " END EVALUATE", " WRITE \"after\""])
  assertEqual ctx "EVALUATE: QUIT in a clause leaves the enclosing routine" r12 (Right "one")
  let Right envSign = defineItem "_GLOBAL" "SIGN" KindFunction (Just ["N"]) (T.unlines
        [ " EVALUATE TRUE"
        , " WHEN N>0 RETURN \"pos\""
        , " WHEN N<0 RETURN \"neg\""
        , " END EVALUATE"
        , " RETURN \"zero\"" ]) adminEnv
  mapM_ (\(n, want) -> do
           res <- runRoutine (case mapM parseLogicalLine (buildLogicalLines (" WRITE CALL SIGN(" <> n <> ")")) of Right ls -> ls; Left _ -> []) envSign
           assertEqual ctx ("EVALUATE: RETURN in a clause returns from the function, N=" ++ T.unpack n)
             (fmap getOutputText res) (Right want))
    [("5", "pos"), ("-5", "neg"), ("0", "zero")]

  -- postcondition on EVALUATE itself, case-insensitivity, errors
  r13 <- run (T.unlines [" SET S=1", " EVALUATE:0 S", " WHEN 1 WRITE \"x\"", " END EVALUATE", " WRITE \"end\""])
  assertEqual ctx "EVALUATE: a false postcondition skips the whole block" r13 (Right "end")
  r14 <- run (T.unlines [" SET S=1", " evaluate S", " when 2 WRITE \"two\"", " when 1 WRITE \"one\"", " when other WRITE \"o\"", " end evaluate"])
  assertEqual ctx "EVALUATE: keywords are case-insensitive" r14 (Right "one")
  r15 <- run " WHEN 1 WRITE \"x\""
  assertIsLeft ctx "EVALUATE: a WHEN with no EVALUATE is a runtime error" r15
  r16 <- run (T.unlines [" EVALUATE A ALSO B", " WHEN 1 WRITE \"x\"", " END EVALUATE"])
  assertIsLeft ctx "EVALUATE: a clause with the wrong number of tests is a runtime error" r16

  -- explicit DO ... END DO runs its body like a dotted DO
  r17 <- run (T.unlines [" DO", " WRITE \"in\"", " END DO", " WRITE \"out\""])
  assertEqual ctx "DO ... END DO runs its body once and continues" r17 (Right "inout")
  r18 <- run (T.unlines [" FOR I=1:1:3 DO", " WRITE I", " END DO", " WRITE \"!\""])
  assertEqual ctx "FOR ... DO ... END DO repeats the END-delimited body" r18 (Right "123!")
  r19 <- run (T.unlines [" DO", " WRITE \"a\"", " QUIT", " WRITE \"b\"", " END DO", " WRITE \"c\""])
  assertEqual ctx "DO ... END DO: QUIT leaves just the block, like a dotted DO" r19 (Right "ac")

-- --------------------------------------------------------------------
-- CONDITION / SIGNAL / HANDLE / CATCH / RETRY (Part III §1)
-- --------------------------------------------------------------------

testConditionsGroup :: TestContext -> IO ()
testConditionsGroup ctx = do
  adminEnv <- testAdminEnv
  let stmtsOf src = case parseLogicalLine (LogicalLine 0 Nothing src) of
        Right (Line _ _ ps) -> Right (map psStmt ps)
        Left e              -> Left (show e)
      -- Run a script in an environment; the error stays a real EvalError
      -- so a test can say exactly which condition escaped.
      runIn env src = case mapM parseLogicalLine (buildLogicalLines src) of
        Left pe -> pure (Left (SyntaxError (T.pack (show pe))))
        Right ls -> fmap (fmap getOutputText) (runRoutine ls env)
      run = runIn adminEnv
      var n = Var (mkRef Local n [])
      str = Lit . mkValue
      cond n fs = Condition n [(k, mkValue v) | (k, v) <- fs]
      isSyntaxError r = case r of Left (SyntaxError _) -> True; _ -> False
      -- Specifically the "RETRY is not allowed here" error, not just any <SYNTAX>.
      isRetryMisuse r = case r of Left (SyntaxError m) -> "RETRY" `T.isInfixOf` m; _ -> False

  -- ---- parsing ----
  assertEqual ctx "parse: CONDITION with fields"
    (stmtsOf "CONDITION <OOPS>(reason, code)") (Right [SCondition "OOPS" ["reason", "code"]])
  assertEqual ctx "parse: CONDITION with no field list, and names are upper-cased"
    (stmtsOf "CONDITION <oops>") (Right [SCondition "OOPS" []])
  assertEqual ctx "parse: condition names may contain - and _"
    (stmtsOf "CONDITION <LOCK-TIMEOUT_2>()") (Right [SCondition "LOCK-TIMEOUT_2" []])
  assertIsLeft ctx "parse: CONDITION rejects a postcondition (§8)" (stmtsOf "CONDITION:X <OOPS>")
  assertEqual ctx "parse: SIGNAL with fields (spaces after : and ,)"
    (stmtsOf "SIGNAL <OOPS>(reason: \"bad\", n: X)")
    (Right [SSignal "OOPS" [("reason", str "bad"), ("n", var "X")]])
  assertEqual ctx "parse: SIGNAL with no fields"
    (stmtsOf "SIGNAL <OOPS>") (Right [SSignal "OOPS" []])
  assertEqual ctx "parse: SIGNAL accepts a postcondition"
    (fmap length (stmtsOf "SIGNAL:X <OOPS>")) (Right 1)
  assertEqual ctx "parse: HANDLE with AS binding"
    (stmtsOf "HANDLE <CONFLICT> AS c") (Right [SHandle "CONFLICT" (Just "c")])
  assertEqual ctx "parse: HANDLE without AS"
    (stmtsOf "HANDLE <CONFLICT>") (Right [SHandle "CONFLICT" Nothing])
  assertEqual ctx "parse: CATCH <ANYTHING> AS e"
    (stmtsOf "CATCH <ANYTHING> AS e") (Right [SCatch "ANYTHING" (Just "e")])
  assertEqual ctx "parse: the rest of a clause line is its first body statements"
    (fmap length (stmtsOf "HANDLE <X> AS c  RETRY")) (Right 2)
  assertIsLeft ctx "parse: HANDLE rejects a postcondition (§8)" (stmtsOf "HANDLE:X <OOPS>")
  assertIsLeft ctx "parse: CATCH rejects a postcondition (§8)" (stmtsOf "CATCH:X <OOPS>")
  assertEqual ctx "parse: bare RETRY" (stmtsOf "RETRY") (Right [SRetry Nothing []])
  assertEqual ctx "parse: RETRY n TIMES"
    (stmtsOf "RETRY 3 TIMES") (Right [SRetry (Just (Lit (mkValue "3"))) []])
  assertEqual ctx "parse: RETRY 1 TIME (singular)"
    (stmtsOf "RETRY 1 TIME") (Right [SRetry (Just (Lit (mkValue "1"))) []])
  assertEqual ctx "parse: RETRY WITH, spaces around = and after the comma"
    (fmap (map (\st -> case st of SRetry Nothing ws -> map fst ws; _ -> [])) (stmtsOf "RETRY WITH a = 1, b=X"))
    (Right [["a", "b"]])
  assertEqual ctx "parse: RETRY n TIMES WITH ..."
    (fmap (map (\st -> case st of SRetry (Just _) ws -> map fst ws; _ -> [])) (stmtsOf "RETRY 2 TIMES WITH a=1"))
    (Right [["a"]])
  assertEqual ctx "parse: two spaces end a bare RETRY and start a new statement"
    (fmap length (stmtsOf "RETRY  WRITE 1")) (Right 2)
  assertIsLeft ctx "parse: one space after a bare RETRY is rejected like QUIT"
    (stmtsOf "RETRY WRITE 1")
  assertEqual ctx "Lines: HANDLE, CATCH and the others at column 0 are commands, not tags"
    (map llTag (buildLogicalLines "HANDLE <X> AS c\nCATCH <X>\nRETRY\nSIGNAL <X>\nCONDITION <X>"))
    (replicate 5 Nothing)

  -- ---- SIGNAL / CATCH basics ----
  r1 <- run (T.unlines
    [ "DO", "    WRITE \"before \"", "    SIGNAL <OOPS>(reason: \"bad\")", "    WRITE \"not reached\""
    , "CATCH <OOPS> AS e", "    WRITE \"caught: \",e(\"reason\")", "END DO", "WRITE \" after\"" ])
  assertEqual ctx "CATCH: runs, binds fields, and the program carries on after END DO"
    r1 (Right "before caught: bad after")
  r2 <- run " SIGNAL <X>(a: 1)"
  assertEqual ctx "SIGNAL with no handler is an ordinary halting error"
    r2 (Left (Signaled (cond "X" [("a", "1")])))
  assertEqual ctx "an unhandled signal prints as <NAME> field=value"
    (fmap formatError (either Just (const Nothing) r2)) (Just "<X> a=1")
  r3 <- run (T.unlines [ "DO", "SIGNAL <OOPS>(reason: \"bad\")", "CATCH <OOPS> AS e", "WRITE e(\"message\")", "END DO" ])
  assertEqual ctx "the automatic message field is <NAME> followed by the fields"
    r3 (Right "<OOPS> reason=bad")
  r4 <- run (T.unlines [ "DO", "SIGNAL <OOPS>(message: \"custom\")", "CATCH <OOPS> AS e", "WRITE e(\"message\")", "END DO" ])
  assertEqual ctx "a message field the signaller supplied wins over the generated one" r4 (Right "custom")
  r5 <- run (T.unlines [ "DO", "SIGNAL <OOPS>", "CATCH <OOPS>", "WRITE \"no binding needed\"", "END DO" ])
  assertEqual ctx "AS is optional" r5 (Right "no binding needed")
  r6 <- run (T.unlines [ "DO", "SIGNAL <oops>", "CATCH <OOPS>", "WRITE \"same\"", "END DO" ])
  assertEqual ctx "condition names are case-insensitive" r6 (Right "same")
  r7 <- run (T.unlines
    [ "DO", "SIGNAL <A>(a: 1)", "CATCH <A> AS e", "END DO"
    , "DO", "SIGNAL <B>(b: 2)", "CATCH <B> AS e", "WRITE $DATA(e(\"a\")),$DATA(e(\"b\"))", "END DO" ])
  assertEqual ctx "a binding is cleared before each new condition, so no stale fields" r7 (Right "01")
  r8 <- run (T.unlines [ "DO", "SIGNAL <X>", "CATCH <X>", "END DO", "WRITE \"after\"" ])
  assertEqual ctx "a clause with an empty body swallows the condition and the program continues"
    r8 (Right "after")
  r9 <- run (T.unlines [ " DO", " . SIGNAL <X>", " . CATCH <X>", " . WRITE \"dotted\"" ])
  assertEqual ctx "clauses work in the dotted spelling too" r9 (Right "dotted")

  -- ---- clause selection ----
  r10 <- run (T.unlines
    [ "DO", "SIGNAL <OOPS>", "CATCH <ANYTHING>", "WRITE \"any\"", "CATCH <OOPS>", "WRITE \"specific\"", "END DO" ])
  assertEqual ctx "a specific clause beats <ANYTHING> even when ANYTHING is written first" r10 (Right "specific")
  r11 <- run (T.unlines [ "DO", "SIGNAL <WEIRD>", "CATCH <OOPS>", "WRITE \"oops\"", "CATCH <ANYTHING>", "WRITE \"any\"", "END DO" ])
  assertEqual ctx "<ANYTHING> catches whatever no other clause names" r11 (Right "any")
  r12 <- run (T.unlines [ "DO", "SIGNAL <WEIRD>", "CATCH <OOPS>", "WRITE \"oops\"", "END DO", "WRITE \"unreached\"" ])
  assertEqual ctx "no matching clause: the signal goes on and halts" r12 (Left (Signaled (cond "WEIRD" [])))
  r13 <- run (T.unlines
    [ "DO", "DO", "SIGNAL <A>", "CATCH <B>", "WRITE \"inner\"", "END DO", "CATCH <A>", "WRITE \"outer\"", "END DO" ])
  assertEqual ctx "an unmatched signal reaches the next enclosing handler" r13 (Right "outer")
  r14 <- run (T.unlines
    [ "DO", "SIGNAL <A>", "CATCH <A>", "SIGNAL <B>", "CATCH <B>", "WRITE \"same block\"", "END DO" ])
  assertEqual ctx "a signal raised BY a clause is not caught by the same block's other clauses"
    r14 (Left (Signaled (cond "B" [])))

  -- ---- what a caught signal undoes ----
  r15 <- run (T.unlines
    [ "DO", "SET a=1", "SET ^CT(2)=7", "SIGNAL <X>", "CATCH <X>", "WRITE $DATA(a),$DATA(^CT(2))", "END DO" ])
  assertEqual ctx "effects of lines BEFORE the failing one survive, locals and globals" r15 (Right "11")
  r16 <- run (T.unlines
    [ "DO", "SET a=1  SET ^CT(1)=5  WRITE \"gone\"  SIGNAL <X>", "CATCH <X>", "WRITE $DATA(a),$DATA(^CT(1)),\"|\"", "END DO" ])
  assertEqual ctx "the failing LINE is atomic: its sets, global writes and output are dropped" r16 (Right "00|")
  r17 <- run (T.unlines
    [ "DO", "FOR I=1:1:5 DO", ". IF I=3 SIGNAL <STOP>(at: I)", ". WRITE I", "CATCH <STOP> AS s", "WRITE \"|\",s(\"at\")", "END DO" ])
  assertEqual ctx "a FOR containing the error is one atomic unit (documented granularity)" r17 (Right "|3")
  r18 <- run (T.unlines
    [ " FOR I=1:1:3 DO", " . IF I=2 SIGNAL <SKIP>", " . WRITE I", " . CATCH <SKIP>", " . WRITE \"s\"" ])
  assertEqual ctx "a handler inside a FOR body is applied per iteration" r18 (Right "1s3")

  -- ---- signals from inside calls ----
  let Right envThrow = defineItem "_GLOBAL" "THROW" KindFunction (Just []) " SIGNAL <FROMFN>(where: \"fn\")" adminEnv
  r19 <- runIn envThrow (T.unlines [ "DO", "WRITE CALL THROW()", "CATCH <FROMFN> AS e", "WRITE e(\"where\")", "END DO" ])
  assertEqual ctx "a signal raised in a called function, in expression position, is caught by the caller's block"
    r19 (Right "fn")
  let Right envDecl = defineItem "_GLOBAL" "THROW2" KindFunction (Just []) " SIGNAL <D>(undeclared: 1)" envThrow
  r20 <- runIn envDecl (T.unlines [ " CONDITION <D>(only)", " DO", " WRITE CALL THROW2()", " CATCH <D>", " WRITE \"c\"", " END DO" ])
  assertEqual ctx "CONDITION declarations belong to their own routine: the callee is not bound by the caller's"
    r20 (Right "c")

  -- ---- CONDITION declarations ----
  r21 <- run (T.unlines [ " CONDITION <D>(reason)", " DO", " SIGNAL <D>(reason: \"ok\")", " CATCH <D> AS e", " WRITE e(\"reason\")", " END DO" ])
  assertEqual ctx "a declared field is accepted" r21 (Right "ok")
  r22 <- run (T.unlines [ " CONDITION <D>(reason)", " DO", " SIGNAL <D>(reasno: \"typo\")", " CATCH <ANYTHING>", " WRITE \"caught\"", " END DO" ])
  assertEqual ctx "an undeclared field is a <SYNTAX> error, which a condition handler does not catch"
    (isSyntaxError r22) True
  r23 <- run (T.unlines [ " DO", " SIGNAL <D>(reasno: \"typo\")", " CATCH <D>", " WRITE \"c\"", " END DO", " CONDITION <D>(reason)" ])
  assertEqual ctx "a declaration after its first use still applies (collected up front)" (isSyntaxError r23) True
  r24 <- run (T.unlines [ " DO", " SIGNAL <E>(anything: 1)", " CATCH <E>", " WRITE \"c\"", " END DO" ])
  assertEqual ctx "an undeclared condition has no field restriction" r24 (Right "c")

  -- ---- RETRY ----
  r25 <- run (T.unlines
    [ " SET n=0", "DO", "    SET n=n+1", "    IF n<3 SIGNAL <AGAIN>(count: n)", "    WRITE \"done n=\",n"
    , "HANDLE <AGAIN> AS c", "    WRITE \"retry \",c(\"count\"),\" \"", "    RETRY", "END DO" ])
  assertEqual ctx "RETRY re-runs the body; effects of earlier passes are kept (n keeps counting)"
    r25 (Right "retry 1 retry 2 done n=3")
  r26 <- run (T.unlines
    [ " SET n=0", "DO", "    IF n<1 SIGNAL <AGAIN>(next: 10)", "    WRITE \"n=\",n"
    , "HANDLE <AGAIN> AS c", "    RETRY WITH n = c(\"next\")", "END DO" ])
  assertEqual ctx "RETRY WITH sets locals for the next pass, evaluated while the binding is still in scope"
    r26 (Right "n=10")
  r27 <- run (T.unlines
    [ " SET n=0", "DO", "    SET n=n+1", "    SIGNAL <AGAIN>", "HANDLE <AGAIN>", "    RETRY 2 TIMES", "    WRITE \"gave up after \",n", "END DO" ])
  assertEqual ctx "RETRY 2 TIMES: two re-entries (three runs), then it falls through to the next statement"
    r27 (Right "gave up after 3")
  r28 <- run (T.unlines
    [ " SET n=0", "DO", "    SET n=n+1", "    SIGNAL <AGAIN>", "HANDLE <AGAIN>", "    RETRY 1 TIME", "    WRITE \"n=\",n", "END DO" ])
  assertEqual ctx "RETRY 1 TIME: one re-entry" r28 (Right "n=2")
  r29 <- run (T.unlines
    [ " SET n=0", "DO", "    SET n=n+1", "    IF n<3 SIGNAL <AGAIN>", "    WRITE \"n=\",n", "HANDLE <AGAIN>", "    RETRY 5 TIMES", "END DO" ])
  assertEqual ctx "RETRY n TIMES stops as soon as the body succeeds" r29 (Right "n=3")
  r30 <- run (T.unlines
    [ " SET n=0", "DO", "    SET n=n+1", "    IF n<3 SIGNAL <AGAIN>", "    WRITE \"n=\",n", "HANDLE <AGAIN>", "    IF 1 DO", "    . RETRY", "    WRITE \"unreachable\"", "END DO" ])
  assertEqual ctx "RETRY inside a nested block of the clause unwinds all the way out" r30 (Right "n=3")
  r31 <- run (T.unlines [ " SET ^_CONFIG(\"retry\",\"max\")=5", " DO", " SIGNAL <X>", " HANDLE <X>", " RETRY", " END DO" ])
  assertEqual ctx "a plain RETRY that never succeeds hits the configured ceiling"
    (either (\e -> "RETRY limit reached" `T.isInfixOf` formatError e && "5 times" `T.isInfixOf` formatError e) (const False) r31) True
  r32 <- run (T.unlines [ " DO", " SIGNAL <X>", " HANDLE <X>", " RETRY", " END DO" ])
  assertEqual ctx "the default ceiling is 1000"
    (either (\e -> "1000 times" `T.isInfixOf` formatError e) (const False) r32) True
  r33 <- run (T.unlines [ " SET ^_CONFIG(\"retry\",\"max\")=3", " SET n=0", " DO", " SET n=n+1", " SIGNAL <X>", " HANDLE <X>", " RETRY 10 TIMES", " WRITE \"n=\",n", " END DO" ])
  assertEqual ctx "the ceiling also bounds a counted RETRY"
    (either (\e -> "RETRY limit reached" `T.isInfixOf` formatError e) (const False) r33) True

  r34 <- run " RETRY"
  assertEqual ctx "RETRY outside any block is a <SYNTAX> error" (isRetryMisuse r34) True
  r35 <- run (T.unlines [ " DO", " SIGNAL <X>", " CATCH <X>", " RETRY", " END DO" ])
  assertEqual ctx "RETRY in a CATCH clause is a <SYNTAX> error (only HANDLE retries)" (isRetryMisuse r35) True
  r36 <- run (T.unlines [ " DO", " RETRY", " HANDLE <X>", " WRITE 1", " END DO" ])
  assertEqual ctx "RETRY in the body of a block, not in a HANDLE clause, is a <SYNTAX> error" (isRetryMisuse r36) True
  let Right envRetryer = defineItem "_GLOBAL" "RETRYER" KindFunction (Just []) (T.unlines [ " RETRY", " RETURN 1" ]) adminEnv
  r37 <- runIn envRetryer (T.unlines [ " DO", " SIGNAL <X>", " HANDLE <X>", " SET v=CALL RETRYER()", " END DO" ])
  assertEqual ctx "RETRY inside a function called from a HANDLE clause is not lexically in it" (isRetryMisuse r37) True
  r38 <- run " HANDLE <X>"
  assertEqual ctx "a stray HANDLE with no block is a <SYNTAX> error" (isSyntaxError r38) True

  -- ---- built-in conditions ----
  r39 <- run (T.unlines [ "DO", "WRITE 1/0", "CATCH <DIVIDE-BY-ZERO> AS e", "WRITE e(\"dividend\"),\"/\",e(\"divisor\")", "END DO" ])
  assertEqual ctx "<DIVIDE-BY-ZERO> carries dividend and divisor" r39 (Right "1/0")
  r40 <- run (T.unlines [ "DO", "WRITE 7#0", "CATCH <DIVIDE-BY-ZERO> AS e", "WRITE e(\"dividend\"),\"/\",e(\"divisor\")", "END DO" ])
  assertEqual ctx "<DIVIDE-BY-ZERO> from modulo too" r40 (Right "7/0")
  r41 <- run (T.unlines [ "DO", "WRITE 9\\0", "CATCH <DIVIDE-BY-ZERO>", "WRITE \"idiv\"", "END DO" ])
  assertEqual ctx "<DIVIDE-BY-ZERO> from integer division too" r41 (Right "idiv")
  r42 <- run " WRITE 1/0"
  assertEqual ctx "with no handler present, divide by zero stays the plain error" r42 (Left DivideByZero)
  r43 <- run (T.unlines [ "DO", "WRITE 1/0", "CATCH <OTHER>", "END DO" ])
  assertEqual ctx "a handler that does not name it: the built-in condition halts"
    r43 (Left (Signaled (cond "DIVIDE-BY-ZERO" [("dividend", "1"), ("divisor", "0")])))
  r44 <- run (T.unlines [ "DO", "WRITE \"a\"", "CATCH <X>", "END DO", "WRITE 1/0" ])
  assertEqual ctx "after the block ends the handler is gone: plain error again" r44 (Left DivideByZero)
  r45 <- run (T.unlines [ "DO", "SIGNAL <X>", "CATCH <X>", "END DO", "WRITE 1/0" ])
  assertEqual ctx "...also after a caught signal" r45 (Left DivideByZero)
  let Right envDiv = defineItem "_GLOBAL" "DIVIDER" KindFunction (Just []) " RETURN 1/0" adminEnv
  r46 <- runIn envDiv (T.unlines [ "DO", "WRITE CALL DIVIDER()", "CATCH <DIVIDE-BY-ZERO>", "WRITE \"in callee\"", "END DO" ])
  assertEqual ctx "a handler applies to everything in its dynamic extent, callees included" r46 (Right "in callee")

  r47 <- run (T.unlines [ "DO", "WRITE nope(1,2)", "CATCH <UNDEFINED> AS e", "WRITE e(\"name\"),\"|\",e(\"path\")", "END DO" ])
  assertEqual ctx "<UNDEFINED> for a local carries name and path" r47 (Right "nope|1,2")
  r48 <- run (T.unlines [ "DO", "WRITE ^NOPE(5)", "CATCH <UNDEFINED> AS e", "WRITE e(\"name\"),\"|\",e(\"path\")", "END DO" ])
  assertEqual ctx "<UNDEFINED> for a global carries name and path" r48 (Right "^NOPE|5")
  r49 <- run " WRITE nope"
  assertEqual ctx "with no handler present an undefined variable stays the plain error"
    (either (\e -> case e of UndefinedVariable _ _ -> True; _ -> False) (const False) r49) True
  r50 <- run (T.unlines [ "DO", "WRITE $GET(nope,\"dflt\")", "CATCH <UNDEFINED>", "WRITE \"caught\"", "END DO" ])
  assertEqual ctx "$GET of an undefined variable is not an error, so does not signal" r50 (Right "dflt")

  -- LOCK-TIMEOUT: another job already holds ^A(1)
  let heldByOther = case acquireLocks "OTHERJOB" [GlobalPath "A" [mkValue "1"]] emptyLockTable of
        (_, t) -> t
      envLocked = adminEnv { envLocks = heldByOther }
  r51 <- runIn envLocked (T.unlines
    [ "DO", "LOCK +^A(1):7", "CATCH <LOCK-TIMEOUT> AS e", "WRITE e(\"global\"),\"|\",e(\"path\"),\"|\",e(\"timeoutMs\")", "END DO" ])
  assertEqual ctx "<LOCK-TIMEOUT> when a handler is present: global, path, timeoutMs" r51 (Right "A|1|7")
  r52 <- runIn envLocked " LOCK +^A(1):7"
  assertEqual ctx "with no handler a failed LOCK still does not signal" (fmap (const ()) r52) (Right ())
  r53 <- runIn envLocked (T.unlines [ "DO", "LOCK +^A(2)", "WRITE \"got it\"", "CATCH <LOCK-TIMEOUT>", "WRITE \"timeout\"", "END DO" ])
  assertEqual ctx "a LOCK that succeeds under a handler does not signal" r53 (Right "got it")

  -- ---- plain blocks are untouched ----
  r54 <- run (T.unlines [ "DO", "WRITE 1/0", "END DO" ])
  assertEqual ctx "a block with no clause stays an ordinary block: no handler is present" r54 (Left DivideByZero)

-- --------------------------------------------------------------------
-- Main
-- --------------------------------------------------------------------

main :: IO ()
main = do
  ctx <- newTestContext
  putStrLn "Starting PhaseDB Haskell Test Suite..."

  runGroup ctx "Phase.Value" testValueGroup
  runGroup ctx "Phase.Parser" testParserGroup
  runGroup ctx "Phase.Lines" testLinesGroup
  runGroup ctx "Phase.Stmt" testStmtGroup
  runGroup ctx "EVALUATE / WHEN and END blocks (Part II §5)" testEvaluateGroup
  runGroup ctx "CONDITION / SIGNAL / HANDLE / CATCH / RETRY (Part III §1)" testConditionsGroup
  runGroup ctx "Phase.Tree" testTreeGroup
  runGroup ctx "Phase.Eval (Expr)" testEvalExprGroup
  runGroup ctx "Phase.Eval (Stmt & Routine)" testEvalStmtGroup
  runGroup ctx "Phase.Catalog (^_CATALOG Repository & Versioning)" testCatalogGroup
  runGroup ctx "Phase.Storage (MVCC, WAL, Transactions, Time Travel & Locks)" testStorageGroup
  runGroup ctx "$CHILDCOUNT intrinsic" testChildCountGroup
  runGroup ctx "REF(...) dynamic references (Part VI §1)" testRefDynamicGroup
  runGroup ctx "Security (null subscripts, ^_CATALOG ownership, last-admin safeguard)" testSecurityGroup
  runGroup ctx "Naked references and $PIECE/$EXTRACT SET targets" testNakedAndLvalueGroup

  passed <- readIORef (tcPassed ctx)
  failed <- readIORef (tcFailed ctx)
  putStrLn "\n========================================"
  putStrLn $ "Tests passed: " ++ show passed
  putStrLn $ "Tests failed: " ++ show failed
  putStrLn "========================================"

  if failed > 0
    then exitFailure
    else do
      putStrLn "\ESC[32mAll PhaseDB tests passed successfully!\ESC[0m"
      exitSuccess

