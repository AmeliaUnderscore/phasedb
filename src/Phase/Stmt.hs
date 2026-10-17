{-# LANGUAGE OverloadedStrings #-}

-- | The Phase statement/command parser. Consumes one already-joined
-- 'Phase.Lines.LogicalLine' at a time and produces a 'Line', which,
-- per handbook §5.1, may hold SEVERAL statements, not just one. The
-- canonical example is
-- @IF X>0 WRITE "positive",! ELSE  WRITE "not positive",!@: four
-- statements (@IF@, @WRITE@, @ELSE@, @WRITE@) on one line, separated
-- by plain whitespace, each of which may carry its own independent
-- postcondition (§8). 'pLine' parses this as a sequence of
-- 'PredStmt's, stopping at end of input.
--
-- == Whitespace rules at THIS level (different from 'Phase.Parser')
--
-- 'Phase.Parser' enforces zero inter-token whitespace throughout,
-- because that is genuinely true /inside/ an expression, subscript
-- list, argument list, or target list (prior-work finding, carried
-- over from the Java implementation). Statement level is different:
--
--   * NO whitespace between a command word and its OWN postcondition's
--     leading @:@, every handbook example shows @WRITE:X>0@,
--     @QUIT:I=""@, @SET:DONE@ with the colon flush against the word.
--   * Arbitrary whitespace IS allowed between a command word (or its
--     postcondition, if present) and the start of its argument list,
--     e.g. the handbook's own working example has @SET SUM=0@ and
--     @WRITE "SUM=",SUM,!@ with a single space there.
--   * Whitespace is ALSO what separates one statement from the next
--     ON THE SAME LINE (§5.1), once one statement's own argument
--     grammar has consumed everything it recognizes, any following
--     text (after whitespace) is tried as the START of a new
--     statement, not as more of the previous one.
--   * Once we're inside the argument expression(s) themselves, we
--     hand off to 'Phase.Parser', which goes back to zero-whitespace
--     mode, exactly as established there.
--
-- == Postcondition vs. a command's own colon syntax (§8)
--
-- "A command's own postcondition, when present, is always the FIRST
-- `:` right after the command word, before any of that command's own
-- colon syntax." We implement this literally, and, this matters,
-- POSITION IT CORRECTLY: each command's postcondition is parsed by
-- 'pKeyword', called from WITHIN that command's own parser
-- immediately after its keyword matches, never independently at the
-- start of a whole statement attempt. (An earlier draft of this
-- module tried to parse a bare @:expr@ once, generically, before even
-- looking at the command word, which cannot work, since the colon in
-- @SET:DONE@ comes after "SET", not before it; that draft's
-- postcondition parsing could never actually succeed for any real
-- statement. This was caught and fixed before being shipped anywhere
-- outside this port's own history.) Only after the postcondition slot
-- is resolved (present or absent) does a command move on to its own
-- argument grammar, which, for `FOR`/`LOCK`/`UNLOCK`, has further
-- colons of its own (@start:inc:stop@, @:timeout@) that are NOT
-- postconditions, because by that point the postcondition slot has
-- already been consumed.
--
-- Postconditions attach to ONE statement, not to the whole line
-- (§8, §5.1): 'PredStmt' pairs each 'Stmt' with its own
-- 'Maybe Expr' postcondition. Each command parser returns this pair
-- directly (see 'pStmt''s type); 'pLine' just assembles the sequence.
--
-- `NAMESPACE`, `CONDITION`, `HANDLE`, `CATCH`, and `WHEN` do not
-- accept a postcondition at all (§8), moot for now since all five
-- currently parse as 'SUnsupported' (Parts III/V not yet
-- implemented), but flagged here so it isn't forgotten when those
-- parts are built: whichever parser eventually handles them must use
-- a variant that does NOT call 'pPostCond' at all (i.e. must not use
-- 'pKeyword' as-is), rather than silently accepting one it should
-- reject.
--
-- == Single space vs. double space (the team's rule, not in the handbook)
--
-- Some commands' arguments are entirely optional AND, when present,
-- start with a bare identifier or expression that on its own could
-- also just be a completely unrelated NEXT statement's command word
--, @QUIT@, @RETURN@, @NEW@, @KILL@, bare @DO@. Is @WRITE@ in
-- @QUIT WRITE@ the value being quit with, or the start of a fresh
-- @WRITE@ statement? the team's resolution: a SINGLE space commits to
-- reading whatever follows as this command's own argument (a
-- collision with a real command keyword there is now a parse error,
-- not a graceful reinterpretation); a DOUBLE space (or more) always
-- means a fresh statement follows, full stop, matching the
-- handbook's own @ELSE  WRITE@ example, which uses two spaces. See
-- 'pOptionalArg' for the implementation and 'pOptionalArgNoKeywordGuard'
-- for the narrower variant `FOR` needs (its argument grammar always
-- requires more syntax after the leading identifier, so it has no
-- bare-standalone-identifier reading to collide with a following
-- statement in the first place).
--
-- == No abbreviations (§9)
--
-- "Phase's parser recognizes full command words only." Every keyword
-- parser here checks a trailing word-boundary (the next character is
-- not alphanumeric) so that, e.g., a hypothetical identifier starting
-- with "IF" is never misread as the `IF` command.
module Phase.Stmt
  ( pLine
  , parseLogicalLine
  ) where

import Data.Char (isSpace)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Phase.AST
import qualified Phase.AST as AST
import Phase.Lines (LogicalLine (..))
import Phase.Json (pJsonValue)
import Phase.Parser (PParser, caseInsensitiveKeywordString, pCallTarget, pCompareBinOp, pExpr, pIdentifier, pPinSpec, pRef)

-- --------------------------------------------------------------------
-- Entry points
-- --------------------------------------------------------------------

-- | Parse one already-joined logical line's text into a full 'Line',
-- carrying over the depth/tag that 'Phase.Lines' already worked out.
parseLogicalLine :: LogicalLine -> Either ParseError AST.Line
parseLogicalLine ll = runParser (pLine ll <* eof) () "" (llText ll)

-- | The statement-level parser for one logical line's text: repeatedly
-- parses one full 'PredStmt' (keyword, its own postcondition, then
-- its own arguments, see 'pKeyword'), then either whitespace-then-
-- another, or end of input. Takes the already-resolved 'LogicalLine'
-- (for its depth/tag) but parses only 'llText', depth and tag are
-- NOT re-derived here, they're carried straight through from what
-- 'Phase.Lines' already decided.
--
-- IMPORTANT: postcondition parsing does NOT happen here, independently
-- of which command follows. An earlier draft tried to parse a bare
-- @:expr@ at the very START of each statement attempt, before even
-- looking at the command word, that is simply wrong, since the colon
-- in @SET:DONE@ comes AFTER "SET", not before it, so a check for a
-- leading colon at the very start of the statement can never succeed.
-- Every real command's postcondition is parsed by that command's OWN
-- parser, immediately after its keyword matches (via 'pKeyword'),
-- which is the only position where the colon can actually appear.
pLine :: LogicalLine -> PParser AST.Line
pLine ll = do
  stmts <- pPredStmt `sepEndBy1'` spacesOnly1
  pure (Line (llDepth ll) (llTag ll) stmts)
  where
    -- Snapshot the input exactly as it stands at the START of this
    -- statement attempt (before even its keyword is consumed), a
    -- plain 'Text' value, captured via 'getInput' and threaded down
    -- as an ordinary function argument, NOT Parsec user state (which
    -- would force 'Phase.Parser.PParser''s type to change everywhere
    -- it's used). Only 'pSet' currently uses this snapshot, to
    -- recover the complete original text of a statement it needs to
    -- bail out of, see 'bailToUnsupported'.
    pPredStmt = do
      stmtStart <- getInput
      uncurry PredStmt <$> pStmt stmtStart

-- | Like 'sepBy1', but for statement sequences: separated by ONE OR
-- MORE spaces/tabs, with NO trailing separator required or expected
-- (parsing simply stops, via the final 'pStmt' alternative's own
-- backtracking, once no further statement can be parsed after the
-- last whitespace run). Spelled out rather than reusing a stock
-- Parsec combinator so the "separator is mandatory whitespace, not
-- optional and not a comma" fact stays visible at the call site,
-- mirroring the same auditability reasoning behind
-- 'Phase.Parser.chainl1'' and this module's own 'sepBy1''.
sepEndBy1' :: PParser a -> PParser () -> PParser [a]
sepEndBy1' p sep = do
  x <- p
  xs <- many (try (sep *> p))
  pure (x : xs)

-- --------------------------------------------------------------------
-- Postcondition (§8)
-- --------------------------------------------------------------------

-- | Parse a postcondition immediately following a command word: a
-- bare @:@ (no space before it) followed by an expression (no space
-- after it either, the expression parser is zero-whitespace
-- throughout, per 'Phase.Parser'). Returns 'Nothing' if there is no
-- colon at all at this position (i.e. this is not a failure, a
-- postcondition is always optional). Called ONLY from 'pKeyword',
-- immediately after a command's own keyword has matched, never
-- independently at the start of a whole statement attempt (see
-- 'pLine''s docs for why that positioning would be wrong).
pPostCond :: PParser (Maybe Expr)
pPostCond = optionMaybe (try (char ':') *> pExpr)

-- | Match a command keyword, with a trailing word-boundary check,
-- THEN immediately parse that command's own optional postcondition
-- (§8), the two always belong together, since the postcondition's
-- colon can only ever appear glued to the command word itself, and
-- every command parser in this file needs both steps done in this
-- exact order before its own argument grammar runs. Returns the
-- postcondition so callers can thread it through to their own
-- 'PredStmt' result; wrapped in 'try' so a failed keyword match
-- backtracks cleanly for 'pStmt''s alternation, exactly as the old
-- bare 'keyword' helper did.
--
-- `NAMESPACE`, `CONDITION`, `HANDLE`, `CATCH`, and `WHEN` reject a
-- postcondition outright (§8: "The parser throws an error if a `:`
-- follows any of these five words"), moot for now since all five
-- currently parse as 'SUnsupported' (Parts III/V not yet
-- implemented), but flagged here so it isn't forgotten when those
-- parts are built: whichever parser eventually handles them must use
-- a variant that does NOT call 'pPostCond' at all, rather than this
-- one.
pKeyword :: Text -> PParser (Maybe Expr)
pKeyword kw = try (caseInsensitiveKeywordString kw *> notFollowedBy alphaNum) *> pPostCond

-- | Zero or more spaces/tabs. Used ONLY at the statement level, at
-- the specific boundary between a command word (or its
-- postcondition) and the start of its argument list, never inside
-- an expression, subscript list, argument list, or target list,
-- where 'Phase.Parser' governs and whitespace is never tolerated.
spacesOnly :: PParser ()
spacesOnly = skipMany (char ' ' <|> char '\t')

-- | One or more spaces/tabs, used specifically as the mandatory
-- separator BETWEEN two statements on the same line (§5.1). Distinct
-- from 'spacesOnly' (zero-or-more) because a statement boundary
-- genuinely requires at least one space; "SET X=1SET Y=2" with no
-- gap at all is not two statements, it's a single malformed one.
spacesOnly1 :: PParser ()
spacesOnly1 = skipMany1 (char ' ' <|> char '\t')

-- | Every command word this parser recognizes. Used only by
-- 'pOptionalArg''s ambiguity guard (see below), nowhere else needs a
-- reified list, since 'pStmt' otherwise just tries each command's own
-- parser directly.
reservedKeywords :: [Text]
reservedKeywords =
  [ "SET", "KILL", "WRITE", "IF", "ELSE", "FOR", "DO", "GOTO"
  , "QUIT", "RETURN", "NEW", "MERGE", "LOCK", "UNLOCK", "NAMESPACE"
  , "SIGNAL", "RETRY", "ZMERGE", "ZWRITE"
  ]

-- | The single-space/double-space split at the heart of
-- 'pOptionalArg', WITHOUT its keyword-collision guard. Exactly one
-- space here commits to parsing an argument with the supplied parser
-- (a failure past that point is a real parse error); two or more
-- spaces means "bare form, statement boundary," and only ONE of
-- those spaces is consumed (see 'pOptionalArg''s docs for exactly
-- why that's still correct).
--
-- Used directly by 'pFor', whose argument grammar ('pForSpec') always
-- requires more syntax after its leading identifier (a mandatory
-- @=@) and therefore has NO bare-standalone-identifier reading to be
-- ambiguous with a following statement's command word in the first
-- place, @FOR WRITE=1:1:10@ unambiguously uses @WRITE@ as the loop
-- variable's name, because no command can start with a literal @=@
-- immediately after its own word. Applying the full keyword-collision
-- guard here would be over-broad and would wrongly reject that
-- otherwise well-formed statement.
pOptionalArgNoKeywordGuard :: PParser a -> PParser (Maybe a)
pOptionalArgNoKeywordGuard argParser = do
  sawSpace <- option False (True <$ char ' ')
  if not sawSpace
    then pure Nothing
    else do
      sawSecondSpace <- lookAhead (option False (True <$ char ' '))
      if sawSecondSpace
        then pure Nothing
        else Just <$> argParser

-- | Parse an OPTIONAL trailing argument for a command whose bare form
-- (no argument at all) is also valid AND whose argument grammar
-- accepts a bare, standalone identifier/expression as a complete
-- parse on its own, @KILL@, @QUIT@, @RETURN@, @NEW@, and bare @DO@,
-- where a following recognized command keyword could otherwise be
-- misread as this command's own bare-identifier argument (e.g. is
-- @WRITE@ in @QUIT WRITE@ the value to quit with, or the start of a
-- new @WRITE@ statement?). NOT used by 'pFor', see
-- 'pOptionalArgNoKeywordGuard' for why its argument grammar doesn't
-- have this ambiguity to begin with.
--
-- the team's resolution, exact and by design: a single space introduces
-- THIS command's own argument; a double space (or more) always
-- starts a NEW statement, full stop, matching the handbook's own
-- @ELSE  WRITE@ example (two spaces) for the general
-- statement-separator convention. So:
--
--   * No space at all here → bare form, nothing consumed.
--   * Exactly one space, then a recognized command keyword (as a
--     full word) → this is exactly the case that needed a double
--     space to be unambiguous and didn't get one: a hard parse
--     error, NOT a silent fallback to the bare form.
--   * Exactly one space, then anything else → parse it with the
--     supplied argument parser. A failure here is now also a real
--     parse error (we're committed once we know it's not a
--     reserved-keyword collision), not a silent fallback.
--   * Two or more spaces → bare form; we consume NONE of that
--     whitespace here, leaving all of it for 'pLine''s own
--     inter-statement separator ('spacesOnly1') to consume as the
--     boundary before the next statement.
pOptionalArg :: PParser a -> PParser (Maybe a)
pOptionalArg argParser = do
  sawSpace <- option False (True <$ char ' ')
  if not sawSpace
    then pure Nothing  -- no space at all: bare form
    else do
      -- We've now unavoidably consumed exactly one space (we had to,
      -- to find out it was there). Peek at the NEXT character without
      -- consuming it: if it's another space, this was a 2+-space run,
      -- i.e. a statement boundary, not an argument-introducing single
      -- space. We leave that second (and any further) space entirely
      -- unconsumed here; 'pLine''s own 'spacesOnly1' separator picks
      -- up from there and consumes whatever whitespace remains (it
      -- only needs at least one, so this is correct regardless of
      -- exactly how many spaces remain).
      sawSecondSpace <- lookAhead (option False (True <$ char ' '))
      if sawSecondSpace
        then pure Nothing  -- 2+ spaces total: statement boundary, not an argument
        else do
          isKeyword <- lookAhead (option False (True <$ choice (map (try . keywordLookahead) reservedKeywords)))
          if isKeyword
            then unexpected "a command keyword where a single space suggested an argument was intended (use two spaces to start a new statement)"
            else Just <$> argParser
  where
    -- A reserved word only counts as "the start of a new statement"
    -- if it's followed by whitespace or end of input, exactly the
    -- shape every real Phase command requires between its own
    -- keyword and whatever comes next (bare form, or a
    -- space-then-argument). A keyword-shaped word followed
    -- immediately by something else, @(@, @,@, @=@, can never be
    -- the start of a real statement (no command attaches its
    -- argument with zero space), so it is unambiguously being used
    -- as this command's own bare tag/identifier/value instead. This
    -- is what lets @DO SET(1)@ (calling a tag actually named "SET",
    -- immediately followed by a parenthesized argument list) parse
    -- correctly, while still catching the genuine ambiguity in
    -- @QUIT WRITE@ (a bare keyword with nothing attached, which
    -- COULD start a real statement).
    keywordLookahead kw = caseInsensitiveKeywordString kw *> notFollowedBy alphaNum *> lookAhead (eof <|> () <$ satisfy (\c -> c == ' ' || c == '\t'))

-- --------------------------------------------------------------------
-- Statement dispatch
-- --------------------------------------------------------------------

-- | Try each known command in turn; anything that doesn't match falls
-- through to either a bare expression statement (Part XIV REPL
-- toplevel, "a line that is not a recognized statement, but that
-- stands entirely on its own as one complete Phase expression") or
-- 'SUnsupported', matching the handbook's own graceful-degradation
-- philosophy (§9: parses fine, throws @<NOTIMPL>@ only at execution).
-- Returns the postcondition alongside the statement, see 'pKeyword'
-- and 'pLine''s docs for why postcondition parsing must happen inside
-- each command's own parser rather than uniformly beforehand.
--
-- Takes 'stmtStart', the input snapshot from this statement's very
-- first character, before anything (even a keyword) has been
-- consumed, and threads it to every alternative as a plain argument.
-- Only 'pSet' actually uses it (to recover the complete original text
-- for a graceful 'SUnsupported' bailout on a recognized-but-
-- unimplemented extension of its own grammar); every other parser
-- ignores it. Threading it uniformly, rather than only to 'pSet',
-- keeps every command parser's signature the same shape and makes it
-- a one-line change to give another command the same capability
-- later (e.g. if @LOCK@ or @DO@ ever grow their own recognized
-- extensions).
pStmt :: Text -> PParser (Maybe Expr, Stmt)
pStmt stmtStart =
      pSet stmtStart
  <|> pNamespace
  <|> pMerge
  <|> pZMerge
  <|> pKill
  <|> pWrite
  <|> pZWrite
  <|> pIf
  <|> pElse
  <|> pFor
  <|> pEndDo
  <|> pEndEvaluate
  <|> pEvaluate
  <|> pWhen
  <|> pCondition
  <|> pSignal
  <|> pHandle
  <|> pCatch
  <|> pRetry
  <|> pDo
  <|> pGoto
  <|> pQuit
  <|> pReturn
  <|> pNew
  <|> pLock
  <|> pUnlock
  <|> pUnsupportedOrExpr

-- --------------------------------------------------------------------
-- SET
-- --------------------------------------------------------------------

-- | @SET t=v@, @SET t1=v1,t2=v2@. Note the keyword-word-boundary match
-- (inside 'pKeyword') is the only thing wrapped in 'try': once the
-- word @SET@ has genuinely matched (full word, correct boundary),
-- we're committed to this being a SET statement, and a malformed
-- assignment list past that point is a real parse error, not a signal
-- to quietly fall through to 'pUnsupportedOrExpr' as if this might
-- have been something else entirely. (An earlier draft wrapped the
-- whole parser in one big 'try', which, combined with
-- 'pUnsupportedOrExpr' being able to read a bare identifier like
-- @SET@ itself as a one-word expression, meant a typo like @SET X@
-- with no @=@ silently misparsed as an unrelated bare-expression
-- statement instead of failing clearly at the point of the typo.
-- Every other command parser below follows this same
-- keyword-only-commits discipline for the same reason.)
--
-- The @EXPECTING@ (optimistic concurrency, Part III §2) and
-- multi-value @CALL@-destructuring (Part II §1) forms ARE recognized
-- here, just not fully implemented. Immediately after parsing the
-- first target 'Ref', 'pRecognizedSetExtension' peeks for either
-- trailing shape (a bare @EXPECTING@ keyword, or @=CALL@ in place of
-- an ordinary value expression) and, if found, bails out to
-- 'bailToUnsupported', capturing this WHOLE statement (from @SET@
-- onward, using the 'stmtStart' snapshot 'pStmt' threaded in) as
-- 'SUnsupported' rather than hard-failing to parse it. See
-- 'bailToUnsupported''s own docs for the general mechanism this
-- relies on. Only a genuinely UNRECOGNIZED shape (a typo, garbage
-- after the target) still hard-fails, this is what closes the gap an
-- earlier draft of this port left open: "known extension, not yet
-- supported" now degrades gracefully; "not a real SET at all" still
-- errors clearly.
pSet :: Text -> PParser (Maybe Expr, Stmt)
pSet stmtStart = do
  postCond <- pKeyword "SET"
  spacesOnly
  try (pCallAssign postCond) <|> pStandardOrExpecting postCond
  where
    pCallAssign postCond = do
      targets <- pRef `sepBy1'` char ','
      spacesOnly
      _ <- char '='
      spacesOnly
      _ <- caseInsensitiveKeywordString "CALL" *> notFollowedBy alphaNum
      spacesOnly1
      target <- pCallTarget
      pure (postCond, SCallAssign targets target)

    pStandardOrExpecting postCond = do
      firstTarget <- pSetTarget
      mExpecting <- pRecognizedExpecting
      case mExpecting of
        Just () -> (,) postCond <$> bailToUnsupported "EXPECTING" stmtStart
        Nothing -> do
          spacesOnly
          firstVal <- char '=' *> spacesOnly *> pExpr
          rest <- many (try (spacesOnly *> char ',' *> spacesOnly *> pAssign))
          pure (postCond, SSet ((firstTarget, firstVal) : rest))

    pAssign = do
      target <- pSetTarget
      spacesOnly
      _ <- char '='
      spacesOnly
      val <- pExpr
      pure (target, val)

    -- | A SET target: an ordinary reference, or one of the two
    -- pseudo-lvalue forms (Part XIII) for in-place string surgery.
    -- Tried before falling back to 'pRef': nothing 'pRef' recognizes
    -- can start with '$', so there is no ambiguity to backtrack
    -- through, and a '$NAME' that ISN'T one of these two is a real
    -- parse error here, not a silent fallback to some other production.
    pSetTarget = pDollarTarget <|> (TargetRef <$> pRef)
      where
        pDollarTarget = do
          _ <- char '$'
          rawName <- pIdentifier
          case T.toUpper rawName of
            "PIECE"   -> pPieceArgs
            "EXTRACT" -> pExtractArgs
            other     -> parserFail (T.unpack ("$" <> other <> " cannot be used as a SET target"))

        -- @$PIECE(ref,delim[,which])=val@ -- 'which' defaults to 1,
        -- exactly like the expression-position @$PIECE@ it mirrors.
        pPieceArgs = between (char '(') (char ')') $ do
          r <- pRef
          spacesOnly *> char ',' *> spacesOnly
          delim <- pExpr
          mWhich <- optionMaybe (try (spacesOnly *> char ',' *> spacesOnly) *> pExpr)
          pure (TargetPiece r delim (fromMaybe (Lit (mkValue "1")) mWhich))

        -- @$EXTRACT(ref[,start[,end]])=val@ -- 'start' defaults to 1
        -- and 'end' to 'start', exactly like the expression-position
        -- @$EXTRACT@ it mirrors.
        pExtractArgs = between (char '(') (char ')') $ do
          r <- pRef
          mStart <- optionMaybe (try (spacesOnly *> char ',' *> spacesOnly) *> pExpr)
          mEnd   <- optionMaybe (try (spacesOnly *> char ',' *> spacesOnly) *> pExpr)
          let start = fromMaybe (Lit (mkValue "1")) mStart
              end   = fromMaybe start mEnd
          pure (TargetExtract r start end)

    pRecognizedExpecting = lookAhead $ optionMaybe $
      try (spacesOnly1 *> caseInsensitiveKeywordString "EXPECTING" *> notFollowedBy alphaNum *> pure ())

-- | The general "recognized-but-not-yet-supported extension of an
-- already-committed command" backout: given the offending KEYWORD
-- itself (e.g. @"EXPECTING"@, whatever the caller just peeked and
-- recognized as a real-but-unimplemented shape) and the ORIGINAL,
-- unconsumed text from this statement's very first character
-- (captured by 'pLine' before the command's keyword was even parsed),
-- consume whatever remains of the CURRENT parse position (so
-- 'parseLogicalLine' still reaches @eof@ successfully) and return
-- 'SUnsupported' carrying BOTH, the specific token for a precise
-- @<NOTIMPL>@ message, and the full statement for context, built
-- from the original snapshot, NOT from however much text happens to
-- remain at the point this is called. This is what lets a command
-- parser recognize a real, spec'd shape it doesn't implement yet (like
-- @SET@'s @EXPECTING@ clause) and degrade gracefully to
-- 'SUnsupported' for the WHOLE statement, rather than either
-- hard-failing (the old behavior) or silently misparsing a truncated
-- prefix of it.
--
-- Deliberately NOT implemented via Parsec's user state: threading a
-- snapshot through 'Text.Parsec' user state would force
-- 'Phase.Parser.PParser''s underlying type to carry that state
-- everywhere it's used, including in parsers that have nothing to do
-- with statement-level bailout. Plain function-argument threading
-- (see 'pStmt''s 'stmtStart' parameter) achieves the same result
-- without that ripple.
bailToUnsupported :: Text -> Text -> PParser Stmt
bailToUnsupported token stmtStart = SUnsupported token stmtStart <$ many anyChar

-- --------------------------------------------------------------------
-- NAMESPACE
-- --------------------------------------------------------------------

-- | @NAMESPACE name@ (Part V: compile-time / runtime namespace directive).
-- Allows a LEADING '_' for the same reason 'Phase.Parser.pIdentifier'
-- does (consistency with the special-global naming convention), but
-- deliberately not an internal one, same concatenation-operator
-- collision 'pIdentifier''s own docs explain.
pNamespace :: PParser (Maybe Expr, Stmt)
pNamespace = do
  postCond <- pKeyword "NAMESPACE"
  spacesOnly1
  c0 <- letter <|> char '%' <|> char '_'
  rest <- many (alphaNum <|> char '%' <|> char '.')
  let ns = T.pack (c0 : rest)
  pure (postCond, SNamespace ns)

-- --------------------------------------------------------------------
-- MERGE
-- --------------------------------------------------------------------

-- | @MERGE t=s@, @MERGE t1=s1,t2=s2@ (§9, Part X §1). Structurally
-- identical to 'pSet' minus the @EXPECTING@/@CALL@ extension
-- recognition, the handbook does not describe either shape for
-- @MERGE@, so there is nothing yet to peek for; a malformed target
-- list past the keyword is a genuine parse error, same discipline as
-- 'pSet'.
pMerge :: PParser (Maybe Expr, Stmt)
pMerge = do
  postCond <- pKeyword "MERGE"
  spacesOnly
  assigns <- pAssign `sepBy1'` char ','
  pure (postCond, SMerge assigns)
  where
    pAssign = do
      target <- pRef
      _ <- char '='
      source <- pRef
      pure (target, source)

-- | Comma-separated list with NO whitespace tolerated around the
-- commas themselves (the established no-whitespace rule covers
-- target lists explicitly), 'sepBy1' from Parsec would be fine here
-- too since we never call 'spaces', but we spell out our own version
-- for the same auditability reason 'Phase.Parser.chainl1'' does.
sepBy1' :: PParser a -> PParser sep -> PParser [a]
sepBy1' p sep = do
  x <- p
  xs <- many (try (sep *> p))
  pure (x : xs)

-- --------------------------------------------------------------------
-- ZMERGE
-- --------------------------------------------------------------------

-- | @ZMERGE target=json, ...@ where json may be an inline raw JSON value
-- or a Phase expression evaluating to a JSON string.
pZMerge :: PParser (Maybe Expr, Stmt)
pZMerge = do
  postCond <- pKeyword "ZMERGE"
  spacesOnly
  assigns <- pAssign `sepBy1'` (spacesOnly *> char ',' <* spacesOnly)
  pure (postCond, SZMerge assigns)
  where
    pAssign = do
      target <- pRef
      spacesOnly
      _ <- char '='
      spacesOnly
      source <- (ZMergeValue <$> try pJsonValue) <|> (ZMergeExpr <$> pExpr)
      pure (target, source)

-- --------------------------------------------------------------------
-- KILL
-- --------------------------------------------------------------------

-- | Bare @KILL@ (kills all locals) or @KILL target,target,...@. Uses
-- 'pOptionalArg', see its docs, for the same bare-vs-argument
-- ambiguity as @DO@/@QUIT@/@RETURN@/@NEW@ (e.g. @KILL WRITE ...@: is
-- @WRITE@ a target local named "WRITE", or the start of a new @WRITE@
-- statement? Single space commits to the former and is now a parse
-- error against a reserved word; double space means the latter).
pKill :: PParser (Maybe Expr, Stmt)
pKill = do
  postCond <- pKeyword "KILL"
  targets <- pOptionalArg (pRef `sepBy1'` char ',')
  pure (postCond, SKill targets)

-- --------------------------------------------------------------------
-- WRITE
-- --------------------------------------------------------------------

-- | @WRITE expr,expr,...@ where an item may be @!@ (newline), @#@
-- (form feed), or @?N@ (tab to column N), interspersed freely with
-- ordinary expressions in the comma list.
pWrite :: PParser (Maybe Expr, Stmt)
pWrite = do
  postCond <- pKeyword "WRITE"
  spacesOnly
  items <- pItems
  pure (postCond, SWrite items)
  where
    pItems = pItem `sepBy1'` char ','
    pItem =
          (Right WNewline <$ char '!')
      <|> (Right WFormFeed <$ char '#')
      <|> (try (Right . WTabTo . read <$> (char '?' *> many1 digit)))
      <|> (Left <$> pExpr)

-- --------------------------------------------------------------------
-- ZWRITE
-- --------------------------------------------------------------------

-- | Bare @ZWRITE@ (dumps all locals as JSON) or @ZWRITE target,target,...@.
pZWrite :: PParser (Maybe Expr, Stmt)
pZWrite = do
  postCond <- pKeyword "ZWRITE"
  targets <- pOptionalArg (pRef `sepBy1'` char ',')
  pure (postCond, SZWrite targets)

-- --------------------------------------------------------------------
-- IF / ELSE
-- --------------------------------------------------------------------

-- | @IF cond1,cond2,...@, comma means AND (short-circuits on the
-- first false one, per §5.1; that short-circuit behavior is an
-- EVALUATION concern for 'Phase.Eval', not something this parser
-- needs to encode, we simply capture the ordered list of conditions
-- here).
pIf :: PParser (Maybe Expr, Stmt)
pIf = do
  postCond <- pKeyword "IF"
  spacesOnly
  conds <- pExpr `sepBy1'` char ','
  pure (postCond, SIf conds)

-- | Bare @ELSE@, no condition of its own (§5.1), though, per §8,
-- @ELSE@ is not in the handful of commands that reject a
-- postcondition outright (that list is @NAMESPACE@/@CONDITION@/
-- @HANDLE@/@CATCH@/@WHEN@), so we still parse one here even though no
-- handbook example shows @ELSE:cond@ in practice; whether it's
-- meaningful is an evaluation-time question, not a parse-time one.
pElse :: PParser (Maybe Expr, Stmt)
pElse = do
  postCond <- pKeyword "ELSE"
  pure (postCond, SElse)

-- --------------------------------------------------------------------
-- FOR (classic forms and FOR EACH, Part IV §4)
-- --------------------------------------------------------------------

-- | Bare @FOR@; @FOR X=start@; @FOR X=start:inc@;
-- @FOR X=start:inc:stop@ (§5.2); @FOR EACH var IN collection@ (Part IV §4).
--
-- Uses 'pOptionalArgNoKeywordGuard', see its docs, for the same
-- bare-vs-argument SPACE-counting behavior @DO@/@QUIT@/@RETURN@/
-- @NEW@/@KILL@ need (e.g. in @FOR  WRITE X,!@, bare FOR, then a
-- separate WRITE statement, the double space must NOT be swallowed
-- as ordinary "whitespace before the for-spec" the way a plain
-- 'spacesOnly' would), but WITHOUT the keyword-collision guard: see
-- 'pOptionalArgNoKeywordGuard''s own docs for why 'pForSpec' has no
-- bare-standalone-identifier reading to collide with a following
-- statement's command word in the first place.
--
-- Per §8, `FOR`'s OWN colons (increment/stop) are distinct from a
-- postcondition's colon. The postcondition here is parsed by
-- 'pKeyword' immediately after @FOR@ itself matches, BEFORE
-- 'pForSpec' ever runs, so every colon 'pForSpec' sees is
-- unambiguously FOR's own increment/stop syntax, never a
-- postcondition, exactly matching the handbook's ordering rule.
pFor :: PParser (Maybe Expr, Stmt)
pFor = do
  postCond <- pKeyword "FOR"
  try (pForEach postCond) <|> pClassicFor postCond
  where
    pForEach postCond = do
      spacesOnly1
      _ <- caseInsensitiveKeywordString "EACH" *> notFollowedBy alphaNum
      spacesOnly1
      var <- pIdentifier
      spacesOnly1
      _ <- caseInsensitiveKeywordString "IN" *> notFollowedBy alphaNum
      spacesOnly1
      coll <- pRef
      pure (postCond, SForEach var coll)

    pClassicFor postCond = do
      spec <- pOptionalArgNoKeywordGuard pForSpec
      pure (postCond, SFor spec)

    pForSpec = do
      var <- pIdentifier
      _ <- char '='
      start <- pExpr
      minc <- optionMaybe (char ':' *> pExpr)
      mstop <- case minc of
        Nothing -> pure Nothing
        Just _  -> optionMaybe (char ':' *> pExpr)
      pure (ForSpec var start minc mstop)

-- --------------------------------------------------------------------
-- DO / GOTO and call targets
-- --------------------------------------------------------------------

-- | Bare @DO@ (dotted implicit block or explicit @END DO@, both
-- resolved by 'Phase.Lines', not here); @DO TAG@; @DO TAG(args)@;
-- @DO TAG^ROUTINE@; @DO ^ROUTINE@.
pDo :: PParser (Maybe Expr, Stmt)
pDo = do
  postCond <- pKeyword "DO"
  mWith <- optionMaybe pPinSpec
  case mWith of
    Just spec -> do
      -- @DO WITH @TX(...) [@BIND(...)]@ alone opens a dotted block;
      -- followed by one space and a call target it is a single pinned
      -- call instead. The same single-space/double-space rule as a
      -- plain @DO@ decides which.
      target <- pOptionalArg pCallTarget
      pure (postCond, SDoWith spec target)
    Nothing -> do
      target <- pOptionalArg pCallTarget
      pure (postCond, SDo target)

-- | @GOTO TAG@; @GOTO TAG^ROUTINE@; @GOTO ^ROUTINE@ (§5.3). Unlike
-- @DO@, @GOTO@ always requires a target, there is no bare @GOTO@, so
-- there is no bare-vs-argument ambiguity here at all: 'pCallTarget'
-- always runs, unconditionally, once "GOTO" has matched.
pGoto :: PParser (Maybe Expr, Stmt)
pGoto = do
  postCond <- pKeyword "GOTO"
  spacesOnly
  target <- pCallTarget
  pure (postCond, SGoto target)

-- --------------------------------------------------------------------
-- QUIT / RETURN
-- --------------------------------------------------------------------

-- | Bare @QUIT@ or @QUIT expr@. Uses 'pOptionalArg', see its docs,
-- to resolve the same bare-vs-argument ambiguity @DO@ has.
pQuit :: PParser (Maybe Expr, Stmt)
pQuit = do
  postCond <- pKeyword "QUIT"
  val <- pOptionalArg pExpr
  pure (postCond, SQuit val)

-- | Bare @RETURN@ (Part II §1's alternative to bare QUIT) or
-- @RETURN v1,v2,...@ (multi-value). 'pOptionalArg' hands back
-- 'Nothing' for the bare form; we flatten that to @[]@ here since
-- 'SReturn' has no separate "bare" constructor of its own.
pReturn :: PParser (Maybe Expr, Stmt)
pReturn = do
  postCond <- pKeyword "RETURN"
  vals <- pOptionalArg (pExpr `sepBy1'` char ',')
  pure (postCond, SReturn (maybe [] id vals))

-- --------------------------------------------------------------------
-- NEW
-- --------------------------------------------------------------------

-- | Bare @NEW@ (news every current local) or @NEW X,Y,Z@. Uses
-- 'pOptionalArg', see its docs, to resolve the same bare-vs-argument
-- ambiguity @DO@/@QUIT@ have (e.g. is @DO@ in @NEW DO@ this NEW's own
-- argument, or the start of a new @DO@ statement? Per Cox: a single
-- space means the former, and is now a parse error since @DO@ isn't a
-- valid local name; a double space means the latter).
pNew :: PParser (Maybe Expr, Stmt)
pNew = do
  postCond <- pKeyword "NEW"
  names <- pOptionalArg (pIdentifier `sepBy1'` char ',')
  pure (postCond, SNew names)

-- --------------------------------------------------------------------
-- LOCK / UNLOCK
-- --------------------------------------------------------------------

-- | Bare @LOCK@ (release/acquire everything) or
-- @LOCK ref,+ref,-ref[:timeout]@ (§9, Part XII §3). Uses
-- 'pOptionalArg', see its docs, for the same bare-vs-argument
-- ambiguity @KILL@/@NEW@/@QUIT@/@RETURN@/@DO@ have: a bare, unsigned
-- @ref@ item (e.g. @LOCK WRITE@) is indistinguishable in shape from a
-- following statement's command word, so a single space commits to
-- reading it as a lock target and a reserved-keyword collision there
-- is a parse error. A SIGNED item (@+ref@/@-ref@) has no such
-- ambiguity, no command starts with a literal @+@ or @-@ immediately
-- after its own word, but 'pOptionalArg' only inspects the character
-- immediately after the single space to decide bare-vs-argument, and
-- a leading sign character there means the keyword-collision branch
-- never even triggers (it only fires when what follows LOOKS LIKE a
-- bare reserved word).
pLock :: PParser (Maybe Expr, Stmt)
pLock = do
  postCond <- pKeyword "LOCK"
  specs <- pOptionalArg (pLockSpec `sepBy1'` char ',')
  pure (postCond, SLock specs)

-- | Bare @UNLOCK@ or @UNLOCK ref,+ref,-ref[:timeout]@. Same grammar
-- and same ambiguity treatment as 'pLock'.
pUnlock :: PParser (Maybe Expr, Stmt)
pUnlock = do
  postCond <- pKeyword "UNLOCK"
  specs <- pOptionalArg (pLockSpec `sepBy1'` char ',')
  pure (postCond, SUnlock specs)

-- | One @LOCK@/@UNLOCK@ list item: an optional leading sign, a
-- reference, and an optional trailing @:timeout@ expression (in
-- milliseconds, per the field's own doc in 'Phase.AST.LockSpec').
-- The @:timeout@ colon is unambiguous here for the same reason
-- @FOR@'s own colons are: this statement's own postcondition, if any,
-- was already consumed by 'pKeyword' immediately after @LOCK@/@UNLOCK@
-- matched, before 'pLockSpec' ever runs, so every colon it sees here
-- is necessarily its own timeout syntax.
pLockSpec :: PParser LockSpec
pLockSpec = do
  sign <- optionMaybe ((True <$ char '+') <|> (False <$ char '-'))
  ref <- pRef
  timeout <- optionMaybe (char ':' *> pExpr)
  pure (LockSpec sign ref timeout)

-- --------------------------------------------------------------------
-- END DO
-- --------------------------------------------------------------------

-- | Closes an explicit @DO...END DO@ block (§5.4, Part III). Unlike
-- every other command word here, this one is TWO words, @END@ is not
-- independently a statement keyword (it never appears alone in the
-- §9 command table), so it is deliberately absent from
-- 'reservedKeywords': a bare @END@ not followed by @DO@ poses no
-- keyword-collision risk to any other command's ambiguity guard,
-- since it could never parse as a genuine competing statement start.
--
-- The handbook's §8 list of postcondition-rejecting commands
-- (@NAMESPACE@/@CONDITION@/@HANDLE@/@CATCH@/@WHEN@) does not mention
-- @END DO@, so we parse an optional postcondition for it too, for the
-- same "don't forbid what the handbook doesn't forbid" reasoning as
-- 'pElse'.
pEndDo :: PParser (Maybe Expr, Stmt)
pEndDo = do
  try (caseInsensitiveKeywordString "END" *> spacesOnly1 *> caseInsensitiveKeywordString "DO" *> notFollowedBy alphaNum)
  postCond <- pPostCond
  pure (postCond, SEndDo)

-- --------------------------------------------------------------------
-- EVALUATE / WHEN / END EVALUATE (Part II §5)
-- --------------------------------------------------------------------

-- | A word that must stand alone: matches case-insensitively, then
-- requires that what follows is whitespace or the end of the line. The
-- stricter boundary (rather than just "not followed by a letter or
-- digit") keeps @TRUE@, @ANY@ and @OTHER@ from swallowing the front of
-- a longer expression such as @TRUE(1)@, @ANY_X@ or @OTHER+1@, which
-- must still parse as ordinary expressions.
pStandaloneWord :: Text -> PParser ()
pStandaloneWord w =
  try (caseInsensitiveKeywordString w *> notFollowedBy alphaNum *> lookAhead (eof <|> () <$ oneOf " \t"))

-- | @ALSO@, the separator between subjects (in an @EVALUATE@ header)
-- and between tests (in a @WHEN@ clause).
pAlso :: PParser ()
pAlso = spacesOnly1 *> pStandaloneWord "ALSO" *> spacesOnly1

-- | @EVALUATE subject [ALSO subject]...@. The bare word @TRUE@ is the
-- condition-chain subject ('SubjectTrue'); anything else is an
-- expression. The header holds no body: the @WHEN@ lines that follow
-- are separate logical lines, one level deeper ('Phase.Lines').
pEvaluate :: PParser (Maybe Expr, Stmt)
pEvaluate = do
  postCond <- pKeyword "EVALUATE"
  spacesOnly1
  first <- pSubject
  rest <- many (try (pAlso *> pSubject))
  pure (postCond, SEvaluate (first : rest))
  where
    pSubject = (SubjectTrue <$ pStandaloneWord "TRUE") <|> (SubjectExpr <$> pExpr)

-- | @WHEN test [ALSO test]... [WHERE guard]@ or @WHEN OTHER@, followed
-- on the SAME line by the clause's body statements (parsed by
-- 'pLine''s ordinary statement loop, since this parser stops right
-- after the header). Unlike nearly every other command, a clause takes
-- NO postcondition (§8): a @:@ glued to the word is a parse error, not
-- an ignored suffix.
pWhen :: PParser (Maybe Expr, Stmt)
pWhen = do
  pNoPostCondKeyword "WHEN"
  spacesOnly1
  (Nothing, SWhenOther) <$ pStandaloneWord "OTHER" <|> pClause
  where
    pClause = do
      first <- pTest
      rest <- many (try (pAlso *> pTest))
      guardExpr <- optionMaybe (try (spacesOnly1 *> pStandaloneWord "WHERE" *> spacesOnly1 *> pExpr))
      pure (Nothing, SWhen (first : rest) guardExpr)

    -- ANY, then a bare operator + operand, then a plain value. The
    -- operator alternative can't swallow a value that merely starts
    -- with a quote or bracket, since 'pCompareBinOp' only accepts
    -- complete operator tokens ('\'=', '>=', '[', ...), and a unary
    -- NOT such as @'x@ is not one of them, so it falls through to
    -- 'pExpr'.
    pTest =
          (AnyTest <$ pStandaloneWord "ANY")
      <|> (CompareTest <$> pCompareBinOp <*> pExpr)
      <|> (ValueTest <$> pValue)

    -- @TRUE@ inside a WHEN is the literal 1, so @WHEN TRUE@ matches in
    -- chain mode (and compares equal to a subject of 1 elsewhere).
    pValue = (Lit (mkValue "1") <$ pStandaloneWord "TRUE") <|> pExpr

-- | Closes an explicit @EVALUATE ... END EVALUATE@ block. Two words,
-- like 'pEndDo', and for the same reason @END@ is not itself a
-- reserved keyword. Parses an optional postcondition for the same
-- "don't forbid what the handbook doesn't forbid" reasoning as
-- 'pEndDo'.
pEndEvaluate :: PParser (Maybe Expr, Stmt)
pEndEvaluate = do
  try (caseInsensitiveKeywordString "END" *> spacesOnly1 *> caseInsensitiveKeywordString "EVALUATE" *> notFollowedBy alphaNum)
  postCond <- pPostCond
  pure (postCond, SEndEvaluate)

-- --------------------------------------------------------------------
-- CONDITION / SIGNAL / HANDLE / CATCH / RETRY (Part III §1)
-- --------------------------------------------------------------------

-- | A command word that takes NO postcondition (§8: "The parser throws
-- an error if a @:@ follows" @NAMESPACE@, @CONDITION@, @HANDLE@,
-- @CATCH@ or @WHEN@). The word match is the only thing wrapped in
-- 'try'; a colon right after it is a hard parse error, not something
-- to quietly skip.
pNoPostCondKeyword :: Text -> PParser ()
pNoPostCondKeyword kw = do
  try (caseInsensitiveKeywordString kw *> notFollowedBy alphaNum)
  colon <- optionMaybe (lookAhead (char ':'))
  case colon of
    Just _  -> unexpected ("a postcondition after " ++ T.unpack kw ++ " (" ++ T.unpack kw ++ " accepts none, handbook Part I §8)")
    Nothing -> pure ()

-- | @<NAME>@: a condition name in angle brackets. Letters, digits,
-- @-@ and @_@ after an initial letter, stored upper-cased and without
-- the brackets so @<divide-by-zero>@ and @<DIVIDE-BY-ZERO>@ are the
-- same condition.
pConditionName :: PParser Text
pConditionName = do
  _ <- char '<'
  c <- letter
  cs <- many (alphaNum <|> char '-' <|> char '_')
  _ <- char '>'
  pure (T.toUpper (T.pack (c : cs)))

-- | A comma between list items, with optional spaces on either side
-- (the handbook writes @(field1, field2)@ and @field=expr, field=expr@
-- with a space after each comma, though expressions themselves never
-- tolerate whitespace). Wrapped in 'try' so that a list's last item,
-- followed by a space that is NOT part of a comma, backtracks cleanly
-- to being the end of the list.
pListComma :: PParser ()
pListComma = try (spacesOnly *> char ',' *> spacesOnly)

-- | @CONDITION <NAME>(field1, field2, ...)@. The field list (and its
-- parentheses) may be absent or empty.
pCondition :: PParser (Maybe Expr, Stmt)
pCondition = do
  pNoPostCondKeyword "CONDITION"
  spacesOnly1
  name <- pConditionName
  fields <- option [] (char '(' *> spacesOnly *> (pIdentifier `sepBy` pListComma) <* spacesOnly <* char ')')
  pure (Nothing, SCondition name fields)

-- | @SIGNAL <NAME>(field: expr, field: expr, ...)@. Note the @:@
-- between a field and its value here, where @RETRY WITH@ uses @=@.
-- Spaces are allowed around the colon (the handbook writes
-- @field: expr@).
pSignal :: PParser (Maybe Expr, Stmt)
pSignal = do
  postCond <- pKeyword "SIGNAL"
  spacesOnly1
  name <- pConditionName
  fields <- option [] (char '(' *> spacesOnly *> (pField `sepBy` pListComma) <* spacesOnly <* char ')')
  pure (postCond, SSignal name fields)
  where
    pField = do
      f <- pIdentifier
      spacesOnly
      _ <- char ':'
      spacesOnly
      e <- pExpr
      pure (f, e)

-- | @HANDLE <NAME> [AS binding]@ and @CATCH <NAME> [AS binding]@: the
-- two clause heads share one grammar. As with @WHEN@, this parser
-- stops after the head; anything else on the same line is the start of
-- the clause's body, parsed by 'pLine''s ordinary statement loop.
pHandle, pCatch :: PParser (Maybe Expr, Stmt)
pHandle = pClauseHead "HANDLE" SHandle
pCatch  = pClauseHead "CATCH" SCatch

pClauseHead :: Text -> (Text -> Maybe Text -> Stmt) -> PParser (Maybe Expr, Stmt)
pClauseHead kw ctor = do
  pNoPostCondKeyword kw
  spacesOnly1
  name <- pConditionName
  binding <- optionMaybe (try (spacesOnly1 *> pStandaloneWord "AS" *> spacesOnly1 *> pIdentifier))
  pure (Nothing, ctor name binding)

-- | @RETRY@, @RETRY WITH f=e, ...@, @RETRY n TIMES@ and
-- @RETRY n TIMES WITH f=e, ...@ (@TIME@ is accepted for @TIMES@, so
-- @RETRY 1 TIME@ reads naturally). Uses 'pOptionalArg', see its docs,
-- for the same bare-vs-argument ambiguity @QUIT@/@DO@/@NEW@ have: one
-- space introduces this command's own argument, two or more start a
-- new statement. Spaces are allowed around the @=@ of a @WITH@ field
-- (the handbook writes @RETRY WITH expectedTx = c("actualTx")@).
pRetry :: PParser (Maybe Expr, Stmt)
pRetry = do
  postCond <- pKeyword "RETRY"
  (count, seeds) <- maybe (Nothing, []) id <$> pOptionalArg pRetryArgs
  pure (postCond, SRetry count seeds)
  where
    pRetryArgs =
          ((,) Nothing <$> pWith)
      <|> do
            n <- pExpr
            spacesOnly1
            _ <- pStandaloneWord "TIMES" <|> pStandaloneWord "TIME"
            seeds <- option [] (try (spacesOnly1 *> pWith))
            pure (Just n, seeds)

    pWith = do
      pStandaloneWord "WITH"
      spacesOnly1
      pSeed `sepBy1` pListComma

    pSeed = do
      f <- pIdentifier
      spacesOnly
      _ <- char '='
      spacesOnly
      e <- pExpr
      pure (f, e)

-- --------------------------------------------------------------------
-- Fallthrough: unrecognized command word, or a bare expression
-- --------------------------------------------------------------------

-- | Anything that reaches here is either (a) a bare expression
-- standing entirely on its own, the Part XIV REPL toplevel case,
-- or (b) a leading word this port doesn't yet implement (MACLIB,
-- TRANSACTION, NAMESPACE, EDGE, INDEX,
-- CONDITION/SIGNAL/HANDLE/CATCH/RETRY, DEFINE MACRO/DROP MACRO,
-- BLOBSAVE/BLOBMETA, or a genuinely unrecognized word).
--
-- We require the (a) reading to consume ALL remaining text in this
-- logical line (@<* eof@) before accepting it, NOT just "however
-- much of it happens to parse as an expression." Without that
-- requirement, an unsupported multi-word command like
-- @TRANSACTION START@ would silently fragment: 'pExpr' happily reads
-- the bare identifier @TRANSACTION@ as a one-word expression and
-- stops at the space, leaving @START@ behind to be independently
-- misread as ANOTHER bare-expression statement by the next iteration
-- of 'pLine''s statement loop, two meaningless
-- @SExpr (Var "TRANSACTION")@ / @SExpr (Var "START")@ results instead
-- of one clear @SUnsupported "TRANSACTION START"@. Requiring @eof@
-- means a partial expression match is rejected outright (backtracking
-- cleanly, since it's wrapped in 'try'), and 'pRestOfLine' below
-- captures the genuinely unrecognized construct as ONE unit, which
-- is the whole point of 'SUnsupported': "so a diagnostic can show the
-- user what wasn't handled," as a single coherent blob, not
-- fragments.
--
-- One consequence: 'pRestOfLine' now always captures the rest of THIS
-- LOGICAL LINE in full (there is no remaining statement boundary
-- within it to respect, precisely because we couldn't recognize one)
--, so an unsupported statement is always the LAST 'PredStmt' 'pLine'
-- produces for its line; nothing after it is separately parsed. That
-- matches every real unsupported command in the handbook's own
-- reference table (§9): none of them are shown coexisting with a
-- FOLLOWING recognized statement on the same source line.
--
-- The FIRST word of the remaining text becomes 'SUnsupported''s
-- offending-token field, e.g. @"TRANSACTION"@ out of
-- @"TRANSACTION START"@, so a @<NOTIMPL>@ error can point at
-- specifically what wasn't handled, rather than only being able to
-- show the whole statement text and leave the reader to find the
-- actual unrecognized word themselves.
--
-- A bare expression or an unrecognized leading word has no
-- postcondition slot of its own (there is no "command word" for a
-- colon to attach to), always returns 'Nothing' for that half of the
-- pair.
pUnsupportedOrExpr :: PParser (Maybe Expr, Stmt)
pUnsupportedOrExpr =
      try ((,) Nothing . SExpr <$> pExpr <* eof)
  <|> do
        rest <- pRestOfLine
        let token = T.takeWhile (not . isSpace) rest
        pure (Nothing, SUnsupported token rest)
  where
    pRestOfLine = T.pack <$> many anyChar
