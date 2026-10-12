{-# LANGUAGE OverloadedStrings #-}

-- | The Phase expression parser.
--
-- This module implements handbook Part I §4 and Appendix B: a genuine
-- precedence-climbing parser (unlike real M76's strict left-to-right
-- rule), plus §3 (references/subscripts) and Part IV's slice postfix.
--
-- == The no-whitespace rule
--
-- Prior work on the Java implementation established (and this port
-- must preserve): /Phase's parser tolerates no whitespace on either
-- side of commas or binary operators/, inside expressions, subscript
-- lists, argument lists, or target lists. That is: @1+2@ parses,
-- @1 + 2@ does not, at least not as a single expression; each token
-- would instead be read as the start of a new statement-level word.
--
-- We enforce this by making 'pExpr' and everything it calls consume
-- ZERO whitespace between tokens. 'Text.Parsec' consumers built with
-- 'Text.Parsec.Token' normally skip trailing whitespace after every
-- lexeme, which is exactly wrong here, so we do NOT use the standard
-- 'makeTokenParser' lexer for expression parsing. Instead each token
-- parser below is whitespace-agnostic by construction (it simply does
-- not call 'spaces' anywhere), and the boundary where whitespace
-- becomes legal again, between a command word and its argument list,
-- is handled one level up, in the (future) statement parser, not here.
--
-- == Precedence ladder (loosest to tightest)
--
-- @
-- 1  &  !                                     parseOr      (left)
-- 2  =  '=  <  '<  >  '>  [  '[  ]]  ']]  ?  '?   parseCompare (left)
-- 3  _                                        parseConcat  (left)
-- 4  +  -                                     parseAdditive (left)
-- 5  *  /  \  #                               parseMultiplicative (left)
-- 6  **                                       parsePower   (RIGHT)
-- 7  -  +  '  @  (unary, prefix)              parseUnary
-- @
--
-- plus slice syntax @s[start:end]@ binding tighter than everything
-- above (a postfix wrap, handled in 'pPostfix').
module Phase.Parser
  ( PParser
  , pExpr
  , pRef
  , pCallArg
  , pCallTarget
  , pCallExpr
  , pPinSpec
  , pIdentifier
  , pNamespaceIdentifier
  , parseExpr
  , caseInsensitiveKeywordString
  ) where

import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Parsec
import Text.Parsec.Text (Parser)
import Phase.AST

type PParser = Parser

-- | Match a keyword string case-insensitively: 'target' is always the
-- canonical, UPPERCASE spelling used at every call site (e.g. "SET",
-- "CALL", "EACH"), and this matches that word in the actual input
-- regardless of the input's own letter case ("set", "Set", "SeT",
-- "sET" all match "SET"). Deliberately narrow and used ONLY for
-- command-word/language-keyword matching, 'pStringLit' (Phase string
-- literals), identifiers, and tag names are entirely untouched by
-- this and remain exactly as case-sensitive as before. Parsec's base
-- library has no case-insensitive 'string' combinator, so this is
-- spelled out character by character via 'satisfy'. Lives here
-- (rather than in "Phase.Stmt", which is its main user) so
-- 'pCallExpr' below, an expression-position use of the @CALL@
-- keyword, can share the exact same matching rule as every
-- statement-level keyword, instead of duplicating it.
caseInsensitiveKeywordString :: Text -> PParser ()
caseInsensitiveKeywordString target = mapM_ matchChar (T.unpack target)
  where
    matchChar c = satisfy (\x -> toUpper x == c) *> pure ()

-- --------------------------------------------------------------------
-- Entry points
-- --------------------------------------------------------------------

-- | Parse a complete expression from 'Text', consuming the whole
-- input (trailing whitespace aside, callers doing statement-level
-- parsing should slice out just the expression's own text first,
-- since we do not skip inter-token whitespace mid-expression).
parseExpr :: Text -> Either ParseError Expr
parseExpr = runParser (pExpr <* eof) () ""

-- --------------------------------------------------------------------
-- Level 1: & (And), ! (Or), loosest, left-associative
-- --------------------------------------------------------------------

pExpr :: PParser Expr
pExpr = pOr

pOr :: PParser Expr
pOr = chainl1' pCompare orOp
  where
    orOp =  (BinExpr And <$ char '&')
        <|> (BinExpr Or  <$ char '!')

-- --------------------------------------------------------------------
-- Level 2: comparison / contains / sorts-after / pattern-match,
-- each with a `'`-negated form. Left-associative.
-- --------------------------------------------------------------------

pCompare :: PParser Expr
pCompare = chainl1' pConcat compareOp
  where
    -- Order matters: longer operators (]], '=, '[, etc.) must be
    -- tried before their shorter prefixes, and `'` must be
    -- disambiguated from the unary-NOT `'` that pUnary also uses,
    -- here it is always immediately followed by one of =,<,>,[,?,]
    -- because a bare `'` at this position is a negated-comparison
    -- prefix, never a standalone binary operator.
    --
    -- '>=' and '<=' are NOT their own 'BinOp' constructors, they are
    -- alternative spellings of the M-style negated forms Phase
    -- already has: '>=' ("greater than or equal") is exactly
    -- "not less than", i.e. 'NotLt' ('<'); '<=' ("less than or
    -- equal") is exactly "not greater than", i.e. 'NotGt' ('>').
    -- 'Phase.Eval.compareValues' produces a genuine total 'Ordering'
    -- (either a canonical-numeric compare or a string compare, never
    -- both, never incomparable), so "not LT" and "GTE" are exhaustively
    -- the same condition in every case, introducing separate
    -- constructors would only duplicate 'Phase.Eval''s existing
    -- 'NotLt'/'NotGt' evaluation logic for no semantic difference.
    -- The handbook itself never specifies '>='/'<=' at all (an
    -- acknowledged spec gap, not a documented design), so this is the
    -- most consistent choice available given what the language
    -- already establishes for negated comparisons.
    --
    -- Tried BEFORE the bare '<'/'>' forms since they share a prefix,
    -- 'char' would otherwise consume the '<' or '>' alone and leave a
    -- dangling '=' to confuse the next operand.
    compareOp =
          try (BinExpr NotSortsAfter <$ string "']]")
      <|> try (BinExpr SortsAfter    <$ string "]]")
      <|> try (BinExpr NotEq         <$ string "'=")
      <|> try (BinExpr NotLt         <$ string "'<")
      <|> try (BinExpr NotGt         <$ string "'>")
      <|> try (BinExpr NotContains   <$ string "'[")
      <|> try (BinExpr NotMatch      <$ string "'?")
      <|> try (BinExpr NotLt         <$ string ">=")
      <|> try (BinExpr NotGt         <$ string "<=")
      <|>     (BinExpr Eq            <$ char '=')
      <|>     (BinExpr Lt            <$ char '<')
      <|>     (BinExpr Gt            <$ char '>')
      <|>     (BinExpr Contains      <$ char '[')
      <|>     (BinExpr Match         <$ char '?')

-- --------------------------------------------------------------------
-- Level 3: _ (string concatenation). Left-associative.
-- --------------------------------------------------------------------

pConcat :: PParser Expr
pConcat = chainl1' pAdditive (BinExpr Concat <$ char '_')

-- --------------------------------------------------------------------
-- Level 4: binary + -. Left-associative.
-- --------------------------------------------------------------------

pAdditive :: PParser Expr
pAdditive = chainl1' pMultiplicative addOp
  where
    addOp =  (BinExpr Add <$ char '+')
         <|> (BinExpr Sub <$ char '-')

-- --------------------------------------------------------------------
-- Level 5: * / \ #. Left-associative.
-- --------------------------------------------------------------------

pMultiplicative :: PParser Expr
pMultiplicative = chainl1' pPower mulOp
  where
    mulOp =  (BinExpr Mul    <$ char '*')
         <|> (BinExpr Div    <$ char '/')
         <|> (BinExpr IntDiv <$ char '\\')
         <|> (BinExpr Mod    <$ char '#')

-- --------------------------------------------------------------------
-- Level 6: **, RIGHT-associative, the one exception.
-- --------------------------------------------------------------------

-- | @2**3**2@ must parse as @2**(3**2)@ (handbook §4 example: "512, **
-- is right-associative"). We implement this with explicit recursion
-- rather than 'chainl1'': parse one unary term, then optionally parse
-- @**@ followed by a RECURSIVE call back into 'pPower' itself (not
-- into 'pMultiplicative' or any looser level) so the right-hand side
-- greedily absorbs any further @**@ chain before this level returns.
pPower :: PParser Expr
pPower = do
  base <- pUnary
  option base $ do
    _ <- try (string "**")
    rhs <- pPower  -- right-recursive: binds the whole rest of the ** chain
    pure (BinExpr Pow base rhs)

-- --------------------------------------------------------------------
-- Level 7: unary prefix - + ' @  (tightest, before postfix slice)
-- --------------------------------------------------------------------

-- | Unary operators are prefix and, per the precedence table, bind
-- tighter than every binary operator, including **. Note this means
-- @-2**2@ parses as @(-2)**2@ = 4, NOT @-(2**2)@ = -4, that is what
-- "unary prefix (highest)" in Appendix B means, and we must not be
-- tempted to special-case unary-minus to bind looser than ** the way
-- some other languages do.
pUnary :: PParser Expr
pUnary =
      (UnExpr Neg      <$> (char '-' *> pUnary))
  <|> (UnExpr Pos      <$> (char '+' *> pUnary))
  <|> (UnExpr Not      <$> (char '\'' *> pUnary))
  <|> (UnExpr Indirect <$> (char '@' *> pUnary))  -- Part XV: parses; <NOTIMPL> at eval
  <|> pPostfix

-- --------------------------------------------------------------------
-- Postfix: slice syntax s[start:end], tighter than every operator
-- above, applied directly to the atom that precedes it.
-- --------------------------------------------------------------------

pPostfix :: PParser Expr
pPostfix = do
  base <- pAtom
  slices <- many (try pSliceSuffix)
  pure (foldl (\e (s, e') -> Slice e s e') base slices)

pSliceSuffix :: PParser (Maybe Expr, Maybe Expr)
pSliceSuffix = between (char '[') (char ']') $ do
  -- NOTE: this '[' / ']' pair is the slice syntax, textually
  -- identical to the Level-2 "contains" operator's '['. The two are
  -- disambiguated purely by position: pSliceSuffix only runs
  -- immediately after a postfix atom, inside pPostfix, so by the time
  -- we're here we've already committed to "this is a slice," not a
  -- comparison. Any true ambiguity in Phase's own grammar (if the
  -- Java parser has one) should be cross-checked against
  -- Parser.java's actual slice-vs-contains disambiguation before this
  -- is treated as final, flagged here rather than silently assumed.
  start <- optionMaybe pExpr
  _ <- char ':'
  end <- optionMaybe pExpr
  pure (start, end)

-- --------------------------------------------------------------------
-- Atoms: literals, references, parenthesized expressions, calls
-- --------------------------------------------------------------------

pAtom :: PParser Expr
pAtom =
      try pCallExpr
  <|> pParenExpr
  <|> pFuncCall
  <|> pStringLit
  <|> pNumberLit
  <|> (Var <$> pRef)

-- | Argument to a call: pass-by-reference (&ref, Part II §2) or pass-by-value (expression).
--
-- Parses a FULL 'Ref' after '&', including a global sigil, a
-- namespace qualifier, or subscripts, not just a bare local name.
-- The handbook is explicit that only a bare, unsubscripted local is
-- ever semantically valid here (everything else is rejected with
-- '<SYNTAX>'), but that check belongs to 'Phase.Eval', not to the
-- parser: parsing only a bare identifier after '&' meant '&^ORDERS'
-- (a global) choked with a raw, unhelpful Parsec error the moment it
-- hit the '^' sigil ("unexpected '^', expecting letter or '%'")
-- instead of being recognized as a syntactically well-formed
-- reference argument that Phase's own evaluator then rejects
-- cleanly. See 'Phase.AST.CallArg' for the fuller rationale.
pCallArg :: PParser CallArg
pCallArg =
      try (CallArgRef <$> (char '&' *> pRef))
  <|> (CallArgVal <$> pExpr)

-- | A call target (TAG, TAG^ROUTINE, ^ROUTINE) with optional parenthesized arguments.
pCallTarget :: PParser CallTarget
pCallTarget =
      try pTagAndRoutine
  <|> try pBareRoutine
  <|> pBareTag
  where
    pTagAndRoutine = do
      tag <- pNamespaceIdentifier
      _ <- char '^'
      routine <- pNamespaceIdentifier
      args <- optionMaybe pArgList
      pure (CallTarget (Just tag) (Just routine) args)

    pBareRoutine = do
      _ <- char '^'
      routine <- pNamespaceIdentifier
      args <- optionMaybe pArgList
      pure (CallTarget Nothing (Just routine) args)

    pBareTag = do
      tag <- pNamespaceIdentifier
      args <- optionMaybe pArgList
      pure (CallTarget (Just tag) Nothing args)

    pArgList = between (char '(') (char ')') (pCallArg `sepBy` char ',')

-- | Expression-position function invocation: @CALL target(args...)@ (Part II §1).
--
-- With a pin clause, @CALL WITH @TX(x) [@BIND(...)] target(args...)@, the
-- call runs inside its own pin scope ('CallWith', see 'pPinSpec').
pCallExpr :: PParser Expr
pCallExpr = do
  _ <- caseInsensitiveKeywordString "CALL" *> notFollowedBy alphaNum
  mPin <- optionMaybe pPinSpec
  _ <- skipMany1 (char ' ' <|> char '\t')
  target <- pCallTarget
  pure (maybe (Call target) (\spec -> CallWith spec target) mPin)

-- | The pin clause shared by @DO WITH@ ('Phase.Stmt.pDo') and
-- @CALL WITH@ ('pCallExpr'): one space, the word @WITH@, one space,
-- @\@TX(@ pin-argument @)@, then optionally one space and
-- @\@BIND(@ global, global, ... @)@ (no whitespace inside the list, like
-- every other Phase argument list).
--
-- The pin argument is either the bare word @LAST@ (release: back to live
-- data) or an ordinary expression — typically a global reference carrying
-- a relative time-travel bracket, e.g. @^[-2]FOO("BAR")@, or an absolute
-- transaction number.
--
-- Each bind-list item names a WHOLE global, spelled like a global
-- reference without subscripts: @^NAME@ or @NS^NAME@. A subscripted item
-- is a parse error on purpose — bindings are per global, not per path.
-- @\@BIND()@ (an empty list) is accepted: it binds nothing.
--
-- Fails WITHOUT consuming input unless the text starts with @ WITH
-- \@TX(@, so an ordinary @DO tag@/@CALL tag@ whose tag merely begins
-- with "WITH" still parses as before. Once that prefix has matched the
-- parser is committed: a malformed pin argument or @\@BIND@ list (say a
-- subscripted item) is a real parse error, not a silent fallback to
-- reading @DO WITH@ as a call to a tag named @WITH@.
pPinSpec :: PParser PinSpec
pPinSpec = do
  try (char ' ' *> caseInsensitiveKeywordString "WITH" *> char ' ' *> char '@'
         *> caseInsensitiveKeywordString "TX" *> lookAhead (char '('))
  pin <- between (char '(') (char ')') (try pPinLast <|> pExpr)
  bind <- optionMaybe pBindClause
  pure (PinSpec pin bind)
  where
    pPinLast = TimeTravelLast <$ (caseInsensitiveKeywordString "LAST" *> notFollowedBy alphaNum *> lookAhead (char ')'))

    pBindClause = do
      _ <- try (char ' ' *> char '@' *> caseInsensitiveKeywordString "BIND")
      between (char '(') (char ')') (pBindName `sepBy` char ',')

    pBindName = try pQualified <|> pBare

    pQualified = do
      ns <- pNamespaceIdentifier
      _ <- char '^'
      name <- pIdentifier
      pure (BindName (Just ns) name)

    pBare = do
      _ <- char '^'
      name <- pIdentifier
      pure (BindName Nothing name)

pParenExpr :: PParser Expr
pParenExpr = between (char '(') (char ')') pExpr

-- | String literal: double-quoted, with @""@ as the escape for a
-- literal quote character inside the string (the standard M/MUMPS
-- convention, the handbook's examples always show plain unescaped
-- text, so this is the one place we're extrapolating rather than
-- transcribing; flagged for verification against real Phase source
-- once we can check to be sure this quoting rule is actually used).
pStringLit :: PParser Expr
pStringLit = do
  _ <- char '"'
  chunks <- many (pEscapedQuote <|> pStringChar)
  _ <- char '"'
  pure (Lit (mkValue (T.concat chunks)))
  where
    pEscapedQuote = T.singleton <$> try (char '"' *> char '"')
    pStringChar   = T.singleton <$> noneOf "\""

-- | A numeric literal: digits, optional fraction. Deliberately has NO
-- leading-sign handling of its own, 'pUnary' always tries its own
-- @char '-'@/@char '+'@ alternatives before falling through to
-- 'pPostfix'/'pAtom'/'pNumberLit', so any leading sign on a number is
-- already consumed as a 'UnExpr' one level up by the time we get here.
-- Giving 'pNumberLit' its own optional sign as well would be dead
-- code at best; keep the sign-parsing in exactly one place (pUnary).
--
-- This is deliberately permissive about literal SHAPE (e.g. "007"
-- parses fine as a literal); whether "007" is canonical is a SEPARATE
-- question answered later by 'Phase.Value.canonicalNumber', not here.
pNumberLit :: PParser Expr
pNumberLit = do
  intPart <- many1 digit
  fracPart <- option "" $ do
    dot <- string "."
    digs <- many1 digit
    pure (dot ++ digs)
  pure (Lit (mkValue (T.pack (intPart ++ fracPart))))

-- | An intrinsic function call: @$NAME(arg,arg,...)@. Per the
-- no-whitespace rule, no space is tolerated between the name and the
-- opening paren, or around the argument commas.
--
-- Intrinsic names are uppercased at parse time so that 'Phase.Eval''s
-- 'evalIntrinsic' clauses (which match against uppercase literals such as
-- @"$ORDER"@, @"$DATA"@) work regardless of how the user typed the name.
-- This matches the same case-insensitivity Phase already applies to command
-- keywords via 'caseInsensitiveKeywordString', extended to intrinsics.
pFuncCall :: PParser Expr
pFuncCall = do
  _ <- char '$'
  name <- pIdentifier
  args <- option [] $ between (char '(') (char ')') (pExpr `sepBy` char ',')
  pure (FuncCall (T.cons '$' (T.map toUpper name)) args)

-- | A reference: @REF(name,sub,...)@, local (@X@, @SUM@, @%TEMP@) or
-- global (@^X@), with zero or more comma-separated subscripts.
-- 'pRefDynamic' is tried first, wrapped in 'try', so that a bare
-- identifier that happens to spell @REF@ but is NOT followed by @(@,
-- e.g. a local named @REF@, falls through cleanly to 'pLocal'.
pRef :: PParser Ref
pRef = try pRefDynamic <|> try pNamespaceGlobal <|> try pNakedGlobal <|> pBareGlobal <|> pLocal
  where
    -- | @REF(nameExpr, sub1, sub2, ...)@ dynamic reference (Part VI §1).
    -- Case-insensitive keyword, no whitespace inside the argument list.
    -- Requires at least one argument; zero is accepted at parse time and
    -- rejected at eval time (consistent with every other "wrong arity"
    -- shape in this codebase).
    pRefDynamic = do
      _ <- caseInsensitiveKeywordString "REF"
      args <- between (char '(') (char ')') (pExpr `sepBy` char ',')
      pure (RefDynamic args)

    pNamespaceGlobal = do
      ns <- pNamespaceIdentifier
      _ <- char '^'
      mTimeTravel <- optionMaybe $ try (between (char '[') (char ']') pTimeTravelExpr)
      name <- pIdentifier
      subs <- option [] $ between (char '(') (char ')') (pExpr `sepBy` char ',')
      pure (Ref Global (Just ns) name subs mTimeTravel)

    -- | A naked reference, @^(sub1,sub2,...)@, the sigil directly
    -- followed by a parenthesis, no name in between. At least one
    -- subscript is required: unlike a named global, a naked reference
    -- exists only to give the SAME global a different subscript list,
    -- so an empty one has no meaning (there is no such form in real M
    -- either). Tried before 'pBareGlobal' since that alternative's own
    -- 'pIdentifier' would otherwise fail hard on the '(' having already
    -- consumed the '^', with no 'try' on 'pBareGlobal' itself, that
    -- failure would not backtrack.
    --
    -- Deliberately does not accept a time-travel bracket
    -- (@^[tx](subs)@): 'Phase.AST.RefNaked' carries no such field, so
    -- that combination is simply not a recognized reference shape,
    -- consistent with 'RefDynamic' also not supporting time travel.
    pNakedGlobal = do
      _ <- char '^'
      subs <- between (char '(') (char ')') (pExpr `sepBy1` char ',')
      pure (RefNaked subs)

    pBareGlobal = do
      _ <- char '^'
      mTimeTravel <- optionMaybe $ try (between (char '[') (char ']') pTimeTravelExpr)
      name <- pIdentifier
      subs <- option [] $ between (char '(') (char ')') (pExpr `sepBy` char ',')
      pure (Ref Global Nothing name subs mTimeTravel)

    -- | The content of a time-travel bracket, @^[...]@: either the
    -- @LAST@ keyword (optionally followed by @-N@, ordinary
    -- subtraction against it), or an ordinary expression, an absolute
    -- transaction number, or, under relative revision addressing
    -- (docs/tx-pinning-design.md Part 1), a NEGATIVE one meaning "N
    -- revisions back" (transaction numbers are never negative in
    -- practice, so that range was free to repurpose). Tried as its own
    -- alternative rather than folded into 'pExpr' everywhere: @LAST@ is
    -- not a general-purpose value, it only means something in this
    -- one syntactic position, so making it a global keyword would
    -- risk colliding with an identifier named @LAST@ anywhere else in
    -- a program.
    -- Whitespace-agnostic by construction, same as every other 'pExpr'
    -- token parser in this module (see this file's own header comment):
    -- 'LAST-N' must be written with no space around the '-', exactly
    -- as plain '1-2' already must be.
    pTimeTravelExpr = try pLastMinus <|> try (TimeTravelLast <$ pLastKeyword) <|> pExpr
      where
        pLastKeyword = caseInsensitiveKeywordString "LAST" <* notFollowedBy alphaNum
        pLastMinus = do
          _ <- pLastKeyword
          _ <- char '-'
          n <- pExpr
          pure (BinExpr Sub TimeTravelLast n)

    pLocal = do
      name <- pIdentifier
      subs <- option [] $ between (char '(') (char ')') (pExpr `sepBy` char ',')
      pure (Ref Local Nothing name subs Nothing)

-- | A namespace-or-tag identifier. Allows a LEADING '_' (same reasoning
-- as 'pIdentifier': nothing can sit to its left, so it can never be
-- the concatenation operator) so that @_GLOBAL@, the Catalog's root
-- namespace, can be written explicitly: @CALL FOOBAR^_GLOBAL(X)@.
pNamespaceIdentifier :: PParser Text
pNamespaceIdentifier = do
  c0 <- letter <|> char '%' <|> char '_'
  rest <- many (alphaNum <|> char '%' <|> char '.')
  pure (T.pack (c0 : rest))

-- | An identifier: a local/global name, a tag name, a FOR/NEW/KILL
-- variable name, every place in this codebase that needs "a Phase
-- name" goes through this one parser (imported by 'Phase.Stmt' rather
-- than reimplemented there), so fixing it here fixes every call site
-- at once.
--
-- Allows a LEADING '_' (so @^_CATALOG@/@^_CONFIG@/@^_USERS@-style
-- special-global names, previously unreachable from real Phase source
-- text, now parse) but deliberately NOT an internal one after the
-- first character. This distinction matters: '_' is ALSO the string
-- concatenation operator (@A_B@ means "concatenate A and B", see the
-- precedence table in this module's own header), so an identifier
-- allowed to contain '_' internally would make @A_B@ genuinely
-- ambiguous between "one identifier named A_B" and "A concatenated
-- with B". A LEADING underscore has no such ambiguity: concatenation
-- always needs a real operand on both sides, and there is nothing to
-- the left of the very first character of a name, so @_USERS@ read on
-- its own can only ever be one identifier, never a concatenation.
-- This was a real, previously-invisible bug: the whole "special
-- global" convention this codebase already relies on used names that
-- could never actually be written in a real Phase script, because
-- @^_CATALOG@ and friends were ONLY ever constructed directly in
-- Haskell (e.g. 'Phase.Catalog.catalogGlobalName' as a literal
-- 'Text'), never round-tripped through this parser, so nothing ever
-- exercised this path until an admin tried to
-- @WRITE ^_USERS(...)@ directly from a script and hit a raw parse
-- error instead of reaching the special global they were plainly
-- meant to be able to name.
pIdentifier :: PParser Text
pIdentifier = do
  c0 <- letter <|> char '%' <|> char '_'
  rest <- many (alphaNum <|> char '%')
  pure (T.pack (c0 : rest))

-- --------------------------------------------------------------------
-- chainl1 with NO inter-token whitespace skipping
-- --------------------------------------------------------------------

-- | Our own left-associative chain combinator, spelled out rather
-- than reused from 'Text.Parsec.Combinator.chainl1', for exactly one
-- reason: it needs to be crystal-clear at each call site that no
-- 'spaces' or lexeme-whitespace-skipping sneaks in between the
-- operand and the operator. 'Text.Parsec.Combinator.chainl1' itself
-- doesn't add whitespace skipping either, but spelling it out here
-- keeps the whitespace invariant visible and auditable in one place
-- rather than trusting every call site to have used the "plain"
-- token parsers correctly.
chainl1' :: PParser Expr -> PParser (Expr -> Expr -> Expr) -> PParser Expr
chainl1' operand operator = do
  x <- operand
  rest x
  where
    rest x = option x $ do
      f <- try operator
      y <- operand
      rest (f x y)
