{-# LANGUAGE OverloadedStrings #-}

-- | Turns raw Phase source text into a sequence of logical lines,
-- resolving source-layout rules BEFORE any statement grammar runs,
-- broadly following the handbook's description of @RoutineBuilder@
-- in the Java implementation (Part I §5.4–§5.5, Part XIV), WITH ONE
-- DELIBERATE DEVIATION from the handbook, agreed with Hanko, on how a
-- continuation line joins onto the line above. See "Continuation
-- joining" below; everything else here matches the handbook as
-- written.
--
--   1. __Tags__ (labels) live at column 0 (handbook, Part XIV REPL
--      notes: "A tag needs column 0, exactly like real M routine
--      source"). Anything not at column 0 is an ordinary statement
--      line, conventionally indented by at least one space.
--   2. __Dot-depth block structure__ (§5.4): a bare `DO`'s implicit
--      block runs through every following line whose dot depth
--      (count of leading `.` characters, after the mandatory
--      indent) is greater than the `DO` line's own depth. This is a
--      pure source-layout fact, resolved here, NOT an AST shape,
--      per the handbook, "It does not build a nested-block AST
--      node. The routine stays a flat line list underneath." We
--      preserve that: 'LogicalLine' carries a depth number, not a
--      tree. Dot-count remains the SOLE signal for depth and for
--      triggering continuation, this part is intentionally
--      unchanged, specifically to avoid ever making a continuation
--      line ambiguous with a column-0 tag line (raw-indentation-based
--      continuation was considered and rejected for exactly this
--      reason: indentation has no reliable discrete unit the way an
--      explicit dot count does).
--
--   3. __Continuation joining (REVISED)__: a line whose dot count is
--      HIGHER than the depth reachable from the line above is a
--      continuation of the line above, exactly as in the handbook.
--      "Reachable depth" is unchanged too: the line above's own
--      depth, plus one more level reachable only when that line ends
--      with a bare @DO@.
--
--      Where we deviate: the handbook's original rule joins with NO
--      space ever added, requiring the author to type a trailing
--      space themselves if they wanted one. In practice this breaks
--      the handbook's own Part X §2 materializing-join example (a
--      `FOR EACH` line continued by two `MERGE` lines and a final
--      bare `DO`), none of those lines carry a trailing space in the
--      handbook source, so a literal no-space join concatenates
--      `^ORDERS` directly onto `MERGE` with nothing between them,
--      which does not parse.
--
--      The revised rule: a continuation joins onto the line above
--      WITH exactly one space inserted, by default. To get the OLD
--      no-space "hard join" behavior back, e.g. to split a long
--      string literal token mid-word, with no gap at all, place a
--      literal @|@ immediately after the depth-dots, before the
--      statement text (@".|text"@, no space between @|@ and the
--      dots or between @|@ and the text). The @|@ itself is stripped
--      and is not part of the joined text. We chose a new character
--      here rather than reusing anything already meaningful at that
--      position; @\\@ was considered and rejected because it is
--      already the integer-division operator and a continuation line
--      could legitimately need to start with, e.g., @\\5@ as real
--      Phase code.
--
-- This module's output ('LogicalLine') is what the statement parser
-- consumes. Nothing past this module should ever look at a leading
-- dot, a glue marker, or count physical vs. logical lines again,
-- that ambiguity is resolved once, here.
module Phase.Lines
  ( LogicalLine (..)
  , buildLogicalLines
  , endsWithBareDo
  , needsContinuation
  ) where

import Data.Char (isAlphaNum, isLetter, toUpper)
import Data.Text (Text)
import qualified Data.Text as T

-- | One logical line, after continuation-joining. 'llDepth' is the
-- FIRST physical line's dot depth (a continuation line's own dot
-- count was already consumed to decide continuation-vs-new-statement
-- and is not separately recorded, a continuation carries no depth of
-- its own, only its text feeds into the accumulated line).
--
-- 'llTag' is 'Just name' when this line starts at column 0 with a
-- bare word, i.e. this line both defines a tag AND (per the
-- handbook's minimal example, "MAIN ; entry point") may still carry
-- statement text after the tag name on the same physical line.
data LogicalLine = LogicalLine
  { llDepth :: Int
  , llTag   :: Maybe Text
  , llText  :: Text   -- ^ statement text, with tag (if any) already stripped off the front
  } deriving (Eq, Show)

-- | Build logical lines from raw source. This does three passes
-- conceptually (split physical lines / strip tags & measure depth /
-- join continuations) but we do it as one fold to avoid materializing
-- an intermediate "line with unresolved continuation" type that
-- nothing else needs.
buildLogicalLines :: Text -> [LogicalLine]
buildLogicalLines src =
  let physical = map classify (T.lines src)
  in joinContinuations physical

-- --------------------------------------------------------------------
-- Step 1: classify each physical line, tag? depth? bare-DO-ending?
-- --------------------------------------------------------------------

-- | A classified physical line, before continuation joining. We keep
-- this as an internal type (not exported) since a caller should only
-- ever see the post-joining 'LogicalLine'.
data RawLine = RawLine
  { rlDepth   :: Int
  , rlTag     :: Maybe Text
  , rlGlued   :: Bool  -- ^ True when this line had an explicit @|@
                       -- hard-join marker right after its depth-dots.
                       -- Meaningless (ignored) on a tag line or on
                       -- whatever line happens to start a fresh
                       -- logical line rather than continue one, it
                       -- only matters to 'joinContinuations' at the
                       -- moment THIS line is being considered as a
                       -- possible continuation of the line above.
  , rlText    :: Text  -- ^ text AFTER the tag, AFTER the dots, and
                       -- AFTER the glue marker (if present), with
                       -- exactly the leading whitespace after those
                       -- stripped
  } deriving (Eq, Show)

classify :: Text -> RawLine
classify line
  -- Column 0, non-blank, doesn't start with '.'
  -- A word at column 0 is a tag ONLY if it is a valid identifier AND not a reserved command word.
  -- Otherwise, it is an ordinary statement or expression line (per handbook Part XIV REPL notes).
  | not (T.null line)
  , let c0 = T.head line
  , c0 /= ' ', c0 /= '\t', c0 /= '.' =
      let (firstWord, rest) = T.break (\c -> c == ' ' || c == '\t') line
          baseWord = T.takeWhile (\c -> c /= ':' && c /= '(' && c /= '=') firstWord
      in if isTagIdentifier c0 baseWord && not (isCommandKeyword (T.toUpper baseWord))
           then RawLine 0 (Just firstWord) False (T.stripStart rest)
           else RawLine 0 Nothing False line

  -- Otherwise: an ordinary statement line. It may be a plain-indented
  -- line (depth 0, "conventionally indented by at least one space"
  -- per Part XIV) or a dot-prefixed line (depth = number of leading
  -- dots, per §5.4/§5.5). We strip ALL leading whitespace first (the
  -- handbook: "The parser strips all leading whitespace after the
  -- dots"), but note dots come BEFORE any of that stripped
  -- whitespace, per every example in the handbook (". WRITE K,!",
  -- ".  MERGE ..."). Immediately after the dots, an optional @|@
  -- glue marker may appear (no space before it, it sits directly
  -- against the last dot); anything after THAT, once more
  -- whitespace-stripped, is the statement text.
  | otherwise =
      let afterLeadWs = T.stripStart line
          (dots, afterDots) = T.span (== '.') afterLeadWs
          depth = T.length dots
          (glued, afterGlue) = case T.uncons afterDots of
            Just ('|', rest) -> (True, rest)
            _                -> (False, afterDots)
          text = T.stripStart afterGlue
      in RawLine depth Nothing glued text

-- --------------------------------------------------------------------
-- Step 2: join continuations
-- --------------------------------------------------------------------

-- | Walk the classified lines left to right, carrying "the reachable
-- depth from the line above" and the line above's accumulated text,
-- so we can decide, for each subsequent line, whether it's a
-- continuation (join onto the accumulator) or a new logical line
-- (emit what we were accumulating, start fresh).
--
-- Reachable depth rule (§5.5, unchanged from the handbook): "the
-- parser finds the highest block depth it can reach from the line
-- above. The parser can reach one level deeper than the line above
-- only when that line ends with a bare DO." We check "ends with a
-- bare DO" by testing whether the accumulated line's text, right-
-- trimmed, ends in "DO" as its own standalone trailing word (not part
-- of a longer word like "REDO" or a call like "DO SUB", the
-- handbook's own join-materialization example puts DO as the very
-- last token on the line with nothing after it, so we require it to
-- be a standalone trailing word).
--
-- Join behavior (REVISED, see module header): default is to insert
-- exactly one space between the accumulator and the new line's text.
-- When the new line carried an explicit @|@ glue marker ('rlGlued'),
-- we join with NO space instead, the old handbook behavior,
-- preserved as an opt-in escape hatch for splitting a token
-- mid-word.
joinContinuations :: [RawLine] -> [LogicalLine]
joinContinuations = go Nothing
  where
    -- 'acc' is the logical line currently being accumulated, if any:
    -- (depth, tag, textSoFar). Nothing at the very start or right
    -- after flushing.
    go :: Maybe (Int, Maybe Text, Text) -> [RawLine] -> [LogicalLine]
    go acc [] = flush acc
    go acc (rl : rest)
      | RawLine tagLikeDepth (Just tagName) _glued txt <- rl =
          -- A tag line always starts a NEW logical line. A tag
          -- itself can never be a continuation target or a
          -- continuation of anything, since continuation is decided
          -- purely from dot-count vs. reachable depth, and tags are
          -- fixed at column 0 / depth 0 by definition, which the
          -- handbook never describes as continuable. (Its own
          -- 'rlGlued' is meaningless and ignored, per the field's own
          -- doc.)
          flush acc ++ go (Just (tagLikeDepth, Just tagName, txt)) rest

      | otherwise =
          case acc of
            Nothing ->
              -- First real line, or right after a flush: this can't
              -- be a continuation of anything (nothing above it), so
              -- it starts a new logical line regardless of its own
              -- dot depth or glue marker (both are meaningless with
              -- no line above to join onto).
              go (Just (rlDepth rl, Nothing, rlText rl)) rest
            Just (accDepth, accTag, accText) ->
              let reachable = if endsWithBareDo accText then accDepth + 1 else accDepth
              in if rlDepth rl > reachable
                   then
                     -- Continuation: join onto the accumulator. Default
                     -- is a single inserted space; an explicit '|' glue
                     -- marker on this line requests the old no-space
                     -- hard join instead (module header, "Continuation
                     -- joining (REVISED)").
                     let joined = if rlGlued rl
                                    then accText <> rlText rl
                                    else accText <> " " <> rlText rl
                     in go (Just (accDepth, accTag, joined)) rest
                   else
                     -- New statement at this depth.
                     flush acc ++ go (Just (rlDepth rl, Nothing, rlText rl)) rest

    flush :: Maybe (Int, Maybe Text, Text) -> [LogicalLine]
    flush Nothing = []
    flush (Just (d, tag, txt)) = [LogicalLine d tag txt]

-- | Does this (already-joined-so-far) logical line's text end with a
-- BARE @DO@ as its own trailing word, i.e. is the very last
-- whitespace-delimited token exactly @"DO"@? We check the word
-- boundary rather than a raw suffix match, since "REDO" (hypothetically)
-- or "DO SOMETHING" must not qualify, only a standalone trailing
-- @DO@ opens one extra level of reachable depth, per the handbook's
-- join-materialization example where @DO@ sits alone as the final
-- line of a multi-MERGE block.
--
-- Because 'T.takeWhileEnd' extracts the maximal trailing run of
-- non-whitespace characters, the word it returns is already correctly
-- bounded on its left (either by whitespace or the start of the
-- string), no separate boundary check is needed once we compare that
-- extracted word to "DO".
endsWithBareDo :: Text -> Bool
endsWithBareDo txt =
  T.takeWhileEnd (\c -> c /= ' ' && c /= '\t') (T.stripEnd txt) == "DO"
    || endsWithDoWith txt

-- | Does this line end with a @DO WITH @TX(...)@ clause, optionally
-- followed by an @@BIND(...)@ list (the time-pinned block opener,
-- docs/tx-pinning-design.md Part 2)? It opens a dotted block exactly
-- like a bare @DO@ does, so it has to raise the reachable depth the
-- same way — otherwise the dotted lines after it would be mistaken for
-- a continuation of this one.
--
-- The single-call form, @DO WITH @TX(...) TAG(args)@, ends in the call
-- instead and does NOT open a block, so it must not match here.
--
-- Finds the LAST @ WITH @TX(@ (ASCII case-insensitively), requires the
-- word before it to be @DO@ (or @DO:postcondition@), requires the
-- parenthesis it opens to close, skipping over string literals so a
-- @)@ inside quotes doesn't count, and requires that what follows that
-- closing parenthesis is either nothing or exactly one @ @BIND(...)@
-- clause whose own parenthesis closes at the end of the line.
endsWithDoWith :: Text -> Bool
endsWithDoWith txt =
  let t = T.stripEnd txt
      delim = " WITH @TX("
      (beforeInclDelim, _) = T.breakOnEnd delim (T.toUpper t)
      n = T.length beforeInclDelim
      beforeDelim = T.take (n - T.length delim) t
      lastWord = T.toUpper (T.takeWhileEnd (\c -> c /= ' ' && c /= '\t') beforeDelim)
      afterDelim = T.drop n t
  in n > 0
       && (lastWord == "DO" || "DO:" `T.isPrefixOf` lastWord)
       && opensBlockTail (T.unpack afterDelim)
  where
    -- Called just after the opening paren of @TX( (depth 1).
    opensBlockTail :: String -> Bool
    opensBlockTail afterOpen = case closeParen afterOpen of
      Nothing   -> False
      Just rest -> null rest || bindTailOnly rest

    -- The rest of the line is exactly one @BIND(...) clause.
    bindTailOnly :: String -> Bool
    bindTailOnly rest =
      let bindOpen = " @BIND("
          (pre, post) = splitAt (length bindOpen) rest
      in map toUpper pre == bindOpen && closeParen post == Just ""

    -- Given the text just after an opening paren (depth 1), the text
    -- left after that paren's matching close, skipping string literals.
    closeParen :: String -> Maybe String
    closeParen = go (1 :: Int) False
    go depth inStr cs = case cs of
      [] -> Nothing
      (c : rest)
        | inStr -> if c == '"'
                     then case rest of
                            ('"' : rest') -> go depth True rest'
                            _             -> go depth False rest
                     else go depth True rest
        | c == '"'  -> go depth True rest
        | c == '('  -> go (depth + 1) False rest
        | c == ')'  -> if depth == 1 then Just rest else go (depth - 1) False rest
        | otherwise -> go depth False rest

isTagIdentifier :: Char -> Text -> Bool
isTagIdentifier c0 word =
  (isLetter c0 || c0 == '%') && T.all (\c -> isAlphaNum c || c == '%') word

isCommandKeyword :: Text -> Bool
isCommandKeyword kw =
  T.toUpper kw `elem`
    [ "SET", "KILL", "WRITE", "IF", "ELSE", "FOR", "DO", "GOTO"
    , "QUIT", "RETURN", "NEW", "MERGE", "LOCK", "UNLOCK", "END", "NAMESPACE"
    ]

-- | Does the current accumulated text need more input before it can
-- be executed?
--
-- A block is dotted (@DO@ / @DO WITH @TX(...)@ followed by lines that
-- start with @.@), so input continues while the last line entered
-- either OPENS a block ('endsWithBareDo', shared with "Phase.Lines" so
-- the REPL and the line builder can never disagree about what opens
-- one) or is itself a dotted block line (depth > 0). A blank line ends
-- the block, as does a non-dotted line (it simply runs as the next
-- statement after the block).
--
-- The old rule counted EVERY @DO@ word as an unclosed opener and
-- nothing ever closed it, so even a plain one-line @DO tag@ made the
-- REPL swallow the following lines as "continuation" until EOF. The
-- explicit @DO ... END DO@ spelling is not tracked here: block
-- structure at run time is dot-depth only (@END DO@ is parsed but a
-- no-op), and @:{@ / @:}@ remains the explicit multi-line fallback.
needsContinuation :: Text -> Bool
needsContinuation src
  | lastPhysicalLineBlank = False
  | otherwise =
      case reverse (buildLogicalLines src) of
        (ll : _) -> endsWithBareDo (llText ll) || llDepth ll > 0
        []       -> False
  where
    lastPhysicalLineBlank = case reverse (T.splitOn "\n" src) of
      (l : _ : _) -> T.null (T.strip l)   -- a blank line AFTER some input
      _           -> False
