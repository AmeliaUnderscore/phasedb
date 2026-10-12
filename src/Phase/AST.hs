-- | Core AST types for Phase: expressions, operators, and statement
-- skeletons. This module has no parsing or evaluation logic, it only
-- defines the shapes that 'Phase.Parser' produces and 'Phase.Eval'
-- consumes.
--
-- Design note (handbook Part I §1, §4): Phase is a MUMPS derivative
-- with exactly one runtime value type (the string), but a *real*
-- precedence ladder unlike M76's strict left-to-right rule. We still
-- give the AST proper nested nodes (BinOp/UnOp) rather than a flat
-- list, because the whole point of the precedence-climbing parser is
-- to produce correctly nested structure once, at parse time, so the
-- evaluator never has to think about precedence at all.
module Phase.AST
  ( -- * Values
    PValue (..)
  , mkValue

    -- * References (locals / globals, subscripted)
  , VarKind (..)
  , Ref (..)
  , mkRef
  , mkNSRef
  , mkTimeTravelRef
  , mkNSTimeTravelRef

    -- * Operators
  , BinOp (..)
  , UnOp (..)

    -- * Expressions
  , Expr (..)
  , PinSpec (..)
  , BindName (..)

    -- * Statements (skeleton, extended as later parts are implemented)
  , Stmt (..)
  , SetTarget (..)
  , CallArg (..)
  , CallTarget (..)
  , WriteItem (..)
  , LockSpec (..)
  , ForSpec (..)
  , PredStmt (..)
  , Line (..)
  ) where

import Data.Text (Text)

-- --------------------------------------------------------------------
-- Values
-- --------------------------------------------------------------------

-- | A Phase runtime value. Per handbook Part I §2, Phase has exactly
-- one data type: the string. We newtype it (rather than using a bare
-- 'Text') so that every place in the codebase that wants "the number
-- this string coerces to" is forced to go through 'Phase.Value.toNumber'
-- instead of quietly doing its own ad hoc parsing. This is the same
-- discipline the handbook attributes to 'MValue.toBigDecimal' in the
-- original Java: there is exactly one coercion rule, and everything
-- funnels through it.
newtype PValue = PValue { unPValue :: Text }
  deriving (Eq, Show)

mkValue :: Text -> PValue
mkValue = PValue

-- --------------------------------------------------------------------
-- References: locals and globals, with subscripts (handbook §3)
-- --------------------------------------------------------------------

-- | Which of Phase's two kinds of variable this is. The '^' sigil
-- (parsed away by the lexer) is what distinguishes them; by the time
-- we reach the AST the distinction is a plain sum type, not a stray
-- character to keep checking for.
data VarKind
  = Local
  | Global
  deriving (Eq, Show)

-- | A reference to a local or global variable. Two forms:
--
-- * 'Ref', a statically-named reference, with an optional namespace
--   qualifier, zero or more subscripts, and an optional time-travel
--   bracket. This is the ordinary form produced by 'Phase.Parser.pRef'.
--
-- * 'RefDynamic', a @REF(name, sub1, sub2, ...)@ dynamic reference
--   (Part VI §1). The first expression evaluates to the variable name
--   at runtime; a leading @^@ in that string means global, no @^@
--   means local. The remaining expressions are subscripts, evaluated
--   in order. 'RefDynamic' works in every position a 'Ref' does:
--   read, SET target, KILL target.
data Ref
  = Ref
    { refKind       :: !VarKind
    , refNamespace  :: !(Maybe Text) -- ^ Optional namespace qualifier (e.g. "Billing" in Billing^ORDERS(1))
    , refName       :: !Text
    , refSubscripts :: ![Expr]       -- ^ empty = bare reference, e.g. plain X or ^X
    , refTimeTravel :: !(Maybe Expr) -- ^ Optional time-travel expression (Part VII)
    }
  | RefDynamic ![Expr]
    -- ^ @REF(nameExpr, subExpr1, subExpr2, ...)@ (Part VI §1). The first
    -- expression is the variable name; the rest are subscripts. Evaluated
    -- at runtime by 'Phase.Eval.resolveRefDynamic'. Requires at least one
    -- argument (the name); zero arguments is a @<SYNTAX>@ error at eval
    -- time, not parse time, consistent with how other shape errors are
    -- handled in this codebase (recognised at parse time, rejected cleanly
    -- at eval time rather than with a raw Parsec error).
  | RefNaked ![Expr]
    -- ^ A naked reference, @^(subExpr1, subExpr2, ...)@, no name between
    -- the global sigil and the parenthesis. Always resolves to a GLOBAL
    -- (there is no such thing as a naked local): the same global as the
    -- last one NAMED explicitly anywhere in this session, keeping that
    -- name but replacing its entire subscript list with the ones given
    -- here. See 'Phase.Eval.resolveNakedRef'. At least one subscript is
    -- required syntactically (@^()@ with nothing inside is not a naked
    -- reference, there is no bare-naked form in real M either, since a
    -- naked reference exists only to abbreviate a DIFFERENT subscript
    -- list on the SAME global).
  deriving (Eq, Show)

-- | Construct a standard local or global reference.
mkRef :: VarKind -> Text -> [Expr] -> Ref
mkRef kind name subs = Ref kind Nothing name subs Nothing

-- | Construct a namespace-qualified global reference (e.g. Billing^ORDERS(1)).
mkNSRef :: Text -> Text -> [Expr] -> Ref
mkNSRef ns name subs = Ref Global (Just ns) name subs Nothing

-- | Construct a time-traveled global reference (Part VII).
mkTimeTravelRef :: Text -> Expr -> [Expr] -> Ref
mkTimeTravelRef name ttExpr subs = Ref Global Nothing name subs (Just ttExpr)

-- | Construct a namespace-qualified, time-traveled global reference.
mkNSTimeTravelRef :: Text -> Text -> Expr -> [Expr] -> Ref
mkNSTimeTravelRef ns name ttExpr subs = Ref Global (Just ns) name subs (Just ttExpr)

-- --------------------------------------------------------------------
-- Operators (handbook Part I §4, Appendix B)
-- --------------------------------------------------------------------

-- | Binary operators, ordered here by precedence level (loosest to
-- tightest) purely for readability of this file, the actual
-- precedence used by the parser lives in 'Phase.Parser', not in the
-- order of these constructors.
--
-- Level 1: And, Or
-- Level 2: Eq, Lt, Gt, Contains, SortsAfter, Match, and their negated
--          forms (the handbook's `'`-negatable comparison operators,
--          e.g. `'=`, `'[`)
-- Level 3: Concat
-- Level 4: Add, Sub
-- Level 5: Mul, Div, IntDiv, Mod
-- Level 6: Pow (right-associative, the one exception to left-assoc)
data BinOp
  = And           -- ^ &   (non-short-circuiting, handbook §4)
  | Or            -- ^ !   (non-short-circuiting)
  | Eq            -- ^ =   (canonical-numeric-form equality, §2)
  | NotEq         -- ^ '=
  | Lt            -- ^ <
  | NotLt         -- ^ '<
  | Gt            -- ^ >
  | NotGt         -- ^ '>
  | Contains      -- ^ [
  | NotContains   -- ^ '[
  | SortsAfter    -- ^ ]]  (collation order per §3.2, not ASCII)
  | NotSortsAfter -- ^ ']]
  | Match         -- ^ ?   (parses; throws <NOTIMPL> at eval time, Part XV)
  | NotMatch      -- ^ '?
  | Concat        -- ^ _
  | Add           -- ^ +
  | Sub           -- ^ -
  | Mul           -- ^ *
  | Div           -- ^ /   (throws <DIVIDE BY ZERO> / signals <DIVIDE-BY-ZERO>)
  | IntDiv        -- ^ \
  | Mod           -- ^ #
  | Pow           -- ^ **  (right-associative)
  deriving (Eq, Show)

-- | Unary prefix operators (tightest binding, handbook Appendix B).
-- '@' (indirection) is included in the grammar per Part XV: the
-- tokenizer/parser must accept it so surrounding syntax doesn't choke,
-- but 'Phase.Eval' throws <NOTIMPL> on it. We keep that split, accept
-- at parse time, reject at eval time, faithfully in this port.
data UnOp
  = Neg      -- ^ unary -
  | Pos      -- ^ unary +
  | Not      -- ^ '     (logical NOT)
  | Indirect -- ^ @     (Part XV: parses, evaluates to <NOTIMPL>)
  deriving (Eq, Show)

-- --------------------------------------------------------------------
-- Expressions
-- --------------------------------------------------------------------

-- | Every 'Expr' node holds only literal data or child 'Expr' nodes,
-- deliberately mirroring the handbook's note (§4) that Phase's
-- expression grammar has no side effects to skip, which is *why*
-- '&'/'!' not short-circuiting is observably a no-op today. If a
-- future Phase feature gives expressions side effects (e.g. some form
-- of inline call), that invariant is exactly what would need
-- revisiting alongside the short-circuit question.
data Expr
  = Lit PValue
    -- ^ A string or numeric literal, already unescaped by the lexer.
  | Var Ref
    -- ^ A bare or subscripted local/global read.
  | BinExpr BinOp Expr Expr
  | UnExpr UnOp Expr
  | Slice Expr (Maybe Expr) (Maybe Expr)
    -- ^ @s[start:end]@ (Part IV). Binds tighter than every operator
    -- above; a postfix wrap directly on the value that precedes it.
    -- Both bounds are optional per Phase's slice syntax.
  | FuncCall Text [Expr]
    -- ^ An intrinsic function call, @$NAME(args...)@ (Part XIII). We
    -- keep this untyped at the AST level, 'Phase.Eval' is
    -- responsible for recognizing known intrinsics vs. throwing
    -- <UNDEFINED FUNCTION>.
  | Call !CallTarget
    -- ^ An expression-position function invocation, @CALL tag(args...)@ (Part II §1).
    -- Yields the first returned value (or empty string if none).
  | CallWith !PinSpec !CallTarget
    -- ^ @CALL WITH @TX(x) [@BIND(...)] tag(args...)@: an expression-position
    -- 'Call' made inside its own pin scope (docs/tx-pinning-design.md,
    -- "Explicit global bindings"). The pin scope lasts for the call only;
    -- it is how a pinned block hands one call a different pin, or
    -- @@TX(LAST)@ to release its pinned globals for that call.
  | TimeTravelLast
    -- ^ The @LAST@ keyword, valid ONLY inside a time-travel bracket
    -- (@^[...]@'s content, a 'Ref' or 'RefDynamic' time-travel
    -- 'Expr' field), see 'Phase.Eval.resolveTimeTravelExpr'. Means
    -- "this path's own most recent revision" as a reference point for
    -- relative addressing: not necessarily the SAME transaction as
    -- the database's overall latest write, since this path may not be
    -- the most recently touched thing. @LAST-N@ (N revisions back from
    -- it) parses as ordinary arithmetic, @BinExpr Sub TimeTravelLast
    -- N@, 'TimeTravelLast' is a reference point, not a keyword with
    -- its own grammar for subtraction. Not independently evaluable by
    -- 'Phase.Eval.evalExpr' as an ordinary value; it only means
    -- something in this one syntactic position (relative revision
    -- addressing, docs/tx-pinning-design.md Part 1).
  deriving (Eq, Show)

-- | One global named in an @@BIND(...)@ list. Whole globals only (never
-- a subscripted path): the namespace qualifier mirrors 'Ref', and
-- 'Phase.Eval' folds the two into the qualified name every global read
-- is keyed by.
data BindName = BindName
  { bnNamespace :: !(Maybe Text)
  , bnName      :: !Text
  } deriving (Eq, Show)

-- | The pin clause shared by @DO WITH@ and @CALL WITH@:
-- @WITH @TX(pin) [@BIND(^A,^B)]@.
--
-- * 'psPin' is the @@TX(...)@ argument, resolved at run time by
--   'Phase.Eval.resolvePinExpr' ('TimeTravelLast' = release the pin).
-- * 'psBind' is the optional bind list. 'Nothing' (no @@BIND@ clause)
--   means "every global the enclosing scope has bound", every global
--   at all when nothing encloses it. @Just []@ is a real, empty list.
data PinSpec = PinSpec
  { psPin  :: !Expr
  , psBind :: !(Maybe [BindName])
  } deriving (Eq, Show)

-- --------------------------------------------------------------------
-- Statements, skeleton only
-- --------------------------------------------------------------------

-- | A SET target: either an ordinary reference, or one of the two
-- pseudo-lvalue forms real M allows for in-place string surgery
-- (Part XIII: @$PIECE@/@$EXTRACT@ as a SET target, not just an
-- expression). Both carry the underlying 'Ref' whose CURRENT value
-- gets read, spliced, and written back, 'Phase.Eval.evalStmt' does
-- the read-splice-write itself; the parser (which fills in each
-- intrinsic's own default arguments, @which@, @start@, @end@, so
-- this type never has to represent "omitted") is the only place that
-- decides which of the three a piece of source text means. Only
-- 'SSet' uses this: 'SCallAssign'/'SMerge'/'SKill' keep plain 'Ref'
-- targets, since none of them has an analogous "splice into what's
-- already there" operation in real M.
data SetTarget
  = TargetRef !Ref
    -- ^ An ordinary @SET ref=val@ target.
  | TargetPiece !Ref !Expr !Expr
    -- ^ @SET $PIECE(ref,delim,which)=val@. @which@ is always present
    -- here (parser fills in the default of 1 when omitted from source).
  | TargetExtract !Ref !Expr !Expr
    -- ^ @SET $EXTRACT(ref,start,end)=val@. @start@/@end@ are always
    -- present here (parser fills in the defaults, 1 and @start@, when
    -- omitted from source).
  deriving (Eq, Show)

-- | Statement skeleton. Deliberately minimal for now: we are building
-- Part I (expressions/values/control-flow shape) first, and Parts
-- II–XIII each add commands here as they're tackled. Per handbook §5,
-- 'Stmt' nodes for 'If'/'For' hold NO body, Phase's control flow
-- governs "the rest of the current line," not a nested block. That is
-- why 'Line' (below), not 'Stmt', is the unit that owns a body.
data Stmt
  = SSet [(SetTarget, Expr)]                 -- ^ SET t=v, t2=v2, ...
  | SCallAssign ![Ref] !CallTarget           -- ^ SET t1, t2 = CALL target(args) (Part II §1)
  | SWrite [Either Expr WriteItem]           -- ^ WRITE expr,!,?N,...
  | SIf [Expr]
    -- ^ @IF cond1,cond2,...@ (comma = AND, short-circuits on the
    -- first false one). Sets @$TEST@ (§5.1). On a false @IF@,
    -- execution skips forward only as far as the next 'SElse' ON THE
    -- SAME LINE, i.e. within this 'Line''s 'lineStmts', not to the
    -- end of the routine. That skip is an EVALUATION concern
    -- ('Phase.Eval' walking 'lineStmts'), not something this AST
    -- shape needs to encode structurally.
  | SElse
    -- ^ Bare @ELSE@, no condition of its own. Checks @$TEST@ from the
    -- most recent 'SIf' ON THE SAME LINE (again an evaluation-time
    -- lookup, not an AST-level link, nothing here points back at
    -- "the" preceding 'SIf', since 'Phase.Eval' finds it by walking
    -- 'lineStmts' backward from this 'SElse' at run time).
  | SFor (Maybe ForSpec)
  | SForEach !Text !Ref                      -- ^ FOR EACH var IN coll (Part IV §4)
  | SDo (Maybe CallTarget)
    -- ^ Bare @DO@ (no target) starts a dotted-block or explicit
    -- @DO...END DO@ block, resolved entirely by 'Phase.Lines'
    -- dot-depth before this statement is ever reached, 'SDo Nothing'
    -- carries no body of its own, per the handbook's insistence that
    -- 'Stmt.If'/'Stmt.For'/(here, bare 'Stmt.Do') hold no body.
    -- @DO TAG(args)@ etc. is 'SDo (Just target)'.
  | SDoWith !PinSpec !(Maybe CallTarget)
     -- ^ @DO WITH @TX(expr) [@BIND(...)]@, with no target, a dotted block
     -- exactly like 'SDo' 'Nothing' (same block resolution; the opener
     -- line just ends in this clause instead of a bare @DO@, see
     -- 'Phase.Lines.endsWithBareDo'). With a target,
     -- @DO WITH @TX(expr) [@BIND(...)] TAG(args)@, one 'SDo' call made
     -- inside the same pin scope. Either way, the GLOBALS the scope has
     -- bound are answered as of one pinned transaction instead of live
     -- (docs/tx-pinning-design.md Part 2); the rest read live. The
     -- 'PinSpec' is resolved at run time by 'Phase.Eval.withPinScope':
     -- 'TimeTravelLast' (@LAST@) releases the bound globals back to live
     -- data; a global reference such as @^[-2]FOO("BAR")@ pins to the
     -- transaction of THAT revision; any other expression is an
     -- absolute transaction number.
  | SGoto CallTarget
  | SQuit (Maybe Expr)
  | SReturn [Expr]                           -- ^ bare RETURN = SReturn []; Part II §1
  | SNew (Maybe [Text])                      -- ^ Nothing = bare NEW (news every current local)
  | SKill (Maybe [Ref])                      -- ^ Nothing = bare KILL (kills all locals)
  | SMerge [(Ref, Ref)]                      -- ^ MERGE t=s, t2=s2, ... (§9, Part X §1)
  | SLock (Maybe [LockSpec])                 -- ^ Nothing = bare LOCK (release/acquire everything)
  | SUnlock (Maybe [LockSpec])               -- ^ Nothing = bare UNLOCK
  | SEndDo
    -- ^ Closes an explicit @DO...END DO@ block (§5.4: "Use this form
    -- for HANDLE/CATCH/RETRY blocks... and for TRANSACTION blocks").
    -- We recognize the keyword now, at the Part I layer, purely so a
    -- 'SUnsupported' capture of the not-yet-implemented
    -- @HANDLE@/@CATCH@ body (Part III) terminates cleanly at its own
    -- @END DO@ rather than 'pUnsupportedOrExpr' swallowing everything
    -- all the way to end of input. 'SEndDo' carries no reference back
    -- to the 'SDo' it closes, matching block structure is an
    -- evaluation-time concern (walking the flat line list and
    -- tracking a stack of open blocks), not something this AST shape
    -- needs to encode.
  | SNamespace !Text                         -- ^ NAMESPACE <name> (Part V namespace directive)
  | SExpr Expr                               -- ^ bare expression statement (Part XIV REPL toplevel)
  | SUnsupported Text Text
    -- ^ Anything recognized as "this is clearly an attempted
    -- statement" but not yet implemented by this port, MACLIB,
    -- EVALUATE/WHEN, TRANSACTION, NAMESPACE, EDGE, INDEX,
    -- CONDITION/SIGNAL/HANDLE/CATCH/RETRY, EXPECTING and
    -- reference-parameter forms of SET/DO, and BLOBSAVE/BLOBMETA all
    -- land here for now. Per the handbook's own §9: this is
    -- deliberately a PARSE-time success carrying an execution-time
    -- stub, not a parse error, "a script never fails to parse merely
    -- for containing something Phase has not implemented yet."
    --
    -- Two fields: the first is the offending WORD/TOKEN alone (e.g.
    -- @"TRANSACTION"@, or @"EXPECTING"@ for a recognized-but-
    -- unimplemented extension of an otherwise-supported command),
    -- this is what a @<NOTIMPL>@ error should actually point at,
    -- rather than dumping the whole statement and making the reader
    -- hunt for what specifically wasn't handled. The second is the
    -- full original statement text, kept for context in case a
    -- diagnostic wants to show the whole line too.
  deriving (Eq, Show)

-- | An argument to a function or routine invocation ('DO' / 'CALL').
-- Can be passed by value (an ordinary 'Expr') or by reference ('&name',
-- Part II §2).
--
-- 'CallArgRef' holds a full 'Ref', not just a bare 'Text' name, even
-- though the handbook is explicit that only a bare, unsubscripted
-- LOCAL is ever valid here: "the operand of `&` must be a bare local,
-- never a subscripted local and never a global. Aliasing a global, or
-- an arbitrary expression, makes no sense under this mechanism, so
-- Phase rejects it with `<SYNTAX>`." That rejection is deliberately an
-- EVALUATION-time check, not a parse-time one, @&^ORDERS@ and
-- @&x(1)@ are syntactically well-formed reference arguments (the
-- parser must accept the general shape, or it chokes with a raw,
-- unhelpful Parsec error the moment a global sigil follows @&@,
-- exactly the bug this was fixed to avoid), and Phase's own
-- `<SYNTAX>` error is how a RECOGNIZED-BUT-INVALID reference argument
-- gets reported, matching every other place in this codebase that
-- distinguishes "not recognized at all" from "recognized shape,
-- rejected for a semantic reason." See 'Phase.Eval' for where that
-- check actually happens (immediately before aliasing).
data CallArg
  = CallArgVal !Expr
  | CallArgRef !Ref
  deriving (Eq, Show)

-- | A `DO`/`GOTO`/`CALL` call target (handbook §5.3, §9, Part II): a bare tag, a
-- tag qualified by routine, or a bare routine (entry at line 0),
-- optionally with a parenthesized, comma-separated argument list.
-- Per the no-whitespace rule, no space is tolerated around the `^`
-- or inside the argument list's commas, enforced by whichever
-- parser combinator produces this, not by this type itself.
data CallTarget = CallTarget
  { ctTag     :: !(Maybe Text)      -- ^ Nothing for a bare @^ROUTINE@ target (entry at line 0)
  , ctRoutine :: !(Maybe Text)      -- ^ Nothing for a same-routine bare @TAG@ target
  , ctArgs    :: !(Maybe [CallArg]) -- ^ Nothing = no parens at all; Just [] = empty parens @()@
  } deriving (Eq, Show)

-- | WRITE's special non-expression items: @!@ newline, @#@ form feed,
-- @?N@ tab-to-column (handbook §9 commands table).
data WriteItem
  = WNewline
  | WFormFeed
  | WTabTo Int
  deriving (Eq, Show)

-- | One item in a @LOCK@/@UNLOCK@ list (§9, Part XII §3):
-- @ref@, @+ref@ (acquire), @-ref@ (release), each optionally
-- carrying its own @:timeout@ in milliseconds. A bare @ref@ with no
-- leading sign behaves the same as a caller-context default (which
-- sign that resolves to is an evaluation-time concern, 'Phase.Eval'
-- decides that from whether this is inside a @LOCK@ or an @UNLOCK@
-- statement, not something this type needs to disambiguate itself).
data LockSpec = LockSpec
  { lsSign    :: Maybe Bool  -- ^ Just True = '+' (acquire), Just False = '-' (release), Nothing = no sign given
  , lsRef     :: Ref
  , lsTimeout :: Maybe Expr  -- ^ milliseconds, from an optional trailing @:timeout@
  } deriving (Eq, Show)

-- | The three classic FOR shapes, plus bare FOR. FOR EACH is deferred
-- to the Part IV pass (it needs the collection/traversal machinery),
-- so it is intentionally absent here.
data ForSpec = ForSpec
  { forVar   :: Text
  , forStart :: Expr
  , forInc   :: Maybe Expr
  , forStop  :: Maybe Expr
  } deriving (Eq, Show)

-- | One logical source line: a dot-depth (from the leading-dot block
-- convention, §5.4/§5.5, already resolved by the line-joining pass
-- before this reaches the parser), an optional tag label, and a
-- SEQUENCE of statements.
--
-- Per §5.1, a Phase line is NOT one statement: "Both `WRITE`
-- statements run only when `X>0`, because both sit on the same line
-- as the `IF`," and the canonical example
-- @IF X>0 WRITE "positive",! ELSE  WRITE "not positive",!@ is FOUR
-- statements, @IF@, @WRITE@, @ELSE@, @WRITE@, separated by plain
-- whitespace on one line, not four separate lines. `GOTO`/`DO TAG`
-- resolving to "a line index, not a nested scope" (§5) only makes
-- sense against a flat per-STATEMENT program counter if a line can
-- hold more than one statement, which the interpreter must be able to
-- resume partway through.
data Line = Line
  { lineDepth :: Int
  , lineTag   :: Maybe Text
  , lineStmts :: [PredStmt]
  } deriving (Eq, Show)

-- | One statement together with its own postcondition (§8), if any.
-- Postconditions attach to a SPECIFIC statement, not to the line as a
-- whole, "a command's own postcondition, when present, is always
-- the first `:` right after **the command word**" (singular,
-- per-command). Since a line can hold several statements, each needs
-- its own independent slot: in
-- @WRITE:X>0 "positive",! WRITE:X<0 "negative",!@, the two
-- postconditions are unrelated to each other and each gates only its
-- own `WRITE`.
data PredStmt = PredStmt
  { psPostCond :: Maybe Expr
  , psStmt     :: Stmt
  } deriving (Eq, Show)
