# PhaseDB, Haskell rewrite

A ground-up rewrite of PhaseDB (MUMPS-derived language + its own
hierarchical database) in Haskell, using `Parsec`. Spec is
`phase-handbook.md`; the original Java implementation is being treated
as prior art / design record, not as source to port line-by-line,
given the shift to a purely functional paradigm.

## Status

Part I core is largely in place:

- `Phase.AST`, value/ref/operator/expression types, plus `Stmt`,
  `CallTarget`, `ForSpec`, `WriteItem`, `PredStmt`, and `Line`. A
  `Line` holds a SEQUENCE of `PredStmt`s (postcondition + statement
  pairs), not a single statement, see below.
- `Phase.Value`, numeric coercion (leading-numeric-prefix rule),
  canonical numeric form, `=` semantics, collation ordering. All
  transcribed directly from handbook §2 and §3.2, including the
  intentional M quirks (`"  12"` → 0, `"1.0"` not canonical, etc).
- `Phase.Parser`, full expression grammar: the precedence ladder
  (Appendix B), unary prefix operators, slice postfix syntax. Hand-
  traced against every worked example the handbook gives (`1+2*3=7`,
  `2**3**2=512`, unary binding tighter than `**`) since this sandbox
  has no GHC to compile-check against.
- `Phase.Lines`, turns raw source into logical lines: tag
  recognition (column 0), dot-depth block structure (§5.4), and line
  continuation (§5.5), **with one deliberate deviation from the
  handbook**, see below.
- `Phase.Stmt`, the statement/command grammar: `SET`, `MERGE`,
  `KILL`, `WRITE`, `IF`/`ELSE`, `FOR` (classic forms), `DO`/`GOTO`
  with call targets, `QUIT`, `RETURN`, `NEW`, `LOCK`/`UNLOCK`,
  `END DO`, postconditions (§8, correctly positioned, see deviation
  4 below), and a graceful `SUnsupported` fallback (§9) for everything
  not yet implemented, including recognized-but-unimplemented
  *extensions* of an already-supported command's own grammar (`SET`'s
  `EXPECTING`/`CALL` forms, see deviation 3 below). **A logical line
  can hold several statements**, see deviation 2. This completes
  every Part I command in the handbook's own §9 reference table
  except `FOR EACH`.
- `Phase.Tree`, sparse multi-dimensional hierarchical storage with
  **MUMPS node duality** (nodes holding both a bare value and children
  simultaneously) and subscript ordering governed by Phase's
  canonical-numeric collation (`collationCompare`). Provides
  `getNode`, `setNode`, `killNode`, `dataNode` (the 4-state 0/1/10/11
  existence check for `$DATA`), `orderNode` (`$ORDER` traversal), and
  `mergeNode` (subtree copying for `MERGE`).
- `Phase.State`, process-global execution environment `Env`: local
  variables, in-memory globals, call stack frames `Frame`, `NEW`
  variable restoration scoping, `$TEST` tracking, and output buffering.
- `Phase.Eval`, full expression and statement execution engine.
  Evaluates arithmetic (with 34-digit precision and `HALF_UP` rounding),
  comparisons, non-short-circuiting `&`/`!`, slices `s[start:end]`,
  and intrinsics (`$DATA`, `$ORDER`, `$GET`, `$LENGTH`, `$EXTRACT`,
  `$PIECE`, `$TEST`, `$JOB`, `$TX`), plus statement-level control flow
  (`SET`, `KILL`, `WRITE`, `IF`/`ELSE`, `FOR`, `DO`, `GOTO`, `QUIT`,
  `RETURN`, `NEW`, `MERGE`).
- `Phase.Catalog`, system global repository `^_CATALOG` for namespaces,
  routines, functions, and constants with primitive version control.
  Every update to a function or constant automatically archives its
  prior code and metadata into `^_CATALOG(namespace, name, "HISTORY", version)`.
  Supports same-namespace lookup with automatic fallback to `_GLOBAL`
  (Part V), rollback (`revertItem`), and introspection via `$DATA`/`$ORDER`.
- `Phase.Storage`, persistent MVCC storage engine, transactions, WAL,
  snapshot checkpoints, and advisory lock management:
  - `Phase.Storage.Types`: `TxId`, `GlobalPath`, `Revision`, `WalOp`, `WalRecord`, `StorageError`.
  - `Phase.Storage.Engine`: live global tree + historical revision chains powering Part VII time travel (`^[tx]GLOBAL(subs)` and `^[time]GLOBAL(subs)`), `$DATA` existence checks, and `$ORDER` traversals.
  - `Phase.Storage.WAL`: append-only disk writer, serialization, checkpoint snapshots (`.snap`), and crash recovery replay.
  - `Phase.Storage.Transaction`: staged mutations, optimistic concurrency validation (`EXPECTING`), atomic multi-statement commits and conflict detection.
  - `Phase.Storage.Lock`: path-granular, reentrant advisory locking table (`LOCK`/`UNLOCK`).
- **Language Extensions (Parts II & IV)**:
  - Multi-value `CALL` / `RETURN` destructuring: `SET total,tax = CALL TOTAL(orders, 2)`.
  - Expression-position `CALL`: `WRITE CALL DOUBLE(21),!`.
  - Reference parameters: `DO DOUBLEIT(&x)` / `CALL DOUBLEIT(&x)` with true local variable aliasing across call frames.
  - `FOR EACH var IN collection` collection traversal for globals and locals (lowered to `$ORDER`).
  - Dotted block execution for bare `DO` and `FOR` loops.
- `test/Main.hs`, comprehensive unit and specification test suite
  (**225 tests**, 0 failures) validating Value coercion, Parser precedence, Lines layout,
  Stmt commands, Tree duality/collation, Eval expressions/routines,
  Catalog versioning/dispatch, Storage MVCC/WAL/Transactions/Time Travel/Locks,
  and Language Extensions (`FOR EACH`, `CALL`, multi-value destructuring, `&ref`).
- `app/Main.hs`, Part XIV interactive REPL with state persistence,
  CLI database argument (`phasedb-hs [dbfile]`, defaults to `phase.db`),
  statement execution, bare-expression echoing (`- : <value>`), native `NAMESPACE` statement,
  column-0 `TAG(args)` definition registration, and catalog management commands (`.ns`, `.namespaces`, `.fns`, `.history`, `.revert`, `.save`).

Remaining language extensions (`EVALUATE`, `INDEX`, `CONDITION`/`SIGNAL`,
`HANDLE`/`CATCH`/`RETRY`, `BLOBSAVE`/`BLOBMETA`) currently parse successfully as
`SUnsupported`, per the handbook's own §9 graceful-degradation
philosophy.

## Deliberate deviations from the handbook

### 1. Continuation joining now inserts a default space

The handbook's own §5.5 rule ("the parser joins the remaining text
directly, with no space added... to keep exactly one space at the
join point, type that space before the line break") does not actually
work for the handbook's own Part X §2 materializing-join example, a
`FOR EACH` line continued by two `MERGE` lines and a bare `DO`, none
of which carry a trailing space in the handbook source. A literal
no-space join concatenates `^ORDERS` directly onto `MERGE`, which does
not parse.

Agreed fix with Astra: continuation now joins with a single space
inserted **by default**. Dot-count remains the *only* signal for
depth and for triggering continuation at all (unchanged), an
indentation-based alternative was considered and explicitly rejected,
because raw indentation has no reliable discrete unit the way an
explicit dot count does, and using it would make a continuation line
ambiguous with a fresh column-0 tag line.

To get the old no-space "hard join" behavior back (e.g. splitting a
long string literal token with no gap at all), place a literal `|`
immediately after a line's depth-dots, before its statement text:

```
WRITE "This sentence starts on one line and continues
.|right here with no gap at all.",!
```

The `|` is stripped and is not part of the joined text. `\` was
considered for this marker and rejected, it's already the
integer-division operator, and a continuation line could legitimately
need to start with, e.g., `\5` as real Phase code.

### 2. A logical line holds a SEQUENCE of statements, each with its own postcondition

The handbook is explicit about this (§5.1: "Both `WRITE` statements
run only when `X>0`, because both sit on the same line as the `IF`,"
and the example `IF X>0 WRITE "positive",! ELSE  WRITE "not positive",!`
is four statements on one line), this isn't a deviation so much as a
correction to an earlier draft of this port, which modeled `Line` as
holding exactly one `Stmt` and one line-wide postcondition. Neither
holds up: postconditions attach per-statement (§8: "the first `:`
right after **the command word**," singular), so `PredStmt` pairs
each `Stmt` with its own `Maybe Expr` postcondition, and `Line` holds
`[PredStmt]`.

### 3. `SET`'s `EXPECTING`/`CALL` extensions degrade gracefully via a general backout mechanism

An earlier draft of this port had a real, documented gap here: every
command parser commits once its keyword matches (see `pSet`'s own doc
comment for why, this was itself a bugfix, catching malformed `SET X`
with no `=` silently misreading as a bare-expression statement), so a
syntactically real `SET ref EXPECTING expr = v` (Part III §2's
optimistic-concurrency form) or `SET t1,t2=CALL tag(...)` (Part II
§1's multi-value destructuring) used to hard-fail to parse for that
whole line, rather than degrading gracefully the way an unrecognized
*leading word* does.

This is now fixed with a general mechanism, `Phase.Stmt.bailToUnsupported`:
`pLine` snapshots the input exactly as it stands at the start of each
statement attempt (via `getInput`, threaded down as a plain function
argument, deliberately NOT via Parsec user state, which would force
`Phase.Parser.PParser`'s type to change everywhere it's used, including
parsers that have nothing to do with statement-level bailout). Any
command parser can then recognize a known-but-unimplemented shape of
its own grammar via a lookahead check, `pSet`'s own
`pRecognizedSetExtension` peeks for `EXPECTING` or `=CALL` immediately
after its first target, and, if spotted, call `bailToUnsupported` with
that snapshot to produce `SUnsupported` for the *complete* original
statement text, rather than either hard-failing or silently misparsing
a truncated prefix of it. A genuinely malformed `SET` (a real typo)
still hard-fails exactly as before, only a shape the handbook
actually describes, that this port simply hasn't implemented yet,
gets the graceful treatment.

### 4. Fixed: postcondition parsing was structurally impossible before this pass

Worth being upfront about since it's a correctness bug, not a design
choice: an earlier draft of `Phase.Stmt` tried to parse a line's
postcondition once, generically, in `pLine`, *before* looking at which
command followed. This cannot work, the colon in `SET:DONE` comes
*after* "SET", not before it, so checking for a leading colon at the
very start of a statement attempt can never succeed. In practice this
meant every postcondition-bearing statement in the handbook's own
examples (`WRITE:X>0`, `QUIT:I=""`, `SET:DONE`) hard-failed to parse.

Fixed by moving postcondition parsing into `pKeyword`, called from
*within* each command's own parser immediately after its keyword
matches, the only position where the colon can actually appear. Every
command parser's type changed from `PParser Stmt` to
`PParser (Maybe Expr, Stmt)` accordingly; `pLine` just assembles the
resulting `PredStmt` sequence.

### 5. Single space vs. double space disambiguates optional arguments (the team's rule, not in the handbook)

Some commands' arguments are entirely optional and, when present,
start with a bare identifier/expression that could equally well be an
unrelated NEXT statement's command word, `QUIT`, `RETURN`, `NEW`,
`KILL`, bare `DO`. Is `WRITE` in `QUIT WRITE` the value being quit
with, or the start of a fresh `WRITE` statement?

Resolution: a single space commits to reading what follows as
this command's own argument (a collision with a real command keyword
there is now a parse error, should have used two spaces); a double
space (or more) always means a fresh statement follows. This matches
the handbook's own `ELSE  WRITE` example, which already uses two
spaces. See `Phase.Stmt.pOptionalArg` for the implementation, and
`pOptionalArgNoKeywordGuard` for the narrower variant `FOR` needs
(its argument grammar always requires more syntax after the leading
identifier, a mandatory `=`, so it has no bare-standalone-identifier
reading to collide with a following statement in the first place; the
full keyword guard would wrongly reject e.g. `FOR WRITE=1:1:10` using
`WRITE` as an unusual but valid loop-variable name).

The keyword-collision check itself only fires when the colliding
keyword is followed by whitespace or end of input, never by other
punctuation, so that e.g. `DO SET(1)` (calling a tag actually named
`SET`, immediately followed by a parenthesized argument list) parses
correctly: no real Phase command attaches its own argument with zero
space after the command word, so a keyword immediately followed by
`(`, `,`, `=`, etc. can never be mistaken for the start of a new
statement.

### 6. The text after `^` in `TAG^X` is a Catalog namespace, not a routine file

The handbook (§5.3) describes `TAG^ROUTINE` as reaching into a separate
routine loaded through an `interp.routineLoader` hook. This rewrite has
no `RoutineLoader`: routines and functions are both Catalog items
(`KindRoutine` / `KindFunction`), and `Phase.Eval.invokeTarget` passes
the text after `^` straight to `Phase.Catalog.lookupItem` as the
**namespace**. (The AST fields `ctRoutine` and the error
`UndefinedRoutine` keep their Java-era names; they now mean "namespace".)

The practical result is that another namespace's functions can be
called **without switching the active namespace with `NAMESPACE`**:

```
 WRITE CALL SQUARE^MATH(6),!     ; SQUARE in namespace MATH, whatever NAMESPACE is active
 DO SHOUT^MATH("hi")
 GOTO FINISH^MATH
 WRITE CALL GREETING^_GLOBAL(),! ; explicit root namespace
```

Rules:

- **Order is tag first, namespace second**, the same as `DO`/`GOTO`.
  `MATH^SQUARE(6)` would mean tag `MATH` in namespace `SQUARE`. The
  shape `A^B` can only have one reading, so there is no namespace-first
  form.
- The named namespace is searched first, then `_GLOBAL` (Part V
  fallback, unchanged). `TAG^_GLOBAL` looks only in `_GLOBAL`, which is
  what an unqualified `TAG` finds when the active namespace has no such
  tag. It also gives a way to reach `_GLOBAL`'s version when the
  active namespace defines a tag of the same name.
- `CALL` stays mandatory in expression position. `$$TAG^NS(...)` is
  still rejected on purpose (handbook: `$$` is out of scope).
- `_GLOBAL` can now be written after `^`; `pNamespaceIdentifier`
  previously rejected a leading underscore.
- Bare `GOTO ^NAMESPACE` / `DO ^NAMESPACE` (line-0 entry) is still
  unsupported and reports `<UNDEFINED ROUTINE>`.
- **`GOTO TAG` (same routine)** is a real program-counter move within
  the running line list, so it falls through later tags as in M.
  **`GOTO TAG^OTHER`** cannot: the Catalog stores each tag's body as a
  separate chunk, so it runs that one chunk and then ends the current
  routine (a GOTO never returns).

## Known gaps / things to verify against real Phase behavior

These are called out in code comments too, but collected here since
they're the kind of thing worth checking against the actual Java
parser or a running Phase instance before trusting this port blindly:

1. **String literal escaping** (`Phase.Parser.pStringLit`). I assumed
   the classic M convention of doubling a quote (`""`) to embed a
   literal `"` inside a string, because the handbook's own examples
   never show an escaped quote at all. If Phase actually uses
   backslash-escaping or disallows embedded quotes entirely, this
   needs correcting.
2. **Slice-bracket vs. contains-bracket disambiguation**
   (`Phase.Parser.pSliceSuffix`). Slice syntax `s[start:end]` and the
   Level-2 `[` (contains) operator use the identical character. This
   parser disambiguates purely by *position*, a `[...]` immediately
   following a postfix atom is always read as a slice, never as a
   contains-comparison continuing that atom. I have not cross-checked
   this against `Parser.java`'s actual disambiguation rule (if the two
   ever really can appear in a position where either reading would be
   grammatically valid).
3. **Identifier shape** (`Phase.Parser.pIdentifier`,
   `Phase.Stmt.pIdentifierText`). Built from the handbook's own
   example names (`X`, `SUM`, `%TEMP`, `ORDERS`), letter-or-`%`
   start, then alphanumeric-or-`%`. The handbook doesn't give a formal
   grammar for identifiers, so this is inferred, not transcribed.
4. **No-whitespace rule scope inside expressions.** Prior work on the
   Java version found Phase's parser tolerates no whitespace around
   commas or binary operators in expressions, subscript lists,
   argument lists, or target lists. `Phase.Parser` enforces this by
   construction. `Phase.Stmt` layers a *different*, more permissive
   whitespace model on top for statement-level syntax, see its module
   header for the exact rules (which whitespace is allowed where, and
   why single- vs. double-space matters for a handful of commands).
5. **Tag lines in handbook code fragments.** Several handbook examples
   (e.g. the Part X §2 join, `phase-handbook.md` lines 1288–1291) show
   a `FOR EACH...` line starting at the left margin with no visible
   indentation. Taken 100% literally, `Phase.Lines.classify` would
   read that as a *tag* line (tags require column 0), which is almost
   certainly not the intent, these are code fragments reformatted
   for the handbook's own prose, not literal column-accurate routine
   source. `Phase.Lines` assumes real routine source always indents
   ordinary statements by at least one space/tab, per the Part XIV
   REPL note ("Anything else runs as an ordinary statement" when not
   at column 0), flagged here so this assumption isn't silently
   buried.
6. **`DO tag(&ref, ...)` reference-parameter args (Part II §2)** are
   not modeled by `Phase.Stmt.pCallTarget` yet. A literal `&` inside a
   call's argument list will fail `Phase.Parser.pExpr`, which, since
   `pOptionalArg`'s ambiguity check has already committed by that
   point, surfaces as a genuine parse error rather than a graceful
   fallback to `SUnsupported`. When Part II reference-argument support
   is built, it should extend `pCallTarget` directly.
7. **`LOCK`/`UNLOCK` bare (unsigned) item semantics** (`Phase.AST.LockSpec`,
   `Phase.Stmt.pLockSpec`). The handbook's §9 grammar (`ref,+ref,-ref[:timeout]`)
   doesn't say what a bare, unsigned `ref` means inside a `LOCK` list
   versus an `UNLOCK` list, `LockSpec`'s `lsSign` field is left as
   `Nothing` for that case, with resolution explicitly deferred to
   evaluation (which statement it's in decides the effective sign).
   Worth confirming against `LockTable`'s actual behavior once that
   part of the port is reached.
