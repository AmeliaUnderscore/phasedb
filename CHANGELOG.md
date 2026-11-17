# Revision history for phasedb-hs

## Unreleased

* Feature: `CONDITION`, `SIGNAL`, `HANDLE`, `CATCH` and `RETRY` (Part III
  §1) on `DO ... END DO` and dotted `DO` blocks. A caught signal rolls the
  state back to the start of the failing top-level line of the block;
  earlier lines' effects stay. `<DIVIDE-BY-ZERO>`, `<UNDEFINED>` and
  `<LOCK-TIMEOUT>` are signalled only while a handler is present.
  `<CONFLICT>` waits for `TRANSACTION`; `<INDEX-BUILD-FAILED>` is not
  raised (secondary indexing is deprecated).
* Extension: `RETRY n TIMES` / `RETRY 1 TIME` (optionally with `WITH`), a
  bounded retry that falls through to the rest of the handler once used
  up. Every `RETRY` is capped by `^%CONFIG("retry","max")` (default 1000).
* HANDLE, CATCH, CONDITION, SIGNAL and RETRY are now recognized command
  words at column 0, and `SIGNAL` and `RETRY` joined the keywords that
  stop a bare `QUIT`/`DO` argument.
* Feature: `EVALUATE ... WHEN ... END EVALUATE` (Part II §5): switch mode,
  condition-chain mode (`EVALUATE TRUE`), multi-subject `ALSO`, `ANY`,
  bare-operator tests (`WHEN >500`, `WHEN '=x`), `WHERE` guards and
  `WHEN OTHER`. It runs in place, so it is not a scope: `QUIT`, `RETURN`
  and `GOTO` in a clause leave the enclosing routine. The path-shape
  clause stays deferred.
* Feature: explicit `X ... END X` blocks are resolved in `Phase.Lines`
  into the same depth numbers dotted blocks use (`DO ... END DO`,
  `EVALUATE ... END EVALUATE`), so no nested-block AST was needed. A
  block opener is dotted when the next line is deeper, otherwise an
  `END`-style candidate matched by a stack; a candidate with no `END` is
  left as the old empty bare `DO`. Dotted `EVALUATE` works too, and so
  will any later block kind (`TRANSACTION`). `END DO` is no longer
  counted as a block opener, and the REPL keeps prompting until an open
  `END`-style block is closed.
* `EVALUATE` and `WHEN` are now recognized command words at column 0
  (previously classified as tags).
* Refactor: the comparison-operator tokens are one parser
  (`Phase.Parser.pCompareBinOp`), shared by infix comparison and by
  `WHEN` operator tests.
* Fix: `GOTO` was a silent no-op (`evalStmt (SGoto _)` returned
  `NextStmt`). Same-routine `GOTO TAG` now jumps via `JumpTo`;
  `GOTO TAG^NAMESPACE` transfers into that Catalog item and ends the
  current routine. Unknown targets raise `<UNDEFINED LABEL>`.
* Fix: `\` (integer division) and `#` (modulo) went through `Double`,
  losing precision beyond ~15 digits. They now use exact `Scientific`
  arithmetic (`exactIntQuotient`), truncating toward zero as before.
* Fix: `pNamespaceIdentifier` accepts a leading `_`, so `%GLOBAL` can be
  named after `^` (`CALL FOOBAR^%GLOBAL(X)`).
* Fix: `aliasVar` mapped a reference parameter to itself when its name
  matched the caller's variable (`DO F(&x)` with formal `x`), sending
  `resolveLocalName` into an infinite loop on the first read.
* Fix (storage): a torn trailing WAL record is now truncated off the
  file on open. Before, later commits were appended after the partial
  bytes and silently lost on the next reopen.
* Fix (storage): records after the last `WalCommit`/`WalCheckpoint` (a
  transaction whose commit never reached disk) are discarded and
  truncated on open instead of being replayed as committed.
* Fix (storage): `flushPendingWal` appends all pending records in one
  write and fsyncs before returning (was one open/append per record and
  no fsync). `takeCheckpoint` flushes pending records first.
* Fix (storage): the WAL header version byte is now checked; an
  unsupported version, a foreign file, or mid-file corruption makes
  `openStorageEngine` return `StorageCorruptWal`, and the REPL exits
  without touching the file instead of "starting fresh" on top of it.
  Invalid UTF-8 in a record is a decode failure, not a pure exception.
  Appending no longer reads the whole file to see if it is empty.
* Build: new module `Phase.Storage.Sync` does the fsync
  (`fileSynchronise` from `unix` on POSIX, `FlushFileBuffers` from
  `Win32` on Windows); the cabal file picks the package with
  `if os(windows)`. The Windows branch has never been compiled.
* Tests: scratch files now live in `getTemporaryDirectory` instead of a
  hard-coded `/tmp`, so the suite can run on Windows.
* Fix: `SET`/`MERGE` reject a null-string (`""`) subscript anywhere in
  the path, for locals and globals alike (`<SUBSCRIPT>`,
  `Phase.State.NullSubscript`). Real M reserves `""` as `$ORDER`'s
  exhaustion sentinel, so it was never a legal subscript to begin with.
* Feature/fix: `^%CATALOG` definitions authored by an admin are now
  protected from being overwritten by a non-admin, while non-admins
  keep the ability to define their own routines (the only way they can
  define one at all). Tracked per item as `^%CATALOG(ns,name,
  "AUTHOR_ROLE")`, enforced both in `Phase.Catalog.defineItem` (so
  `revertItem` inherits it) and for a raw `SET`/`KILL`/`MERGE` that
  reaches `^%CATALOG` directly (`Phase.Auth.checkCatalogAccess`, wired
  into `Phase.State.writeRef`/`killRef`/`mergeRef`) -- the latter was a
  real bypass of the former before this change.
* Fix: the REPL now bootstraps a first admin account whenever the
  database has no users at all (`hasAnyUser`), not only when the `.db`
  file didn't already exist, so an existing empty or user-less file
  doesn't get stuck at the login prompt forever.
* Fix: an admin can no longer `KILL` or demote the database's sole
  remaining admin account (`Phase.Auth.checkUsersKillSafety`/
  `checkUsersSetSafety`), so a database can't be locked out from under
  its own admin.
* Tests: new `Phase.State`/`Phase.Auth`/`Phase.Catalog` group covering
  null-subscript rejection, `^%CATALOG` ownership (new items, editing
  your own, admin takeover, raw-write bypass, broad-kill refusal), and
  the last-admin safeguard.
* Feature ( not in the handbook): naked references,
  `^(subs)` -- reuses the last NAMED global referenced anywhere in the
  session, replacing its ENTIRE subscript list. New `Ref` constructor
  `RefNaked`; new `Env` field `envLastGlobalRef`; `<NAKED>` if nothing
  has been named yet. Every `Ref` consumer (`SET`/`KILL`/`MERGE`/
  `CALL`/`LOCK`/`UNLOCK`/reads/`$DATA`/`$ORDER`/`$GET`) handles it via
  the existing `resolveWriteTarget`/`resolveIntrinsicRef` chokepoints.
* Fix found while adding the above: `LOCK`/`UNLOCK` bare-field-accessed
  their target's `Ref` (`refKind`/`refName`/...), which only ever
  worked because nothing had tried a naked or `REF(...)` dynamic lock
  target yet -- either would have crashed with a partial-record-selector
  exception. Both now resolve through `resolveWriteTarget` like every
  other `Ref` consumer, which also means a time-traveled `LOCK` target
  (silently ignored before) now correctly reports `SetOnReadOnly`
  instead.
* Feature ( not in the handbook): `SET $PIECE(ref,delim
  [,which])=val` and `SET $EXTRACT(ref[,start[,end]])=val` as SET
  targets (Part XIII previously only had these as expression-position
  reads). New `Phase.AST.SetTarget` type (`TargetRef`/`TargetPiece`/
  `TargetExtract`); `SSet`'s target list is now `[(SetTarget, Expr)]`.
  Matches real M: `$PIECE` pads with empty pieces past the current
  count; `$EXTRACT` pads with spaces past the current length, and
  `end < start` inserts without removing.
* Tests: new `Phase.Eval`/`Phase.Parser` group for naked references and
  the two SET-target forms, plus two 12-key `$ORDER`/`FOR EACH`
  regression tests (numeric collation across a double-digit key count,
  which the existing 1-2-3 test couldn't distinguish from a
  string-sorted bug).
* Verified (this session only, not previously possible): the project
  actually builds and all 324 tests pass, under GHC 9.4.7 via the
  procedure now in `BUILDING.md` Option C (distro GHC + the vendored
  `cabal-packages/` deps, no Hackage access). Confirmed the REPL itself
  too: fresh-database bootstrap, one `WRITE`, clean `.quit`.
* Fix (found only by actually compiling): `Phase.Storage.Sync`'s
  `openFd` call used the unix>=2.8 3-argument signature unconditionally;
  unix<2.8 (bundled with GHC 9.4.7, and possibly still in use elsewhere)
  has an extra `Maybe FileMode` argument. Now `MIN_VERSION_unix`-gated
  for both.
* Docs: `BUILDING.md` gained an "Option C" for building with an
  older/distro GHC via the bundled `cabal-packages/` tarballs as local
  packages, including the exact `hashable` patches that were needed and
  why (version bounds one patch-version too strict in three places; an
  unconditional `os-string` dependency not available before GHC 9.6/9.8;
  and three `Hashable`/`Hashable1` instance pairs that need an `Eq`
  instance `base` only gained after 4.17), and the UTF-8 locale the test
  binary needs to print a `§` in a test-group name.
* Fix: `.revert`/`.compact` in the REPL left a stale session running,
  every branch now ends in exactly one tail call to `loop` with the
  correct env, instead of a nested call followed by a further, stale
  one. Verified against the real executable.
* Fix: `KILL` now tombstones every descendant of the killed path, not
  just the path itself, in both the live commit path and WAL replay
  (`applyPointKill`/`applySubtreeKill`/`replayFrom`). New
  `Phase.Storage.Engine.killTombstonePaths` and
  `Phase.Tree.findSubNode`. Before this, a time-traveled read or
  `$DATA` of a killed descendant still saw the pre-kill state at any
  tx at or after the kill.
* Fix (format change, breaks compatibility with databases from before
  this change, per the team's go-ahead): `WalCheckpoint` now carries each
  entry's own last-write transaction, not just its value. Reopening
  from a checkpoint (or a compacted file) no longer loses
  `$DATA`/time-travel/`EXPECTING` correctness for a path whose last
  real write predates the checkpoint, `loadCheckpointEntries` seeds
  `seHistory`/`seLastTxMap` per entry instead of leaving them empty for
  anything the checkpoint didn't also replay fresh writes over.
* Docs: `docs/tx-pinning-design.md`, a design proposal (not
  implemented) for relative revision addressing (`^[-N]`, `^[LAST]`)
  and `DO WITH @TX(...)` snapshot-isolation-style blocks.
* Tests: regression coverage for all three storage fixes above,
  including a real-executable smoke test for the REPL fix.
* Feature ( `docs/tx-pinning-design.md` Part 1): relative
  revision addressing. Inside a time-travel bracket, `^[-N]NAME(...)` is
  N revisions back from the path's own latest revision, `^[LAST]` is
  that latest revision itself, and `^[LAST-N]` is ordinary arithmetic on
  it (written without spaces, like every other Phase expression). A
  KILL counts as one revision (its tombstone); going back further than
  a path's recorded history resolves to tx 0 ("before it existed"),
  which every absolute-tx reader already treats as undefined. New
  `Expr` constructor `TimeTravelLast`, `Phase.Parser.pTimeTravelExpr`,
  `Phase.Eval.resolveTimeTravelExpr` and `Phase.State.resolveRelativeTx`
 , all of them resolve a bracket down to a plain absolute `TxId`, so
  plain reads, `$DATA`, and `$GET` needed no relative-aware code paths
  of their own (and lost their repeated parse-the-tx-back-from-text
  boilerplate in the process). `$ORDER` with time travel is still
  `<NOTIMPL>`, as before.
* Fix: `killTombstonePaths` (added earlier in this changelog, for the
  KILL-descendant fix) double-inserted every kill's own tombstone
  revision: `dumpTree` already includes the node's own value at
  relative path `[]`, which the explicit `subKeys :` prefix then
  duplicated. Harmless to value reads, but it threw off anything that
  counts revisions, found by the first relative-addressing test that
  crossed a KILL, not by any existing test.
* Fix: `evalExpr` had no case for the new `TimeTravelLast`; GHC's
  non-exhaustive-pattern warning flagged it and it now raises a clean
  `<SYNTAX>` error instead of a Haskell pattern-match crash if `LAST`
  ever escapes a time-travel bracket.
* Feature: REPL `.adduser <name> [admin|write|read|none]` (admin only,
  default role `write`), prompts twice for a masked password, refuses
  an existing username or an unknown role before prompting, and goes
  through the ordinary permission-checked `writeRef` path rather than
  the bootstrap bypass. Previously the only way to add a user after the
  first admin was to hand-compute a PBKDF2 hash and salt outside Phase
  and `SET` them into `^%USERS`. Its writes are flushed explicitly (as
  `.revert` already did), the first draft wasn't, so a new account
  existed only in that process's memory and couldn't log in afterwards.
* Feature ( `docs/tx-pinning-design.md` Part 2):
  `DO WITH @TX(arg)` pinned blocks. A dotted block in which every bare
  GLOBAL read (plain, dynamic, naked, `$DATA`/`$GET`/`$ORDER`) is
  answered as of one transaction; `arg` is `LAST` (unpin), a global
  reference like `^[-2]FOO("BAR")`, or an absolute transaction number.
  New `SDoWith` statement, `Env.envPinnedTx`, `Phase.Eval.resolvePinExpr`
  / `runDoBlock` / `readPinnedOrLive`. Inside a block `^[-N]` counts from
  the pin while `^[LAST]`/`^[LAST-N]` count from live;
  `Phase.State.resolveRelativeTx` gained a `fromLive` flag for this.
  The explicit global binding list is still deferred, per the design.
* Feature: `$ORDER` as of a past transaction (`Storage.Engine.orderAtTx`,
  `State.orderAtTxRef`): both `$ORDER(^[tx]G(k))` and `$ORDER` inside a
  pinned block. It used to raise `<NOTIMPL>`.
* Fix (pre-existing REPL bug, found while testing the above): the
  continuation rule counted every `DO` word as an unclosed block
  opener, so a one-line `DO tag` swallowed the next lines until EOF,
  and a dotted block could only be ended by EOF. Now input continues
  only while the last line opens a block or is a dotted line, and a
  blank line (or a non-dotted line) ends it. The rule moved to
  `Phase.Lines.needsContinuation` so it is unit-tested. The explicit
  `END DO` spelling is no longer tracked by the REPL (it is a no-op at
  run time anyway; `:{`/`:}` remains the explicit multi-line fallback).
  `Phase.Lines.endsWithBareDo` also recognizes a `DO WITH @TX(...)`
  line as a block opener.
* Docs: README deviation 6 documents that `TAG^X` resolves `X` as a
  Catalog namespace (cross-namespace calls without `NAMESPACE`).
* Tests: eval-level coverage for `GOTO`, cross-namespace
  `CALL`/`DO`/`GOTO`, `%GLOBAL` fallback, and large-operand `\`/`#`.
  None of these has been compiled or run (no GHC in the authoring
  sandbox).

* Feature (`docs/tx-pinning-design.md`, "Explicit global
  bindings"): `DO WITH @TX(...) @BIND(^A,^B)`. A bind list selects WHICH
  globals the pin applies to; every other global reads live (it is not an
  error to use one). With no `@BIND` clause every global is bound, exactly
  as before. Bindings are whole globals (`^NAME` or `NS^NAME`), never
  subscripted paths; `@BIND()` binds nothing. New AST `PinSpec`/`BindName`;
  `SDoWith` now carries a `PinSpec` and an optional call target.
  Pins are now per global: `Env.envPinnedTx` stays the block-wide pin (a
  block with no bind list), and two new fields carry the rest --
  `envPinOverrides` (per-global pins from a bind list; `Nothing` = explicitly
  live) and `envBindScope` (the innermost enclosing bind list, `Nothing` =
  none). `Phase.State.effectivePin` answers "what is this global pinned to
  right now", and `readPinnedOrLive`, the `$DATA`/`$GET`/`$ORDER` implicit pin
  (`implicitPin` now takes the global's name) and `resolveRelativeTx`'s `^[-N]`
  baseline all go through it, so `^[-N]` counts from the pin for a bound
  global and from live for an unbound one.
  Nesting (`Phase.State.enterPinScope`): a nested bind list must be a subset
  of the enclosing one (new `<BIND SCOPE> ^NAME` error, `BindScope`), unless
  nothing encloses it; only the globals a nested list names take the new pin,
  the rest keep the outer pin (only `@TX(LAST)` releases); a nested block with
  no bind list re-pins exactly the globals the enclosing list bound (every
  global, when there is no enclosing list).
* Feature: single-call pin forms -- `DO WITH @TX(x) [@BIND(...)] TAG(args)`
  and, in expression position, `CALL WITH @TX(x) [@BIND(...)] TAG(args)` (new
  `Expr` constructor `CallWith`) run one call inside its own pin scope.
  `DO WITH @TX(LAST) TAG()` is the way to release a block's pin for one call.
  `Phase.Stmt.pDo`/`Phase.Parser.pCallExpr` share `Phase.Parser.pPinSpec`,
  which commits once it has seen ` WITH @TX(`, so a malformed bind list is a
  parse error instead of `DO WITH` being misread as a call to a tag named
  `WITH` (caught by the first test of a subscripted bind item).
  `Phase.Lines.endsWithDoWith` also recognises a block opener ending in
  `@BIND(...)`, and does not treat the single-call form as a block opener.
* Tests: 23 new cases (bound vs unbound reads, empty list, the nesting
  rules above, subset violation, per-global `^[-N]` baseline, `$DATA`,
  single-call forms, `Phase.Lines` block detection). Compiled and run with
  `ghc --make` (GHC 9.4.7, `-XGHC2021`; not through `cabal`): 386 of 386 pass,
  and `app/Main.hs` compiles. The real `cabal` build under GHC 9.10+ was not
  exercised.

* Feature (`docs/tx-pinning-design.md`, "Explicit global
  bindings"): `DO WITH @TX(...) @BIND(^A,^B)`. A bind list selects WHICH
  globals the pin applies to; every other global reads live (it is not an
  error to use one). With no `@BIND` clause every global is bound, exactly
  as before. Bindings are whole globals (`^NAME` or `NS^NAME`), never
  subscripted paths; `@BIND()` binds nothing. New AST `PinSpec`/`BindName`;
  `SDoWith` now carries a `PinSpec` and an optional call target.
  Pins are now per global: `Env.envPinnedTx` stays the block-wide pin (a
  block with no bind list), and two new fields carry the rest --
  `envPinOverrides` (per-global pins from a bind list; `Nothing` = explicitly
  live) and `envBindScope` (the innermost enclosing bind list, `Nothing` =
  none). `Phase.State.effectivePin` answers "what is this global pinned to
  right now", and `readPinnedOrLive`, the `$DATA`/`$GET`/`$ORDER` implicit pin
  (`implicitPin` now takes the global's name) and `resolveRelativeTx`'s `^[-N]`
  baseline all go through it, so `^[-N]` counts from the pin for a bound
  global and from live for an unbound one.
  Nesting (`Phase.State.enterPinScope`): a nested bind list must be a subset
  of the enclosing one (new `<BIND SCOPE> ^NAME` error, `BindScope`), unless
  nothing encloses it; only the globals a nested list names take the new pin,
  the rest keep the outer pin (only `@TX(LAST)` releases); a nested block with
  no bind list re-pins exactly the globals the enclosing list bound (every
  global, when there is no enclosing list).
* Feature: single-call pin forms -- `DO WITH @TX(x) [@BIND(...)] TAG(args)`
  and, in expression position, `CALL WITH @TX(x) [@BIND(...)] TAG(args)` (new
  `Expr` constructor `CallWith`) run one call inside its own pin scope.
  `DO WITH @TX(LAST) TAG()` is the way to release a block's pin for one call.
  `Phase.Stmt.pDo`/`Phase.Parser.pCallExpr` share `Phase.Parser.pPinSpec`,
  which commits once it has seen ` WITH @TX(`, so a malformed bind list is a
  parse error instead of `DO WITH` being misread as a call to a tag named
  `WITH` (caught by the first test of a subscripted bind item).
  `Phase.Lines.endsWithDoWith` also recognises a block opener ending in
  `@BIND(...)`, and does not treat the single-call form as a block opener.
* Tests: 32 new cases (bound vs unbound reads, empty list, the nesting
  rules above, subset violation, per-global `^[-N]` baseline, `$DATA`,
  `$ORDER`, `REF(...)` and naked references, namespace-qualified bind names,
  scope restoration after plain and nested blocks, single-call forms,
  `Phase.Lines` block detection). Compiled and run with
  `ghc --make` (GHC 9.4.7, `-XGHC2021`; not through `cabal`): 395 of 395 pass,
  and `app/Main.hs` compiles. The real `cabal` build under GHC 9.10+ was not
  exercised.

* Fix (`docs/audit-notes.md` #3): `MERGE` into a global now goes through the
  storage engine. `Phase.State.mergeRef` used to write the merged tree
  straight into `envGlobals`/`seLive`, so the merge was never committed:
  no TxId, no WAL records (lost on restart), no revisions (invisible to
  `^[-N]`, time travel and `EXPECTING`). Inside a transaction it also read
  the destination from `seLive` and overwrote `envGlobals` with the result,
  dropping a SET already staged against the same global, and it assigned
  `envGlobals` (staged data included) to `seLive`, leaking uncommitted data
  past a rollback. It also skipped the per-value `checkReservedGlobalWrite`
  a SET gets, so `MERGE ^%USERS(u)=...` could demote the last admin.
  Now a global `MERGE` is the sequence of SETs it stands for: every merged
  path is checked like a SET before anything is written (all or nothing);
  inside a transaction the paths are staged and `envGlobals` updated
  incrementally (committed storage untouched); otherwise all paths commit
  together via `commitTransaction` -- ONE TxId, one `WalSet` per path plus
  one `WalCommit`, one revision per path. No new `WalOp`, so replay stays
  deterministic. An absent source is a no-op and commits nothing. The
  source is still read live and does not follow a `DO WITH` pin. A
  `MERGE` into a local is unchanged. 13 new tests, 8 of which failed
  against the old code (commit count, WAL records, `^[-1]` history, reopen
  from a real file, staged-write loss, `seLive` leak, last-admin demotion).
  395 -> 408 tests, all passing under `ghc --make` (GHC 9.4.7).

## 0.1.0.0 -- 2026-08-02

* First version. Released on an unsuspecting world.
