# The Phase Handbook

## Index

The handbook has these parts:

- **Part I**, the language core: values, variables, globals, operators,
  control flow, line continuation. Read this part first.
- **Part II**, calling conventions: `CALL`/`RETURN`, reference parameters,
  closures, `EVALUATE`.
- **Part III**, error handling and transactions: conditions, `HANDLE`/
  `CATCH`/`RETRY`, `TRANSACTION` blocks.
- **Part IV**, string tools: interpolation, formatting, slicing,
  `FOR EACH`.
- **Part V**, namespaces.
- **Part VI**, dynamic references and macros: `REF(...)`.
- **Part VII**, time travel.
- **Part VIII**, graph operations.
- **Part IX**, secondary indexes.
- **Part X**, bulk data movement: `MERGE` and materializing joins.
- **Part XI**, blob storage.
- **Part XII**, storage and concurrency internals.
- **Part XIII**, intrinsic function reference.
- **Part XIV**, using the REPL.
- **Part XV**, what Phase does not implement yet.
- **Part XVI**, the HTTP server: running the server, TLS, auth, sessions,
  execute endpoints, admin endpoints, blob endpoints, configuration,
  graceful shutdown.
- Appendices: error reference, operator precedence quick reference.

---

# Part I, The Phase Language

## 1. What Phase is

Phase is a MUMPS-derived language. Phase is also its own database. Phase
has no ORM and no separate query language. A variable name that starts
with `^` names a database write. That write becomes durable the moment
`SET` runs.

Phase keeps M's best ideas. Phase keeps sparse hierarchical storage as its
native data model. Phase keeps strings as the one value type, coerced only
when code uses that value. Phase keeps `$ORDER`-style traversal over a
subscript level. Phase keeps a flat, line-oriented routine structure that
maps directly onto a program-counter execution model.

Phase drops the parts of M76 that came from 1976 teletype hardware limits,
not from good design. Phase has no mandatory terseness. Phase arithmetic
follows normal operator precedence, not strict left-to-right evaluation.
Phase has no string-eval indirection (`@`). See `phase-language-design.md`
§1 for the full reasoning behind each choice.

## 2. Values: everything is a string

Phase has exactly one data type: the string. Phase has no integer type, no
float type, no boolean type, and no date type. A number in Phase is a
string that looks numeric. Phase coerces a string to a number whenever
code uses that string in arithmetic or in a comparison
(`MValue.toBigDecimal`).

The coercion rule reads a **leading numeric prefix** and ignores the rest:

```
"12abc"  → coerces to 12       (leading numeric prefix, rest ignored)
"abc"    → coerces to 0        (no valid leading numeric prefix)
"  12"   → coerces to 0        (!), leading whitespace is NOT skipped
```

This is a real, documented M quirk. Phase keeps it on purpose. It is not a
parsing shortcut.

**Truthiness** follows the same coercion rule. A value coerces to a
number. A nonzero number is true. `IF X` means `IF X≠0` after coercion. A
non-numeric string such as `"hello"` is false, because it coerces to `0`.

**Arithmetic** (`+ - * / \ # **`) runs on `BigDecimal`, with 34 digits of
precision and `HALF_UP` rounding. `/`, `\` (integer division), and `#`
(modulo) throw `<DIVIDE BY ZERO>` when the divisor is zero. If a handler
is listening (Part III), Phase signals `<DIVIDE-BY-ZERO>` instead.

**Equality (`=`) is not plain string equality.** Phase checks both sides
against the same canonical-numeric-form rule that subscripts use (§3.2
below). When both operands already have that canonical numeric form, `=`
compares them as numbers. Otherwise, `=` falls back to exact string
comparison. This produces real, well-known M quirks:

```
1=1        → true    (both canonical numbers)
1=1.0      → false   ("1.0" is not canonical, trailing zero after the point)
"01"="1"   → false   ("01" is not canonical, leading zero, so this is a string compare)
```

**`]]`** ("sorts after") compares two values using Phase's own collation
rule (§3.2), not plain ASCII order.

## 3. Variables: locals vs. globals, and the sparse tree

Phase has exactly two kinds of variables. A sigil marks which kind:

|            | Syntax              | Lifetime                                                          | Scope                                                            |
|------------|---------------------|-------------------------------------------------------------------|------------------------------------------------------------------|
| **Local**  | `X`, `SUM`, `%TEMP` | Ends when the process ends, or on `KILL`, or on `NEW`-scoped exit | Visible to the whole process by default, not per routine, see §7 |
| **Global** | `^ORDERS`, `^X`     | Persists in the database; survives process death                  | Shared by every process with the database open                   |

```
SET X=5           ; local, in-memory only
SET ^X=5          ; global, written to disk, durable, shared
```

The `^` character is the whole difference between a scratch variable and
a database write. Phase has no `SAVE` command and no separate `commit()`
call. `SET` on a global name performs the write directly.

### 3.1 Node duality

A global or a local is addressed by a name, plus zero or more
comma-separated subscripts:

```
^ORDERS                          ; the root of the ORDERS tree
^ORDERS(1)                       ; subscripted by 1
^ORDERS(1,"PRICE")               ; two levels deep
^ORDERS(1,"LINES",3,"QTY")       ; four levels deep, no fixed depth limit
```

**One node can hold a value and have children at the same time.** These
two facts do not conflict:

```
SET ^ORDERS(1)="summary text"
SET ^ORDERS(1,"PRICE")=100
```

After both writes, `^ORDERS(1)` has its own value and a child at
`^ORDERS(1,"PRICE")`. Neither write disturbed the other. `$DATA` (§6)
reports which of four possible states a node is in.

**A local has the same duality.** `SET X=1` and `SET X(1)=2` do not
collide. A local's bare value and its subscripted values sit in
independent storage slots. `Environment`'s `LocalVar` stores a bare value
at `SubscriptPath.ROOT` and stores subscripted values at deeper paths, in
the same `NavigableMap`. This is why `$DATA`, `KILL`, and `$ORDER` behave
the same way on a local as on a global: both reuse the same range-query
code.

### 3.2 Subscript collation: numbers sort before strings, always

Phase defines a **canonical numeric form** for a subscript. A canonical
number has no leading zero, no leading `+`, no trailing zero after a
decimal point, no trailing `.`, and no exponent notation. `1`, `23`, `-4`,
and `1.5` are canonical. `01`, `+1`, `1.50`, `1.0`, and `1E2` are **not**
canonical. Phase treats each of these as a plain string, even though each
one looks numeric.

This rule decides collation order. Every canonical-numeric subscript sorts
before every string subscript. Within each group, Phase sorts numeric
subscripts by numeric value (`1 < 2 < 10`, not by character order) and
sorts string subscripts by character order. `^A(1)` and `^A("1.0")` are
genuinely different, unrelated subscripts. They are not the same key with
different formatting.

This rule is also why `$ORDER` traversal (§6) can walk subscripts in an
order you may not expect at first: every canonical number, in numeric
order, then every string, in character order.

## 4. Operators and precedence

Real M76 has no precedence at all; every operator associates strictly
left to right. Phase breaks from that on purpose (see design doc §2) and
implements a normal precedence ladder. `Parser.parseExpr` climbs this
ladder; it does not scan the line flatly.

From loosest to tightest binding:

| Level        | Operators                                                                                | Associativity |
|--------------|------------------------------------------------------------------------------------------|---------------|
| 1 (loosest)  | `&` (AND), `!` (OR)                                                                      | left          |
| 2            | `=` `<` `>` `[` (contains) `]]` (sorts-after) `?` (pattern match), `'` (e.g. `'=`, `'[`) | left          |
| 3            | `_` (string concatenation)                                                               | left          |
| 4            | `+` `-` (binary)                                                                         | left          |
| 5            | `*` `/` `\` (integer division) `#` (modulo)                                              | left          |
| 6            | `**` (exponentiation)                                                                    | **right**     |
| 7 (tightest) | unary `-` `+`, `'` (logical NOT), `@` (indirection, see Part XV)                         | prefix        |

```
WRITE 1+2*3,!          ; 7, * binds tighter than +, unlike M76 (which gives 9)
WRITE 2**3**2,!         ; 512, ** is right-associative: 2**(3**2)
```

Slice syntax (`s[start:end]`, Part IV) binds tighter than every operator
above. Phase applies slicing as a postfix wrap directly on the value that
comes before it.

`&` and `!` do **not** short-circuit. Phase always evaluates both
operands, exactly as M76 does. Every `Expr` node holds only literal data
or child `Expr` nodes, so Phase's expression grammar has no side effects
to skip. Short-circuiting would have no observable effect today. A
condition (Part III) is signaled from a statement or an intrinsic, never
from a bare operator.

**Implementation note.** The class doc for `Expr.java` still carries old
text that claims "no precedence climbing here," describing M76's flat
left-to-right rule. That text predates the precedence-ladder rewrite and
does not describe current behavior. Trust `Parser.java`'s own class doc
and its `parseOr`/`parseCompare`/`parseConcat`/`parseAdditive`/
`parseMultiplicative`/`parsePower`/`parseUnary` methods instead.

## 5. Control flow is line-oriented, not block-structured

Phase's control-flow commands do not wrap a block, the way `if { }` does
in C-family languages. `IF` and `FOR` each govern **the rest of the
current line**, and nothing more:

```
IF X>0 WRITE "positive",! WRITE "still true branch",!
```

Both `WRITE` statements run only when `X>0`, because both sit on the same
line as the `IF`. `GOTO` and `DO TAG` resolve to a line index, not to a
nested scope. A routine is a flat, indexable list of lines with labels.
This is a deliberate design choice, not a shortcut. It lets `Interpreter`
walk the routine with a simple `pcLine`/`pcStmt` program counter, instead
of recursing into nested block statements. It also lets a future
bytecode VM consume the AST directly, with no extra step to "un-flatten"
it first. `Stmt.java`'s own class doc states this directly: `Stmt.If` and
`Stmt.For` hold no body at all.

### 5.1 `IF` / `ELSE` and `$TEST`

```
IF cond1,cond2,...    ; comma = AND, short-circuits on the first false one
```

Every `IF` sets the special variable `$TEST` to its own boolean result. On
a false `IF`, execution skips forward only as far as the next `Stmt.Else`
on the same line, not to the end of the line. Phase does this so `ELSE`
stays reachable:

```
IF X>0 WRITE "positive",! ELSE  WRITE "not positive",!
```

`ELSE` takes no condition of its own. `ELSE` checks `$TEST` from the most
recent `IF` on the same line. If that `IF` was true, `ELSE` skips the
rest of the line. If that `IF` was false, `ELSE` runs.

### 5.2 `FOR`

Phase supports every classic form, plus one Phase-specific form,
`FOR EACH`:

```
FOR                              ; infinite loop
FOR X=start                      ; runs once with X=start
FOR X=start:increment            ; assign, run, add increment, repeat forever
FOR X=start:increment:stop       ; as above, stops once X passes stop
FOR EACH var IN collection DO    ; iterate a local/global's subscript level, see Part IV §4
. WRITE var,!
```

### 5.3 `GOTO` / `DO TAG`

- A target is a bare `TAG`, a `TAG^ROUTINE`, or a bare `^ROUTINE` (entry at
  line 0).
- An unresolvable tag throws `<UNDEFINED LABEL>`.
- A duplicate tag inside one routine is a **parse-time** error.
- A cross-routine call (`TAG^ROUTINE`, where `ROUTINE` is not the current
  routine) needs a `RoutineLoader` configured on the `Interpreter`
  (`interp.routineLoader`, `null` by default). Without one, Phase throws
  `<UNDEFINED ROUTINE>`.
- If the current routine declares a `NAMESPACE` (Part V), an unqualified
  tag reference first tries `namespace.tag`. If that fails, Phase falls
  back to a bare lookup.

### 5.4 Dotted blocks

Classic M supports an argumentless `IF`/`FOR` followed by an indented
dotted block. Phase does not. `Stmt.java`'s own class doc states this
directly. Phase does support a leading-dot convention for a bare `DO`'s
own implicit block:

```
FOR EACH K IN ^LIST DO
. WRITE K,!
```

`RoutineBuilder` strips leading `.` characters off each source line and
counts them as that line's dot depth. A bare `DO`'s block runs through
every following line whose dot depth is greater than the `DO` line's own
depth. This is a source-layout rule, resolved before parsing. It does not
build a nested-block AST node. The routine stays a flat line list
underneath.

Phase also supports an explicit `DO ... END DO` form. Use this form for
`HANDLE`/`CATCH`/`RETRY` blocks (Part III) and for `TRANSACTION` blocks
(Part III §2).

### 5.5 Line continuation

A long line is hard to read. Phase lets you split one logical line across
several physical lines. Use a leading dot on the continuation line:

```
MERGE report(oid,"CUSTOMER")=^CUSTOMERS(^ORDERS(oid,
.                          "CUSTOMER"))
```

The parser decides what a leading dot means by one rule. First, the
parser finds the highest block depth it can reach from the line above.
The parser can reach one level deeper than the line above only when that
line ends with a bare `DO`. Then the parser compares the new line's dot
count to that reachable depth. If the dot count is higher than the
reachable depth, the new line is a **continuation**: the parser joins it
to the line above. If the dot count is at or below the reachable depth,
the new line is a **new statement** at that depth, exactly as dotted
blocks already work (§5.4).

This one rule covers two needs. First, it lets you wrap one long
statement's own argument list onto a further line. Second, it leaves
today's dotted-block behavior unchanged: two sibling statements at the
same depth still stay as two separate, independently addressable lines.

A continuation removes only the leading dots and the whitespace right
after them. The parser joins the remaining text directly, with no space
added. This lets a continuation split a token, such as a long string
literal:

```
WRITE "This sentence starts on one line and continues
.right here with no gap at all.",!
```

To keep exactly one space at the join point, type that space before the
line break, not after it:

```
WRITE "This word boundary is kept 
.because a space comes before the line break.",!
```

Indentation on a continuation line is always safe. The parser strips all
leading whitespace after the dots, so you may align a continuation line
under the statement it continues:

```
FOR EACH oid IN ^ORDERS
.  MERGE wrapped(oid)=^ORDERS(oid)
.  MERGE wrapped(oid,"CUSTOMER")=^CUSTOMERS(^ORDERS(oid,"CUSTOMER"))
.  DO
```

A continuation also works inside a real block. A statement inside a
block body may still wrap across lines. It needs one more dot than the
block's own depth:

```
FOR EACH oid IN ^ORDERS DO
. MERGE nested(oid,"CUSTOMER")=^CUSTOMERS(^ORDERS(oid,
.. "CUSTOMER"))
```

`RoutineBuilder` groups physical lines into logical lines before it
parses any of them. This lets the first physical line of a wrapped
statement stay incomplete on its own. The parser only checks the
completed, joined text.

## 6. The core intrinsic functions

Part XIII holds the full intrinsic reference. Three intrinsics are worth
knowing on sight, since almost every routine uses them.

### `$DATA(ref)`, four-state existence check

```
$DATA(^X)
```

| Result | Meaning                                          |
|--------|--------------------------------------------------|
| `0`    | No value at this exact node, no children either  |
| `1`    | Has a value at this exact node, no children      |
| `10`   | No value at this exact node, but has children    |
| `11`   | Has a value at this exact node, and has children |

`IF $DATA(^X)>9` means "this node has children, regardless of its own
value." Learn to recognize this idiom on sight.

### `$ORDER(ref)`, walk a subscript level

Give `$ORDER` a starting subscript. That subscript need not already
exist. `$ORDER` returns the next subscript that does exist, in collation
order (§3.2). `$ORDER` never loads the whole tree into memory:

```
SET I=""
FOR  SET I=$ORDER(^ITEM(I)) QUIT:I=""  WRITE I,":",^ITEM(I),!
```

`$ORDER` works the same way over a local and over a global. `$ORDER` does
**not** work over a time-traveled reference (Part VII).

### `$TEST`, the last `IF`/postcondition result

§5.1 above describes `$TEST` in full.

## 7. Locals are process-global, not per-routine

A local variable is visible to the **whole process**: every subroutine
call, every nesting level, by default. Phase applies no automatic
per-subroutine scoping:

```
SET X=1
DO SUB
WRITE X    ; prints whatever SUB left X as; SUB saw and could modify the SAME X
```

To give a subroutine its own private copy of a variable, one that Phase
restores automatically on return, say so with `NEW`:

```
SUB  NEW X
     SET X=99
     QUIT
     ; X reverts to its pre-DO value here, back in the caller
```

`NEW X` stack-scopes `X` for the rest of the current `DO`/tag-call frame.
`NEW` saves the current binding, or saves the fact that `X` was
undefined. `NEW` then clears `X`. When that frame exits, `NEW` restores
the saved binding. A bare `NEW`, with no arguments, does this for every
local defined at that moment.

**Implementation shape.** One `Environment` exists per `Interpreter`. That
one `Environment` holds every local for the process's whole call stack,
in one flat `Map<String, LocalVar>`. `NEW` does not create a second
scope. `NEW` pushes a restore action onto the *current call frame's*
restore list (`Environment.newVar`/`newAll`). `Interpreter` pops that
list, in reverse order, whenever a `DO`/tag-call frame exits. Reference
parameters (Part II §2) use this same mechanism:
`Environment.alias(name, targetName)` makes two names point at the same
underlying storage. Phase restores that alias on frame exit, with the
exact same push/pop steps an ordinary `NEW` uses.

## 8. Postconditions, the `:` qualifier

Nearly every command accepts a trailing `:expr` right after the command
word:

```
WRITE:X>0 "positive",!
QUIT:I=""
SET:DONE X=1
```

Read this as "run this command, but only if `expr` is true." Phase
evaluates the postcondition **before** the command's own arguments. If
the postcondition is false, Phase skips the whole statement, arguments
included, and moves to the next statement.

`FOR` and `LOCK` also use `:` for their own, unrelated purposes (increment
and stop values, and timeout). A command's own postcondition, when
present, is always the *first* `:` right after the command word, before
any of that command's own colon syntax.

`NAMESPACE`, `CONDITION`, `HANDLE`, `CATCH`, and `WHEN` do **not** accept
a postcondition at all. The parser throws an error if a `:` follows any
of these five words.

## 9. Commands reference

**Implementation note: no abbreviations.** Classic M lets you write `S`
for `SET`, `W` for `WRITE`, and so on. Phase's parser recognizes **full
command words only**. `Parser.java`'s `COMMANDS` table states this
choice directly: Phase made "a clean cutover to full words only, no
abbreviated forms accepted at all." Writing `S X=1` does not parse as
`SET`. An unrecognized leading word either matches a registered MACLIB
macro (Part VI §2), or falls through to `Stmt.Unsupported`, which throws
`<NOTIMPL>` at execution time. This is not a parse error, so a script
never fails to parse merely for containing something Phase has not
implemented yet.

| Command                              | Forms                                                                                                                                                                  |
|--------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `SET`                                | `SET t=v`; `SET t1=v1,t2=v2`; `SET t EXPECTING expr = v` (optimistic-concurrency form, Part III §2); `SET t1,t2=CALL tag(...)` (multi-value destructuring, Part II §1) |
| `KILL`                               | Bare `KILL` (kills all locals); `KILL target,target,...` (point- or subtree-kill, decided at evaluation time by whether the target has live descendants)               |
| `WRITE`                              | `WRITE expr,expr,...`; items may be `!` (newline), `#` (form feed), `?N` (tab to column N)                                                                             |
| `IF` / `ELSE`                        | `IF cond1,cond2,...` (comma = AND); bare `ELSE`                                                                                                                        |
| `FOR`                                | Bare `FOR`; `FOR X=start`; `FOR X=start:inc`; `FOR X=start:inc:stop`; `FOR EACH var IN collection`                                                                     |
| `DO`                                 | Bare `DO` (dotted implicit block or explicit `END DO`); `DO TAG`; `DO TAG(args)`; `DO TAG^ROUTINE`; `DO ^ROUTINE`; `DO tag(&ref, ...)` reference args                  |
| `GOTO`                               | `GOTO TAG`; `GOTO TAG^ROUTINE`; `GOTO ^ROUTINE`                                                                                                                        |
| `QUIT`                               | Bare `QUIT`; `QUIT expr`                                                                                                                                               |
| `RETURN`                             | Bare `RETURN`; `RETURN v1,v2,...` (Part II §1)                                                                                                                         |
| `NEW`                                | Bare `NEW` (news every current local); `NEW X,Y,Z`                                                                                                                     |
| `LOCK` / `UNLOCK`                    | Bare (release/acquire everything); `LOCK ref,+ref,-ref[:timeout]` (Part XII §3)                                                                                        |
| `TRANSACTION` / `END TRANSACTION`    | Part III §2                                                                                                                                                            |
| `NAMESPACE`                          | Part V                                                                                                                                                                 |
| `EDGE`                               | `EDGE FROM ^ref TO dst [WEIGHT w]`; `EDGE DELETE ^ref TO dst` (Part VIII)                                                                                              |
| `INDEX` / `REBUILD INDEX`            | `INDEX ON ^ref(*,...) AS name`; `INDEX DELETE name`; `REBUILD INDEX name` (Part IX)                                                                                    |
| `CONDITION`                          | `CONDITION <NAME>(field1, field2, ...)` (Part III §1)                                                                                                                  |
| `SIGNAL`                             | `SIGNAL <NAME>(field: expr, ...)` (Part III §1)                                                                                                                        |
| `HANDLE` / `CATCH`                   | Clause headers inside `DO...END DO` / `TRANSACTION...END TRANSACTION` (Part III)                                                                                       |
| `RETRY`                              | Bare `RETRY`; `RETRY [n TIMES] [WITH field=expr,...]` (Part III §1)                                                                                                                |
| `EVALUATE` / `WHEN` / `END EVALUATE` | Part II §5                                                                                                                                                             |
| `DEFINE MACRO` / `DROP MACRO`        | Part VI §2                                                                                                                                                             |
| `MERGE`                              | `MERGE t=s`; `MERGE t1=s1,t2=s2,...`; `MERGE:postcondition t=s` (Part X §1)                                                                                            |
| `BLOBSAVE`                           | `BLOBSAVE hash TO path` (Part XI §2)                                                                                                                                   |
| `BLOBMETA`                           | `BLOBMETA hash SET field=expr,...` (Part XI §2)                                                                                                                        |

## 10. A minimal working example
    
```
MAIN ; entry point (tag with no leading space = a line label)
 NEW I,SUM
 SET SUM=0
 FOR I=1:1:5 SET SUM=SUM+I
 WRITE "SUM=",SUM,!
 SET ^ORDERS(1,"PRICE")=100
 IF ^ORDERS(1,"PRICE")>50 WRITE "expensive",! ELSE  WRITE "cheap",!
 QUIT
```

`NEW` stack-scopes two working locals. `FOR I=1:1:5` runs the rest of the
line five times. The `SET` on a `^`-prefixed name performs a durable
database write. The `IF`/`ELSE` pair relies on `$TEST`. `QUIT` ends the
routine.

---

# Part II, Calling Conventions & Structured Control

## 1. `DO` vs. `CALL`: statement-position vs. value-producing

`DO` stays in statement position. `DO` produces side effects only. `CALL`
is Phase's expression-position counterpart. `CALL` invokes a tag and
yields a value.

```
TOTAL(orders,taxRate)
    NEW sum
    SET sum = orders
    RETURN sum,sum * taxRate
```

```
WRITE CALL DOUBLE(21),!            ; ordinary expression position, yields only the FIRST returned value
SET total,tax = CALL TOTAL(myOrders,2)   ; multi-value destructuring
```

A tag definition may name formal parameters, as `TOTAL(orders, taxRate)`
does above. Phase binds each one positionally at the call site, and
auto-`NEW`s each one into the callee's own frame. These parameters never
leak into, or overwrite, a same-named local in the caller.

`RETURN expr, expr, ...` is the multi-value alternative to a bare `QUIT`.
Phase destructures a `RETURN`'s values only at a `SET target,target =
CALL ...` site (`Stmt.CallAssign`). An ordinary expression-position
`CALL`, such as a `WRITE` argument or an arithmetic operand, has nowhere
to place more than one value, so it always yields just the first one.

**A wrong argument count is a hard runtime error**, not silent padding or
truncation:

```
<SYNTAX> CALL returned 1 value(s), 2 target(s) expected
```

## 2. Reference parameters

```
CALL DOUBLEIT(&x)
DOUBLEIT(n)
 SET n=n*2
 QUIT
```

A `&name` argument at the call site binds the callee's formal parameter
as a true alias into the caller's own local storage
(`Environment.alias`). This is not a copy. A write inside the callee
stays visible to the caller after the call returns. The operand of `&`
must be a bare local, never a subscripted local and never a global.
Aliasing a global, or an arbitrary expression, makes no sense under this
mechanism, so Phase rejects it with `<SYNTAX>`.

## 3. Closures: value-snapshot only

```
SET discountFn = CLOSURE APPLYDISCOUNT() CAPTURING rate = 0.15
SET adjusted = CALL discountFn(100)
APPLYDISCOUNT(amount)
 RETURN amount*(1-rate)
```

A `CLOSURE` literal evaluates every `CAPTURING name=expr` once, **at the
moment the literal itself runs**, not on each later call. Changing `rate`
after you create `discountFn` does not change what `discountFn` uses.
Phase encodes the result as an ordinary opaque string, using the same
sentinel-prefixed convention the interpreter already uses for blob
references (Part XI). A closure is a plain Phase value. You may store a
closure in a local, pass it around, and invoke it more than once through
`CALL`.

Phase does **not** support true closures over shared mutable state.
`Environment`'s whole design assumes a local lives only as long as the
frame that owns it. A real closure would need its captured bindings to
outlive that frame. A value-snapshot closure covers the practical case of
passing a configured function around, without touching that design rule.

## 4. Expression-position conditional

```
SET x = IF 5>3 THEN "yes" ELSE "no" END IF
```

`IF ... THEN ... ELSE ... END IF` is a true expression (`Expr.IfExpr`).
Use it anywhere Phase expects a value. This is distinct from
statement-position `IF`, which gates later statements through `$TEST`
rather than producing a value. This expression form short-circuits:
Phase evaluates only the branch it takes.

## 5. `EVALUATE`: a generalized, COBOL-style multi-branch construct

Phase implements all three of COBOL's `EVALUATE` modes.

**Switch mode** tests one subject for equality against each `WHEN`:

```
EVALUATE orderStatus
    WHEN "PENDING" WRITE "pending",!
    WHEN "SHIPPED" WRITE "shipped",!
    WHEN OTHER     WRITE "unknown",!
END EVALUATE
```

**Condition-chain mode** sets the subject to `TRUE`. Each `WHEN` becomes
its own boolean expression, tested in order. Use this mode in place of a
long `IF`/`ELSE IF` chain:

```
EVALUATE TRUE
    WHEN price>1000 WRITE "bulk",!
    WHEN price>100  WRITE "standard",!
    WHEN OTHER      WRITE "none",!
END EVALUATE
```

**Multi-subject tuple matching (`ALSO`)** tests one condition per
subject:

```
EVALUATE customerType ALSO orderTotal
    WHEN "PREMIUM" ALSO ANY   WRITE "free-premium",!
    WHEN ANY       ALSO >500  WRITE "free-bigorder",!
    WHEN OTHER                WRITE "standard-shipping",!
END EVALUATE
```

`ANY` always matches. A bare comparison operator, such as `>500` or
`'=x`, tests directly against that position's subject. Every `WHEN`
clause, in any mode, may carry an optional `WHERE` guard. Phase ANDs this
guard onto the match:

```
EVALUATE TRUE
    WHEN TRUE WHERE n#2=0 WRITE "even",!
    WHEN OTHER              WRITE "odd",!
END EVALUATE
```

If nothing matches, and no `OTHER` clause exists, `EVALUATE` falls
through with no error. This matches COBOL's own permissive behavior.

**Implementation note: how `EVALUATE` runs.** Phase gives it the same
two spellings as `DO`: the `END EVALUATE` form shown above, and a dotted
form in which the `WHEN` lines are the dotted lines under the header.
Both end up as the same thing in the engine: the clauses are the lines
one level deeper than the header (`Phase.Lines` assigns that depth to an
`END`-delimited body), and `EVALUATE` runs them in place, not as a
sub-routine. The consequences:

- Every subject is evaluated once. Clauses are tried top to bottom and
  the first match wins, with no fall-through, so a `WHEN OTHER` placed
  above other clauses shadows them. A `WHERE` guard is only evaluated
  after every test of its clause has held.
- `EVALUATE` is not a scope. A `QUIT`, `RETURN` or `GOTO` in a clause
  body leaves the enclosing routine. (A `WHEN ... DO` clause with a
  dotted body is an ordinary dotted `DO` for that inner body only.)
- `TRUE` is recognized as the chain subject in the `EVALUATE` header and
  as the literal `1` in a `WHEN`; Phase has no `TRUE` literal elsewhere.
  Against a `TRUE` subject a plain `WHEN expr` matches when `expr` is
  true; against any other subject it matches when the subject equals
  `expr`.
- A `WHEN` takes no postcondition (§8). `EVALUATE` and `END EVALUATE`
  accept one. A `WHEN` with a different number of tests than the header
  has subjects is a `<SYNTAX>` error when reached, as is a `WHEN` with no
  `EVALUATE` above it.
- Explicit `DO ... END DO` works the same way: a bare `DO` whose next
  line is not deeper is the start of an `END`-delimited body. A bare
  `DO` that never meets an `END DO` stays the empty bare `DO` it has
  always been, and an `END DO` closes the nearest open `DO`.

**Implementation note: path-shape `WHEN` clauses are not implemented.**
The design document describes a fourth `WHEN` mode. That mode would
destructure a subscript-path value by segment count and wildcard (`WHEN
(_, "LINES", n) ...`). Phase defers this mode on purpose, pending a
broader decision about whether Phase ever adds a value kind beyond plain
strings. `Stmt.WhenTest` has only three cases today (`ValueTest`,
`CompareTest`, `AnyTest`). It reserves no fourth case yet, in a way that
needs no rework once that broader question is answered.

---

# Part III, Error Handling & Transactions

## 1. Conditions: `CONDITION`, `SIGNAL`, `HANDLE`/`CATCH`/`RETRY`

Phase adopts a narrowed version of Common Lisp's condition system: no
true resumption, and no continuations. A signal can only mean "unwind to
the nearest enclosing handler." Recovery can only mean "re-run the
enclosing block from its top." Phase makes this system no more powerful
than that on purpose. It reuses the same jump-back machinery `FOR`
already uses for its own loop, rather than building a second mechanism.

```
DO
    SET ^ORDERS(id,"STATUS") EXPECTING $TX = "SHIPPED"
HANDLE <CONFLICT> AS c
    RETRY
CATCH <ANYTHING> AS e
    WRITE "unrecoverable: ",e("message"),!
END DO
```

- **`CONDITION <NAME>(field1, field2, ...)`** declares a condition's field
  list up front, for validation. This declaration only matters at compile
  time; it is a no-op at runtime. `RoutineBuilder` collects it into a
  `ConditionRegistry`.
- **`SIGNAL <NAME>(field: expr, field: expr, ...)`** raises a condition.
  Phase walks the enclosing handler stack outward, looking for the
  nearest `HANDLE` or `CATCH` clause that names this condition, or that
  names `<ANYTHING>` (a catch-all). Note that `SIGNAL` separates a field
  name from its value with `:`. `RETRY WITH` (below) uses `=` instead.
- **`HANDLE <NAME> AS binding ...`** is a retry-capable clause. If its
  body runs `RETRY`, the *entire enclosing block* re-executes from its
  first statement.
- **`CATCH <NAME> AS binding ...`** is the ordinary unwind-and-move-on
  form. Its body runs once. Control then falls through to whatever
  follows `END DO`.
- If a block has both a specific clause, such as `CATCH <OOPS>`, and a
  catch-all clause, `CATCH <ANYTHING>`, the specific clause wins,
  regardless of which one you wrote first.
- A plain `DO...END DO` block with no `HANDLE`/`CATCH` inside it stays an
  ordinary block. The handler machinery only activates when at least one
  such clause is present.

**Access a condition's fields with `c("fieldName")`, not `c.fieldName`.**
This is a deliberate difference from the design document's own dotted-
access sketch. Phase's tokenizer has no general `.`-as-operator grammar.
Phase binds condition fields as ordinary subscripted locals under the
`AS` name instead, using the same mechanism `Expr.LocalIndex` already
provides, spelled with parentheses. Every signaled condition also gets an
automatic `message` field (`e("message")`), a human-readable summary of
its name and its fields.

**`RETRY [WITH field=expr, field=expr, ...]`** re-enters the enclosing
block from the top. Phase evaluates each `WITH` field *before*
re-entering the block, while the handler's own bound condition variable
is still in scope. This lets `RETRY WITH expectedTx = c("actualTx")`
reference `c`. Phase then seeds each field as an ordinary local for the
next pass. `RETRY` used outside any `HANDLE` clause is a runtime
`<SYNTAX>` error.

**`RETRY n TIMES [WITH ...]`** (also `RETRY 1 TIME`) is a bounded retry: it
only re-enters the block while the block has been re-entered fewer than
`n` times so far. Once the `n` re-entries are used up, `RETRY` does
nothing and the handler body carries on with the statement after it, so
fallback code can follow it. `n` is an ordinary expression. `RETRY 2
TIMES` therefore lets the body run three times in all.

### 1.1 Built-in conditions

**Implementation note.** `ConditionRegistry` does **not** pre-register any
built-in condition names or field lists, unlike what the design document
describes. `ConditionRegistry` only validates fields for a condition a
routine explicitly declares with `CONDITION`. The registry places no
constraint on a built-in signal. Here are the actual condition names and
fields the interpreter signals internally:

| Condition              | Fields                                          | Raised when                                                                                                                                                                  |
|------------------------|-------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `<CONFLICT>`           | `detail`                                        | `END TRANSACTION` fails its commit check, or a `SET ... EXPECTING n` mismatch happens inside a `TRANSACTION`. Phase rolls back the transaction first.                        |
| `<LOCK-TIMEOUT>`       | `global`, `path`, `timeoutMs`                   | A `LOCK` fails to acquire, **and** at least one enclosing `HANDLE`/`CATCH` exists. With no handler present, `LOCK` keeps its plain boolean-return behavior; no signal fires. |
| `<UNDEFINED>`          | `name`, `path`                                  | Code reads an undefined local or global, **only when a handler is present**.                                                                                                 |
| `<DIVIDE-BY-ZERO>`     | `dividend`, `divisor`                           | `/`, `\`, or `#` divides by zero, **only when a handler is present**. Otherwise, the plain `<DIVIDE BY ZERO>` error propagates instead.                                      |
| `<INDEX-BUILD-FAILED>` | `indexName`, `globalName`, `reason`, `lastPath` | *Deprecated with secondary indexing; this engine never raises it.* An index backfill or rebuild fails, or `$INDEXLOOKUP` queries an index that is not `READY` (Part IX).                                                                        |

**Implementation note.** `<TRAVERSAL-LIMIT>` (Part VIII) is a condition in
concept. Phase currently raises it as a **plain `MRuntimeException`**
instead, not through `SIGNAL`/`Condition`. `HANDLE`/`CATCH` cannot catch
it today, unlike every condition in the table above.

### 1.2 Unhandled signals

A `SIGNAL` with no matching `HANDLE`/`CATCH` anywhere in the current call
stack propagates outward. Phase promotes it into an ordinary halting
error, exactly like an uncaught runtime exception. This is what makes the
whole mechanism strictly additive: code that never uses `HANDLE`/`CATCH`
behaves exactly as if the condition system did not exist.

**Implementation note: how handled blocks run.** `HANDLE`/`CATCH` apply to
`DO ... END DO` blocks and to dotted `DO` blocks (including `FOR` and `DO
WITH` bodies), which is every block there is until `TRANSACTION` exists.
A block takes part only if it has a clause of its own; every other block
is an ordinary block. The clause heads are the lines at the block's own
depth that start with `HANDLE`/`CATCH`: everything before the first one is
the body, and each clause's body is the rest of its head's line plus the
lines up to the next head. The consequences:

- **Each top-level line of the body is atomic.** When a condition is
  caught, the state is rolled back to the start of the line that raised
  it: everything earlier lines did (locals, committed writes, output)
  stays, and everything that line did is dropped. A line with a dotted
  body, such as a `FOR`, is one line, so a `FOR` that raises on its third
  pass is undone as a whole. Put the `HANDLE` inside the loop body if you
  want per-iteration handling.
- **`RETRY` re-runs the body on the state the handler left behind**, not on
  the state at block entry. Earlier passes are not undone. (A `TRANSACTION`
  block will add that.) `RETRY n TIMES` is counted per block entry and
  shared by every `RETRY` in it.
- **A plain `RETRY` has a ceiling**, so a handler that retries an error
  that never goes away cannot loop forever. It is read from
  `^_CONFIG("retry","max")` (default 1000) and also caps `RETRY n TIMES`.
  Reaching it is an ordinary halting error, not a condition.
- **Clauses run outside their own block's handler scope.** A signal raised
  by a clause goes to the handlers outside the block, never to its
  sibling clauses. `RETRY` is legal only in the body of a `HANDLE` clause,
  lexically: not in a `CATCH` body, not in the block's body, and not in a
  routine that a `HANDLE` body calls.
- **The `AS` binding is optional** and is cleared before each new
  condition is bound, so a field of an earlier condition cannot be read as
  part of this one. A `message` field supplied to `SIGNAL` takes
  precedence over the generated one.
- **A handler is present** for the whole dynamic extent of a block's body,
  callees included, and not after the block ends or while its clauses run.
  `<DIVIDE-BY-ZERO>` and `<LOCK-TIMEOUT>` are signalled at their own sites
  only then. `<UNDEFINED>` is made from the undefined-variable error by the
  block that catches it, with `name` as written (a global keeps its `^`)
  and `path` as the subscripts joined by commas. A built-in condition that
  no clause names halts as `<DIVIDE-BY-ZERO> dividend=1, divisor=0`, not as
  the plain `<DIVIDE BY ZERO>`.
- **`CONDITION` declarations are per routine.** They are collected when the
  routine starts, wherever they appear in it. A declared condition's
  `SIGNAL` may only name its declared fields; any other field is a
  `<SYNTAX>` error, which condition handlers do not catch. A callee is not
  bound by its caller's declarations.
- **Only conditions are caught.** `HANDLE`/`CATCH` never see plain errors
  (`<PERMISSION DENIED>`, `<SYNTAX>`, and so on), `<ANYTHING>` included.
- `<CONFLICT>` is not raised yet, because `TRANSACTION` and `EXPECTING` do
  not exist. `<INDEX-BUILD-FAILED>` will not be, secondary indexing being
  deprecated.

## 2. `TRANSACTION` blocks

```
TRANSACTION
    SET ^ORDERS(id,"STATUS") EXPECTING $TX = "SHIPPED"
    SET ^ORDERS(id,"SHIPPED-AT") = $HOROLOG
HANDLE <CONFLICT> AS c
    RETRY
END TRANSACTION
```

Every `SET`/`KILL` inside the block routes through one real
`PhaseDB.Transaction`, opened on entry. `END TRANSACTION` commits that
transaction. If a condition escapes the block unhandled, Phase rolls the
transaction back instead. `HANDLE`/`CATCH`/`RETRY` inside a `TRANSACTION`
block use exactly the mechanism from §1. `TRANSACTION` is sugar for "a
`HANDLE`-capable block that also opens a transaction." It is not a second,
separate mechanism.

`SET target EXPECTING expr = value` feeds `expr` into the transaction's
optimistic-concurrency check, a first-writer-wins rule. If the target's
last writer is not the transaction id `expr` names, the commit fails and
Phase signals `<CONFLICT>`.

**Implementation note.** The design document states that nested
`TRANSACTION` blocks are forbidden ("`<SYNTAX> nested TRANSACTION not
supported`"). **Phase does not enforce this restriction.** Lexical
nesting of `TRANSACTION` blocks works. An outer `HANDLE`/`CATCH` can
catch a `<CONFLICT>` signal that an inner, nested `TRANSACTION` raises:

```
TRANSACTION
    SET ^A=1
    TRANSACTION
        SET ^B=2
    END TRANSACTION
    SET ^C=3
END TRANSACTION
```

Do not rely on this as a stable, intentional feature. Treat it as an
accident of how Phase matches a nested `END TRANSACTION`, not as a
documented guarantee about savepoint behavior. `PhaseDB.Transaction`
itself still has no notion of a savepoint. A "nested" transaction is not
a savepoint. It is simply two sequential transaction frames whose bodies
happen to sit next to each other in the source.

## 3. Standard runtime errors

For anything a condition handler does not catch, Phase falls back to
plain Java-exception-shaped errors. This is a 76-baseline behavior, kept
as Phase's default. The `<TAG>` bracket is a cosmetic, diagnostic
convention. Nothing pattern-matches on it in code, except the condition
system described above. See Appendix A for the full table.

---

# Part IV, String Tools

## 1. String literals

Phase supports only M-style quote-doubling for escaping. `""` inside a
double-quoted literal is an escaped literal `"`. Phase has no backslash
escape convention.

```
WRITE "she said ""hi""",!
```

## 2. Interpolation

```
SET X=5
WRITE "X is {X}",!        ; -> X is 5
```

`{expr}` embeds any expression's value directly inside a string literal.
This is front-end sugar only. The tokenizer recognizes a `{...}` run
inside a string. The parser lowers the whole literal into the same `_`
(concatenation) chain you would get by writing it out by hand. No
interpreter change and no value-model change are involved. A doubled
brace, `{{` or `}}`, produces a literal `{` or `}`.

### 2.1 Value formatting inside interpolation

```
SET total=1234.5
WRITE "Total: {total:9(6)V99}",!     ; -> Total: 001234.50
WRITE "Order {id:9(8)}",!            ; -> Order 00000042
```

An optional `:mask`, placed right after the interpolated expression,
routes the value through `PicFormat`, a COBOL-style PICTURE formatter,
before Phase splices the value in. Here is the mask grammar:

| Symbol        | Meaning                                                                     |
|-----------------|--------------------------------------------------------------------------|
| `9`           | one digit position                                                          |
| `X`           | one text position (any mask containing `X` runs in text mode)               |
| `V`           | an assumed decimal point (printed as a literal `.`)                         |
| `(n)`         | repeats the preceding `9`, `X`, or `V` code `n` times; `9(6)` means six digits |
| anything else | a literal character, copied through as-is (for example, the `-` in `9(4)-99-99`) |

```
PicFormat.format("9(8)", "42")            → "00000042"
PicFormat.format("9(4)-99-99", "20240115") → "2024-01-15"
PicFormat.format("9(6)V99", "1234.56")     → "001234.56"
PicFormat.format("X(5)", "AB")             → "AB   "
```

A value that does not fit the mask renders as all `#`, for the mask's
full width. Phase does not truncate the value silently.

This composes with slice syntax at no extra cost:
`{name[1:5]:X(5)}` slices, then formats, in one expression.

## 3. Slice syntax

```
SET X="HELLO"
WRITE X[2:4],!         ; -> ELL
```

`s[start:end]` lowers directly to `$EXTRACT(s, start, end)`. A
single-index form, `s[i]` with no colon, lowers to `$EXTRACT(s, i, i)`.
Phase tells this apart from M's `[` "contains" operator by adjacency. No
space before `[` always means slicing. With a space before `[`, Phase
reads it as slicing only when a `:` appears before the closing `]`.

## 4. `FOR EACH ... IN`

```
SET ^LIST(1)="A",^LIST(2)="B"
FOR EACH K IN ^LIST DO
. WRITE K,!
```

This is pure parser sugar. The parser lowers it, at parse time, into
exactly the classic `$ORDER` idiom:

```
SET K = ""
FOR
SET K = $ORDER(^LIST(K))
QUIT:K=""
```

This works against a plain local, a subscripted local, or a global
reference as the collection.

---

# Part V, Namespaces

```
NAMESPACE Billing.Invoices

TOTAL(orders,taxRate)
    ...
```

A `NAMESPACE X.Y` declaration is a compile-time-only directive. It is a
no-op at runtime. It prefixes every tag defined later in the same
routine. `TOTAL` above registers in the routine's tag map as
`Billing.Invoices.TOTAL`.

**An unqualified call inside the same namespace resolves automatically.**
A bare `DO TOTAL` or `GOTO TOTAL`, written inside a routine that declared
`NAMESPACE Billing.Invoices`, first tries `Billing.Invoices.TOTAL`. If
that fails, Phase falls back to a bare `TOTAL` lookup.

**Implementation note.** The design document shows an *explicit*,
qualified cross-namespace call:

```
CALL Billing.Invoices.TOTAL(orders, 0.19)
```

**This does not actually parse.** The tokenizer has no support for a dot
inside a name, outside `NAMESPACE`'s own hand-written parsing of its
declaration line. `DO`, `GOTO`, and `CALL` each read a tag reference as a
single `NAME` token, so a literal `.` right after `Billing` is an
unrecognized character. Today, the *only* way a namespace-qualified tag
resolves is the automatic same-namespace fallback described above. Phase
has no syntax to target a tag in a namespace other than the caller's own.

**A global name may contain dots, but only as decoration.**
`^Billing.Invoices.ORDERS` is a legal global name. `PhaseDB`'s
`GLOBAL_PAT` accepts `.` and `_` as interior characters. This carries **no
meaning at all**: `^Billing.Invoices.ORDERS` and
`^BillingInvoicesOrders` are equally flat as far as storage cares.
`NAMESPACE` does **not** prefix a global reference. A bare `^ORDERS`
inside a `NAMESPACE Billing.Invoices` block stays `^ORDERS`. Phase keeps
this asymmetry on purpose: routine and tag organization is a call-graph
concern, while a global's name is data identity, and data identity should
never depend on which `NAMESPACE` block happens to surround a reference to
it in the source.

---

# Part VI, Dynamic References & Macros

## 1. `REF(...)`: dynamic reference construction

A subscript is already an ordinary expression, evaluated at the point of
use. `SET ^ORDERS(id, fieldName) = value` already works with no special
syntax, since `fieldName` is just a runtime value plugged into a shape
the parser already knows. Phase's `@` indirection would have additionally
let the **global or local name itself** be dynamic. `REF(...)` covers
that case instead, with no string re-evaluation involved:

```
SET gname = "^ORDERS"
SET REF(gname,"1","STATUS") = "SHIPPED"
WRITE ^ORDERS("1","STATUS"),!          ; -> SHIPPED
WRITE REF(gname,"1","STATUS"),!      ; -> SHIPPED (read position, too)

SET lname = "ITEM"
SET REF(lname,"3") = "widget"
WRITE ITEM(3),!                        ; -> widget

KILL REF(gname,"1","STATUS")         ; works for KILL as well
```

The interpreter evaluates `name` and every subscript, then decides local
versus global from the evaluated `name` string itself. A leading `^`
(which Phase strips) means Phase resolves the name as a global.
Otherwise, Phase resolves it as a local, using the same dispatch rule
the rest of the language already uses. `name` may be any expression, not
just a bare local. For example, `REF(prefix_suffix, ...)` concatenates
two locals into a global name at runtime.

`REF(...)` works the same way in `SET`, in `KILL`, and in an ordinary
read.

---

# Part VII, Time Travel

Time travel is Phase's flagship extension beyond baseline M: reading a
global as it existed at a past transaction or timestamp.

## 1. Syntax

```
^[50]ORDERS(id)                          ; as of transaction 50
^["2024-01-01 00:00:00"]ORDERS(id)       ; as of this date/time
^[TX]ORDERS(id)                          ; bracket contents are a full Phase expression
```

The bracket sits directly after the global's `^` sigil. Phase requires
**zero-width adjacency**: `^[` with nothing between them. Otherwise,
Phase parses a following `[` as the ordinary "contains" operator instead.
The bracket expression's runtime type decides dispatch. A value that
parses as a plain number is treated as a **transaction id**. Anything
else is parsed as an ISO-8601 date and time, and treated as a
**timestamp cutoff**. A value that fits neither reading throws
`<SYNTAX>`.

```
SET ^ORDERS(1)="first"
SET ^ORDERS(1)="second"
NEW TX SET TX=1
WRITE "as of tx1: ",^[TX]ORDERS(1),!     ; -> first
WRITE "live: ",^ORDERS(1),!               ; -> second
```

`$TX` is a Phase-specific intrinsic, not standard M. `$TX` returns the
current or last-committed transaction id, exactly the value the `[tx]`
bracket form expects. This makes time travel usable and testable from
Phase code, with no need to reach into storage internals.

## 2. Rules to know

- **Read-only.** `SET ^[50]ORDERS(id)=...` throws `<SET ON READONLY>`.
  Phase has no "rewrite history" behavior.
- **No new "undefined" concept.** A time-traveled read of something that
  did not exist yet, at that point, behaves exactly like reading an
  undefined live reference. `$DATA` reports `0` (or `10`, if descendants
  existed by then). A bare read throws `<UNDEFINED VARIABLE>`.
- **No block-scoping for repeated snapshots.** Bind the marker to a local
  instead, as the `TX` example above does. Do not expect a scoping
  construct that does not exist.
- **`$ORDER` through a time-traveled reference is not implemented.** It
  throws `<NOTIMPL> time-traveled $ORDER is not yet implemented`. `$DATA`
  and a plain read through time travel both work. Iteration over a
  historical subscript level does not work yet.

---

# Part VIII, Bulk Data Movement: `MERGE` and Materializing Joins

## 1. `MERGE`: copying a whole subtree

`SET` copies one value. `KILL` deletes a subtree. Phase had no single
command to copy a whole subtree. `MERGE` fills this gap.

```
MERGE target=source
MERGE target1=source1,target2=source2
MERGE:postcondition target=source
```

`target` and `source` each name a local or a global. They may mix
freely:

```
MERGE snap=^CUSTOMERS(1)         ; global into local
MERGE ^BACKUP(3)=^CUSTOMERS(3)   ; global into global
```

`MERGE` reads every live value under `source`, including `source`'s own
value if it has one, and writes each one to the matching position under
`target`. `MERGE` does **not** clear `target` first. A value already
present under `target` stays, unless `source` supplies a new value for
that exact position. To replace a subtree completely, run `KILL target`
before the `MERGE`.

`MERGE` reads through the same subtree readers `WRITE TREE` already uses:
`PhaseDB.dumpSubtree` for a global side, and `Environment.subtreeValues`
for a local side. `MERGE` writes through the same paths an ordinary `SET`
already uses: `assignGlobalPath` for a global target, `env.set` for a
local target. This means index maintenance and transaction staging happen
for a `MERGE` exactly as they would for the same sequence of individual
`SET` statements, written by hand. `MERGE` is not a shortcut around either
mechanism.

`MERGE` from a `source` that does not exist copies nothing. This is not
an error. This one fact gives you an outer join with no extra keyword: run
one unconditional `MERGE` for the row you always want, then a separate,
gated `MERGE` for the data you are not sure exists.

## 2. Materializing a join with `FOR EACH`

A join reads two related pieces of data and stores them together. Phase
builds this from `FOR EACH` and `MERGE`, run together on one line:

```
FOR EACH oid IN ^ORDERS
.  MERGE report(oid)=^ORDERS(oid)
.  MERGE report(oid,"CUSTOMER")=^CUSTOMERS(^ORDERS(oid,"CUSTOMER"))
.  DO
```

Each pass of the loop copies one order's own data, then copies that
order's customer data, into one shared tree, keyed by the order ID. Place
`DO` at the very end of the line, not before either `MERGE`. A `DO`
followed immediately by a word tries to call a routine tag with that
name. `DO MERGE` does not do what it looks like it does.

To filter which rows produce an entry at all, add the same postcondition
to every `MERGE` line for that row:

```
FOR EACH oid IN ^ORDERS
.  MERGE:^ORDERS(oid,"STATUS")="SHIPPED" shipped(oid)=^ORDERS(oid)
.  MERGE:^ORDERS(oid,"STATUS")="SHIPPED" shipped(oid,"CUSTOMER")=^CUSTOMERS(^ORDERS(oid,"CUSTOMER"))
.  DO
```

To keep the row, but filter only the joined data, gate only the second
`MERGE`:

```
FOR EACH oid IN ^ORDERS
.  MERGE london(oid)=^ORDERS(oid)
.  MERGE:^CUSTOMERS(^ORDERS(oid,"CUSTOMER"),"CITY")="London" london(oid,"CUSTOMER")=^CUSTOMERS(^ORDERS(oid,"CUSTOMER"))
.  DO
```

Every order still gets a row. Only an order whose customer lives in
London gets the `"CUSTOMER"` data under that row.

No new keyword governs a join. `FOR EACH`, `MERGE`, and the `:`
postcondition already existed on their own. A join is a plain combination
of these three, nothing more.

---

# Part IX, Blob Storage

**Content-addressed storage** stores a file once and identifies it by a
hash of its own bytes, not by a name a person picks. Two uploads of the
same bytes always produce the same hash, so Phase stores those bytes only
once. Phase's blob storage uses this method.

## 1. Writing and reading a blob

`$BLOBWRITE` reads a local file from disk, stores it, and returns a
pointer string.

```
SET ^ORDERS(1,"INVOICE")=$BLOBWRITE("invoice.pdf","invoice.pdf","bob","application/pdf")
```

The arguments are a file path, an optional file name to record, an
optional uploader name to record, and an optional content type to
record. Only the file path is required.

The returned value always starts with the text `BLOB:`, followed by the
file's hash. This prefix marks the value as a blob pointer, not an
ordinary string. Store this value in a global or a local, exactly as you
would store any other value:

```
SET ^ORDERS(1,"INVOICE")=$BLOBWRITE("invoice.pdf")
WRITE ^ORDERS(1,"INVOICE"),!
```

To store a file Phase has already stored, under a different name or by a
different uploader, run `$BLOBWRITE` again with the new file's path.
Phase checks the new file's hash first. If that hash matches a blob
already stored, Phase adds the new file name and the new uploader to
that blob's own record, instead of storing the bytes a second time.

`BLOBSAVE` writes a stored blob's bytes back out to a file on disk:

```
BLOBSAVE ^ORDERS(1,"INVOICE") TO "invoice-copy.pdf"
```

## 2. Reading and setting blob facts

Four functions read facts about a blob. Each one takes a blob pointer or
a bare hash string.

| Function                | Returns                                                                                                 |
|-------------------------|---------------------------------------------------------------------------------------------------------|
| `$BLOBSIZE(hash)`       | The stored size in bytes, or `""` if the blob does not exist.                                           |
| `$BLOBEXISTS(hash)`     | `"1"` if the blob exists, `"0"` if it does not.                                                         |
| `$BLOBMETA(hash,field)` | One field's value: `filenames`, `uploaders`, `contentType`, `size`, `note`, `firstSeen`, or `lastSeen`. |

`filenames` and `uploaders` each return every recorded name, joined with
commas, since one blob may carry more than one name of each kind.

`BLOBMETA ... SET` changes one or more fields on a stored blob:

```
BLOBMETA ^ORDERS(1,"INVOICE") SET note="Reprinted for billing dispute"
```

You may set `note`, `contentType`, `filenames`, or `uploaders`. If you
set `filenames` or `uploaders`, Phase replaces the whole list with your
new, comma-separated list. Phase does not add your values to the existing
list.

---

# Part XI, Storage & Concurrency Internals

## 1. Architecture, in one pass

```
source text
    │
    ▼
Tokenizer.tokenize(line)         one logical line → List<Token>
    │
    ▼
Parser.parseLine(tokens, ...)    tokens → Stmt.Line (AST)
    │
    ▼
RoutineBuilder.build(name, src)  groups multi-line source into logical
                                  lines (line continuation, Part I §5.5),
                                  strips dot-block markers, builds the
                                  tag→line-index map, resolves HANDLE/
                                  CATCH/EVALUATE marker lines into their
                                  final node shapes, produces one
                                  Stmt.Routine
    │
    ▼
Interpreter.run(routine)         tree-walks the flat line list against one
                                  Environment (locals) and one PhaseDB
                                  handle (globals), via a pcLine/pcStmt
                                  program counter
```

The AST is a deliberately flat `List<Line>`, not a nested-block tree.
`IF`/`FOR` hold no body at all. `GOTO`/`DO TAG` resolve to line indices.
This design lets a future bytecode VM consume the AST directly, with no
"un-flattening" pass first.

## 2. Storage, WAL, and transactions

- **Phase stores every value as a plain string, with no codec.** The
  storage layer performs no type-aware encoding. `MValue` handles every
  numeric interpretation at the point of use, never at rest.
- **Every write goes through a write-ahead log**, with periodic
  checkpoint snapshots that hold only the *latest* revision per path.
  Full history lives in the WAL's commit entries. This is what makes
  time travel possible at all: a historical read walks the revision
  chain, rather than using a separate storage mechanism.
- **`PhaseDB.Transaction`** provides real multi-change atomicity.
  `set`/`pointKill`/`killSubtree` stage a set of changes. `commit()`
  checks every staged `expectedTx` against the live value (a
  first-writer-wins, optimistic-concurrency check). If anything fails
  that check, Phase raises a write-conflict error, rather than
  committing part of the change set. `rollback()`, or an unclosed
  `Transaction` closing itself automatically, discards every staged
  change with no write at all.

## 3. Locking: advisory, non-recursive, and a known race

`LockTable` implements `LOCK`/`UNLOCK` with less power than you might
expect from "locking" in a database with real row or table locks:

- **Advisory.** Holding a lock has *no* effect on `get`, `set`,
  `pointKill`, or `killSubtree`. Each of these proceeds regardless of who
  holds what lock. `LOCK` is only a cooperative signal between processes
  that choose to check it.
- **Non-recursive and path-granular.** Locking `^ORDERS(id)` does not
  lock, block, or even notice `^ORDERS(id,"PRICE")`. Phase treats these
  as independent keys. This follows directly from path-granular storage.
  It is one of the actual reasons Phase uses that storage model, not an
  accident of it.
- **Reentrant per owner thread.** A second `acquire` call, by the same
  thread on the same path, increases a depth counter, rather than
  deadlocking against itself.

**Known, accepted race:** `killSubtree` never checks the lock table at
all. A process holding a fine-grained lock on a descendant path can have
its write silently erased by an unrelated subtree kill, or the reverse
can happen. Phase accepts this on purpose. It matches how real production
MUMPS systems behave, and the project documents it as accepted behavior,
not as a bug to patch casually.

---

# Part XII, Intrinsic Function Reference

## Intrinsic variables (no parentheses)

| Name       | Behavior                                                                                                                   |
|------------|----------------------------------------------------------------------------------------------------------------------------|
| `$TEST`    | The boolean result of the most recent `IF` or postcondition test.                                                          |
| `$HOROLOG` | `"days-since-1840-12-31,seconds-since-midnight"`, the current local time.                                                  |
| `$JOB`     | The owning Java thread's id, as a string.                                                                                  |
| `$TX`      | **Phase extension.** The current or last-committed transaction id, exactly the value a `[tx]` time-travel bracket expects. |

Any other bare `$NAME` throws `<UNDEFINED VARIABLE> $NAME is not
implemented`.

## Function-call intrinsics

| Function                                               | Signature                  | Behavior                                                                                                                                    |
|--------------------------------------------------------|----------------------------|---------------------------------------------------------------------------------------------------------------------------------------------|
| `$DATA(ref)`                                           | 1 arg                      | Four-state existence check (Part I §6).                                                                                                     |
| `$ORDER(ref)`                                          | 1 arg, must be subscripted | Next sibling subscript in collation order, or `""`. Time-traveled refs: `<NOTIMPL>`.                                                        |
| `$GET(ref[,default])`                                  | 1–2 args                   | Safe read; never throws on undefined; returns `default` (or `""`) instead. Works for locals, subscripted locals, and time-traveled globals. |
| `$LENGTH(str)`                                         | 1 arg                      | Plain string length, no delimiter form.                                                                                                     |
| `$EXTRACT(str[,start[,end]])`                          | 1–3 args                   | 1-indexed, inclusive substring. `start` defaults to 1; `end` defaults to `start`.                                                           |
| `$PIECE(str,delim[,which])`                            | 2–3 args                   | Splits on a literal delimiter; `which` defaults to 1 (1-indexed).                                                                           |
| `$INDEXLOOKUP(indexName, value)`                       | 2 args                     | **Phase extension.** Comma-separated list of primary keys under a declared secondary index (Part IX).                                       |
| `$FMT(mask, value)`                                    | 2 args                     | **Phase extension.** PICTURE-style formatting (Part IV §2.1), the non-interpolated form of the same mechanism `{expr:mask}` sugars over.    |
| `$BLOBWRITE(path[,filename[,uploader[,contentType]]])` | 1–4 args                   | **Phase extension.** Stores a local file as a blob; returns a `BLOB:`-prefixed pointer (Part XI §1).                                        |
| `$BLOBSIZE(hash)`                                      | 1 arg                      | **Phase extension.** Stored byte size, or `""` (Part XI §2).                                                                                |
| `$BLOBEXISTS(hash)`                                    | 1 arg                      | **Phase extension.** `"1"` or `"0"` (Part XI §2).                                                                                           |
| `$BLOBMETA(hash,field)`                                | 2 args                     | **Phase extension.** One metadata field (Part XI §2).                                                                                       |

**Explicitly stubbed** (Phase recognizes each one, but each one throws
`<NOTIMPL>`): `$TEXT`, `$TRANSLATE`, `$JUSTIFY`, `$SELECT`.

**Never mentioned at all** (no stub; each one falls to
`<UNDEFINED FUNCTION>`): `$FIND`, `$CHAR`, `$ASCII`, `$RANDOM`, `$STACK`,
`$VIEW`, `$QUERY`, `$ZDATE`, and other later-dialect intrinsics.

Phase rejects a `$$` extrinsic function call outright at parse time, with
its own dedicated error message. Phase places `$$` calls out of scope for
the 76 baseline; Phase has not merely left them unimplemented.

---

# Part XIV, Using the REPL

```
phase# SET X=5 WRITE X,!
5
phase# SET ^ORDERS(1,"PRICE")=100
phase# WRITE $DATA(^ORDERS(1)),!
10
phase# ^ORDERS(1,"PRICE")
- : 100
```

- **A tag needs column 0**, exactly like real M routine source. A line
  that starts right at column 0 defines a tag. Anything else runs as an
  ordinary statement. If you forget this, and type an unrecognized bare
  word at column 0, the parser treats it as a tag attempt instead.
- **The REPL evaluates and echoes a bare expression automatically**, in
  the style of a language toplevel. Take a line that is not a recognized
  statement, but that stands entirely on its own as one complete Phase
  expression. The REPL prints that expression's value with a `- : `
  prefix, instead of showing a `<NOTIMPL>` or `<SYNTAX>` error. See
  `^ORDERS(1,"PRICE")` above; it needed no `WRITE`. This never shadows a
  real command: the ordinary statement pipeline always runs first, so
  `QUIT`, `NEW`, a tag definition, and a registered MACLIB macro all
  behave exactly as this handbook documents everywhere else.
- **A multi-line block construct auto-continues.** A bare `DO`,
  `TRANSACTION`, or `EVALUATE` keeps prompting (`     > `) for more input,
  until you type its matching `END DO`, `END TRANSACTION`, or
  `END EVALUATE`:
  ```
  phase# TRANSACTION
       > SET ^A=1
       > SET ^B=2
       > END TRANSACTION
  phase#
  ```
  A bare `DO` (or `FOR EACH ... DO`), followed right away by a
  `.`-prefixed line, switches the REPL to the classic dotted-block style
  instead; the REPL does not expect an `END DO` in that case. This
  continuation ends the moment you enter a non-dot line, which in
  practice means pressing Enter once more on an empty line, once the
  dotted body is finished. An unterminated string literal, or an
  unbalanced `(` or `[`, also keeps the REPL prompting, on its own,
  independent of the block rule above. A trailing `\` still lets you
  force one more line of input manually, in the REPL session itself, for
  anything these input-continuation rules do not cover. This REPL
  input-continuation rule is separate from the source-file line
  continuation described in Part I §5.5; the two solve different
  problems and use different characters.
- `.quit`/`.exit` close the database cleanly and exit the REPL. `.help`
  lists every dot command. `.vars` lists every local currently defined,
  and its bare value. `.globals [prefix]` lists top-level global names.
  `.tags` lists every tag defined so far in the session. `.macros` lists
  every registered MACLIB macro. `.reset` clears locals and the
  session's own routine source, leaving the database itself untouched.
  `.load <path>` runs a Phase source file into the session, as one
  chunk. `.time [on|off]` toggles printing elapsed time after each
  submitted chunk. `.history [n]` shows recent input. `.clear` clears the
  screen.
- A parse error, meaning bad syntax that also fails as a standalone
  expression, is **not** committed into the session's running source. A
  typo does not permanently block every later line. A runtime error on an
  otherwise-valid statement **is** committed: the line was valid, and it
  only failed at execution, the same way a real M routine line can error
  yet still exist in the routine afterward.
- Ctrl-C aborts the current input, even a multi-line input in progress,
  without exiting the REPL. Ctrl-D exits the REPL, the same way it does
  in most REPLs and shells.

---


# Part XVI, The HTTP Server

PhaseDB ships a standalone HTTP server, `phasedb-server`, that exposes
the database over a JSON/REST API. All data access goes through Phase
commands, so there is one authorization path. The server owns the
database file exclusively while it runs: neither the REPL nor another
server instance may open the same file at the same time.

## 1. Starting the server

```
phasedb-server --db phase.db --port 8484
```

The server binds `127.0.0.1` by default. Change the address with
`--listen`:

```
phasedb-server --db phase.db --listen 0.0.0.0 --port 8484
```

Binding to a non-loopback address without TLS requires
`--allow-insecure-basic` (see §2). The server will refuse to start
without it.

The database file is created on first run if it does not exist. On
startup the server takes an exclusive lock on `<db>.lock` for its
lifetime. If another process already holds that lock the server exits
immediately. The REPL takes the same lock, so the two cannot race on
the same file.

## 2. TLS

TLS is built in via `warp-tls`. Provide PEM files at startup:

```
phasedb-server --db phase.db \
               --tls-cert /etc/phasedb/server.crt \
               --tls-key  /etc/phasedb/server.key
```

TLS 1.2 or newer is required. Without `--tls-cert`/`--tls-key` the
server speaks plain HTTP and will only accept `Basic` authentication
from a loopback connection, unless `--allow-insecure-basic` is given
(not recommended for production).

A reverse-proxy setup is also valid: terminate TLS at the proxy, pass
plain HTTP to `127.0.0.1:8484`, and set `--allow-insecure-basic` so
that the server accepts credentials from the proxy.

## 3. CORS

CORS is off by default. Enable it for a specific origin:

```
phasedb-server --db phase.db --cors-origin https://app.example.com
```

Wildcard origins with credentials are never permitted.

## 4. Authentication

### 4.1 Roles

Every user account has a role: `none`, `read`, `write`, or `admin`.
`none` cannot make any API calls. `admin` is the highest role; `read`
is the lowest useful one. Per-global denylists work exactly as in the
REPL.

The server re-reads the caller's role and denylist on every request
(decision D8 in the spec). A role change or account deletion takes
effect on the very next request, even inside a running session.

### 4.2 Basic authentication (one-shot)

Send `Authorization: Basic base64(username:password)` with the
request. The request runs with fresh locals in the `_GLOBAL` namespace
and leaves no session behind. Locks taken during the request are
released when it finishes. PBKDF2-SHA256 runs on every one-shot call;
clients that call frequently should log in once and use a session
token.

### 4.3 Session tokens (Bearer)

Log in once:

```
POST /v1/sessions
Content-Type: application/json

{ "username": "alice", "password": "secret" }
```

Response (HTTP 201):

```json
{
  "token": "3f8a…",
  "expiresAt": "2026-10-14T08:00:00Z",
  "idleTimeoutSeconds": 1800,
  "user": "alice",
  "role": "write",
  "namespace": "_GLOBAL"
}
```

Use the token for subsequent requests:

```
Authorization: Bearer 3f8a…
```

A session keeps local variables, the current namespace, held `LOCK`s,
and a unique `$JOB` id between requests. Two concurrent requests for
the same session queue; the second waits at most `queueTimeoutMs`
before getting `503 BUSY`.

Sessions expire after 30 minutes of inactivity (`--session-idle-seconds`,
default 1800) or 12 hours absolute (`--session-max-seconds`, default
43200). Sessions do not survive a server restart.

Inspect the current session:

```
GET /v1/sessions/current
Authorization: Bearer 3f8a…
```

Log out:

```
DELETE /v1/sessions/current
Authorization: Bearer 3f8a…
```

Logout waits for any request already running in that session, then
removes the session and releases its `LOCK`s.

### 4.4 Rate limiting

Repeated login failures trigger per-username and per-source-address
backoff with exponential delay. A `429` response carries a
`Retry-After` header. At most four PBKDF2 verifications run concurrently;
further attempts receive `503`.

## 5. Execute endpoints

### 5.1 One-shot execute

```
POST /v1/execute
Authorization: Basic base64(alice:secret)
Content-Type: application/json

{ "command": "WRITE \"hello\",!" }
```

### 5.2 Session execute

```
POST /v1/sessions/current/execute
Authorization: Bearer 3f8a…
Content-Type: application/json

{ "command": "SET x=x+1\nWRITE x,!" }
```

Both endpoints accept the same request body:

| Field | Required | Description |
| --- | --- | --- |
| `command` | yes | Phase source, one or more lines. Processed the same way as REPL input. |
| `namespace` | no | Overrides the session namespace for this request. Defaults to session namespace or `_GLOBAL`. |
| `timeoutMs` | no | Per-request timeout, capped by the server maximum (default 10 000 ms). |

REPL dot-commands (`.save`, `.compact`, …) are not accepted. Use the
admin endpoints instead.

**Success response:**

```json
{
  "ok": true,
  "output": "hello\n",
  "value": null,
  "lastTx": 42,
  "elapsedMs": 2,
  "requestId": "b1c2d3e4"
}
```

- `output` is everything `WRITE` printed.
- `value` holds the result of a bare expression (`WRITE` sets it to
  `null`); the REPL's `- : value` echo.
- If `command` registers column-0 routine tags the response includes
  `"registered": ["NS.ROUTINE"]` instead of `output`/`value`.

**Phase-level failure (HTTP 200, `ok: false`):**

```json
{
  "ok": false,
  "output": "",
  "error": {
    "code": "DIVIDE BY ZERO",
    "message": "<DIVIDE BY ZERO>",
    "condition": null
  },
  "lastTx": 42,
  "elapsedMs": 1,
  "requestId": "b1c2d3e4"
}
```

Phase errors are `ok: false` at HTTP 200, they are language-level
results, not transport errors (decision D1).

**Transport-level errors** (the request never ran):

| HTTP | `code` | Cause |
| --- | --- | --- |
| 400 | `BAD_REQUEST` | Malformed JSON, missing field, wrong type |
| 401 | `UNAUTHENTICATED` | Unknown user or wrong password (indistinguishable) |
| 403 | `FORBIDDEN` | Role too low for this endpoint |
| 408 | `REQUEST_TIMEOUT` | Client too slow sending the body |
| 413 | `TOO_LARGE` | Body over the limit |
| 429 | `RATE_LIMITED` | Too many login failures |
| 503 | `BUSY` | Session lock or engine lock timeout |

**Server-added error codes** (returned as Phase-level `ok: false`):

| Code | Meaning |
| --- | --- |
| `TIMEOUT` | `timeoutMs` exceeded |
| `OUTPUT LIMIT` | Output exceeded `--max-output-bytes` |
| `FILE ACCESS DISABLED` | Phase code called `BLOBSAVE`/`$BLOBWRITE` with a filesystem path inside an API session |

## 6. Admin endpoints

All admin endpoints require admin role (Bearer or Basic). A non-admin
gets `403 FORBIDDEN`. A well-formed request that the engine refuses
returns HTTP 200 with `ok: false`.

Every successful write operation (`checkpoint`, `compact`, `users`)
follows the same commit protocol as an execute request: it takes the
engine lock, applies its change against the latest committed state,
flushes to the WAL, and publishes the new state before responding.

### 6.1 Status

```
GET /v1/admin/status
```

Response:

```json
{
  "ok": true,
  "lastTx": 1234,
  "sessionCount": 3,
  "fileSizeBytes": 20480,
  "version": "0.1.0.0",
  "config": {
    "maxBodyBytes":       { "value": 1048576, "source": "default" },
    "execTimeoutMs":      { "value": 5000,    "source": "flag"    },
    "sessionIdleSeconds": { "value": 1800,    "source": "config"  }
  },
  "requestId": "b1c2d3e4"
}
```

`source` is `flag`, `config` (from `^_CONFIG("server", …)`), or
`default`. `status` reads the committed snapshot and never waits for
the engine lock.

### 6.2 Checkpoint

```
POST /v1/admin/checkpoint
```

Appends a WAL checkpoint record. Non-destructive: no data is removed
and `lastTx` is unchanged. Use this to bound recovery time on a busy
database. Failure code: `CHECKPOINT FAILED`.

### 6.3 Compact

```
POST /v1/admin/compact
Content-Type: application/json

{ "beforeTx": 500 }
```

The state as of transaction 500 becomes the new baseline. Individual
transactions up to 500 are discarded; time travel to before 500 is
permanently gone from the database file. Transactions after 500 are
kept verbatim.

The engine lock is held for the entire compaction. Reads continue on
the previous snapshot; writers queue, and those waiting longer than
`queueTimeoutMs` receive `503 BUSY`.

Before touching the file the server writes a full backup to
`<db>.backup-<timestamp>`. The backup path is logged but never returned
to clients and is never deleted automatically.

Success: `{ "ok": true, "lastTx": 500, "requestId": "…" }`.
Failure (e.g. `beforeTx` exceeds `lastTx`): `{ "ok": false, "error": { "code": "COMPACT FAILED", … } }`.

### 6.4 Add user

```
POST /v1/admin/users
Content-Type: application/json

{ "username": "bob", "password": "hunter2", "role": "write" }
```

Valid roles are `none`, `read`, `write`, `admin`. Any other value is
`400 BAD_REQUEST`. The password is hashed with PBKDF2-SHA256 and a
fresh random salt before the engine lock is taken, so hashing does not
block writers. Under the lock the server checks that the username does
not already exist; two concurrent requests for the same name give one
success and one `USER EXISTS`. Passwords are never logged or echoed.

## 7. Blob endpoints

Blobs are binary objects stored inside the database file and
content-addressed by their SHA-256 hash. They are immutable once
written. The blob store is accessible through HTTP only; the globals
`^_BLOBS` and `^_BLOB` cannot be modified with `SET` or `KILL`, even
by an admin.

### 7.1 Upload

```
PUT /v1/blobs
Authorization: Bearer 3f8a…          (write role required)
Content-Type: image/png
X-Phase-Filename: photo.png           (optional)

<raw bytes>
```

Optional query parameter: `?ref=^GLOBAL(sub1,sub2)`. When present, the
server sets that node to the blob pointer in the same WAL transaction
as the blob write. The caller must have write access to `ref`'s global.

Response:

```json
{
  "ok": true,
  "pointer": "blob:d5adb33ff…",
  "hash": "d5adb33ff…",
  "size": 4096,
  "requestId": "b1c2d3e4"
}
```

The server hashes the body incrementally, so memory usage is bounded
by the streaming window, not the blob size. Uploading the same bytes
twice is idempotent.

Limit: `--max-blob-bytes` (default 10 MiB). Bodies exceeding this
receive `413 TOO_LARGE`.

### 7.2 Download by reference

```
GET /v1/blobs?ref=^MYDATA("avatar")
Authorization: Bearer 3f8a…          (read role required)
```

The server reads `^MYDATA("avatar")` with the caller's credentials.
The value must be a `blob:` pointer (e.g. `blob:d5adb33ff…`); otherwise
`400 BAD_REQUEST`. The node must be readable by the caller, a node
on the caller's denylist is `403 FORBIDDEN`, a missing node is `404`.

Response body is the raw bytes. `Content-Type` and `Content-Length`
are set from the stored metadata.

`HEAD /v1/blobs?ref=…` returns the same headers with an empty body.

### 7.3 Download by hash (admin only)

```
GET /v1/blobs/{hash}
Authorization: Bearer admin-token…
```

Direct lookup by hex SHA-256 hash. Requires admin role (`403` otherwise).
Returns `404` if the hash is not in the store.

`HEAD /v1/blobs/{hash}` returns headers only.

### 7.4 Blob references in Phase code

A blob pointer is a string of the form `blob:<hex-sha256>`. Store one
in an array node:

```
SET ^PROFILE("alice","avatar") = "blob:d5adb33ff…"
```

Retrieve the blob via `GET /v1/blobs?ref=^PROFILE("alice","avatar")`.

The `^_BLOBS` global holds server-managed metadata (size, content
type, filename) for every stored blob. It is readable and writable by
admins through the admin status endpoint only; `SET`/`KILL` against
`^_BLOBS` or `^_BLOB` is rejected at the language level with
`<PROTECT>`.

### 7.5 Blob GC

Compaction (`POST /v1/admin/compact`) garbage-collects blobs. A blob
is kept if:

1. Its hash appears as the value of any live global node at the
   compaction baseline, **or**
2. Any WAL record after the baseline references its hash (a blob
   uploaded before compaction but referenced by a future write).

Blobs that satisfy neither condition are permanently deleted from the
compacted file. The backup taken before compaction retains them.

### 7.6 File I/O disabled in API sessions

Phase statements that read or write the server's filesystem
(`BLOBSAVE`, `$BLOBWRITE` with a path argument) are disabled in API
sessions. Calling them returns `ok: false` with code
`FILE ACCESS DISABLED`. Use the `PUT /v1/blobs` endpoint to upload
files instead.

## 8. Configuration reference

Settings are resolved in this order (decision D9):

1. Command-line flag or its `PHASEDB_*` environment variable.
2. `^_CONFIG("server", name)` in the database.
3. Built-in default.

A flag overrides the database value. Database values are re-read on
each request, so an admin can change a limit with `SET` and it applies
from the next request without a restart, unless a flag pins it.

`GET /v1/admin/status` reports each effective value and its source.

| Flag | Config key | Default | Notes |
| --- | --- | --- | --- |
| `--db PATH` | - | `phase.db` | Startup only |
| `--listen ADDR` | - | `127.0.0.1` | Startup only |
| `--port N` | - | `8484` | Startup only |
| `--tls-cert PATH` | - | off | Startup only |
| `--tls-key PATH` | - | off | Startup only |
| `--cors-origin ORIGIN` | - | none | Startup only |
| `--allow-insecure-basic` | - | off | Startup only |
| `--max-body-bytes N` | `maxBodyBytes` | 1 048 576 | Max JSON body |
| `--exec-timeout-ms N` | `execTimeoutMs` | 10 000 | Default timeout per execute request |
| `--exec-timeout-max-ms N` | `execTimeoutMaxMs` | 60 000 | Hard ceiling a client can request |
| `--max-output-bytes N` | `maxOutputBytes` | 1 048 576 | WRITE output cap |
| `--session-idle-seconds N` | `sessionIdleSeconds` | 1 800 | Idle session timeout |
| `--session-max-seconds N` | `sessionMaxSeconds` | 43 200 | Absolute session lifetime |
| `--max-sessions N` | `maxSessions` | 256 | Concurrent session cap |
| `--queue-timeout-ms N` | `queueTimeoutMs` | 5 000 | Max wait for engine/session lock |
| `--max-blob-bytes N` | - | 10 485 760 | Max blob upload size |

Settings in the first group (`--db`, `--listen`, `--port`, etc.) are
read before the database opens and cannot come from `^_CONFIG`.

## 9. Request tracing

Every response carries `X-Request-Id`. If the client supplies this
header (1–64 visible ASCII characters, no spaces) it is echoed back;
otherwise the server generates one. The request id also appears in the
response body (`"requestId"`) and in the access log.

The access log is JSON lines written to stderr:

```json
{
  "requestId": "b1c2d3e4",
  "user": "alice",
  "method": "POST",
  "path": "/v1/sessions/current/execute",
  "status": 200,
  "elapsedMs": 4,
  "bytes": 128
}
```

Passwords, tokens and `Authorization` headers are never logged.
Command text is logged only at debug level.

## 10. Graceful shutdown

The server catches `SIGTERM` (Linux/macOS) and `Ctrl-C`
(`UserInterrupt`, works on Windows too). On shutdown:

1. The server stops accepting new connections.
2. In-flight requests are given a bounded window to finish.
3. The file lock is released.

A hard kill (`SIGKILL`) skips the shutdown sequence but leaves the
database file intact. On next startup the WAL recovery routine trims
any torn trailing record and any transaction whose commit never
reached disk.


# Part XIV, What Phase does not implement yet

This part exists so nobody spends an afternoon "fixing" something that is
actually a known, deliberate gap:

- **The `?` pattern-match operator.** The parser accepts it, but
  `MValue.matchesPattern` unconditionally throws `<NOTIMPL>`. Phase has
  built no repeat-count, alternation, or literal pattern grammar yet.
- **`@` indirection.** The tokenizer and parser accept it
  (`Expr.Indirect`), so surrounding syntax does not choke on it, but
  evaluating it throws `<NOTIMPL>`. It is not coming back in its classic
  string-eval form. `REF(...)` (Part VI §1) and MACLIB (Part VI §2) are
  its intended, narrower replacements.
- **Time-traveled `$ORDER`.** Reading and `$DATA` through an `[asOf]`
  reference both work. Iterating a historical subscript level does not
  work (Part VII §2).
- **Path-shape `EVALUATE` clauses.** Phase defers this, pending a
  decision on whether Phase ever adds a value kind beyond plain strings
  (Part II §5).
- **List and array literals.** Phase has not resolved this at all. It
  shares the same underlying blocker as path-shape `EVALUATE` clauses.
- **True lexical closures over shared mutable state.** Only
  value-snapshot closures exist (Part II §3), by deliberate design.
- **True resumption and continuations** in the condition system. Only
  retry-the-block exists (Part III §1).
- **Explicit cross-namespace qualified calls** (`CALL Other.Namespace.TAG
  (...)`). The grammar for this does not exist. Only same-namespace
  automatic fallback resolution works (Part V).
- **`<TRAVERSAL-LIMIT>` and `<CONFLICT>` as fully structured conditions.**
  `<TRAVERSAL-LIMIT>` is not routed through the catchable condition
  mechanism at all yet. `<CONFLICT>`'s fields are a single best-effort
  `detail` string, rather than the structured `global`, `path`,
  `expectedTx`, and `actualTx` fields the design document describes.
- **Index `CORRUPT` status.** Phase never assigns it. A crash mid-backfill
  leaves an index at `BUILDING` indefinitely, rather than Phase detecting
  this automatically.
- **MACLIB's `txId`-keyed cache invalidation, `^MACLIB(id,"DESC")`, and
  `LIST MACLIB HISTORY`.** None of these exist. The macro registry is a
  plain in-memory table, synced only by `DEFINE MACRO`/`DROP MACRO`
  themselves (Part VI §2).
- **`TRANSACTION` nesting restrictions.** The design document says Phase
  forbids nesting. The parser does not actually enforce this (Part III
  §2).
- **A reusable, named join definition.** `MERGE` (Part X §1) and a
  materializing `FOR EACH`/`MERGE` join (Part X §2) both exist. A way to
  declare a join once, under a name, and re-run it later without
  retyping its clauses, does not exist yet.

---

# Appendix A, Error reference

| Tag                    | Thrown when                                                                                                                                                                                                                                                                                                                                                                        |
|------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `<UNDEFINED VARIABLE>` | A bare read of a local or global finds no value at that exact node, and no handler is catching `<UNDEFINED>`.                                                                                                                                                                                                                                                                      |
| `<UNDEFINED LABEL>`    | `DO`/`GOTO` targets a tag not in the routine's tag map.                                                                                                                                                                                                                                                                                                                            |
| `<UNDEFINED FUNCTION>` | Code calls an unrecognized `$FUNCTION(...)`.                                                                                                                                                                                                                                                                                                                                       |
| `<DIVIDE BY ZERO>`     | `/`, `\`, or `#` runs with a zero divisor, and no handler is catching `<DIVIDE-BY-ZERO>`.                                                                                                                                                                                                                                                                                          |
| `<SUBSCRIPT>`          | For example, `$ORDER` is called against a bare, unsubscripted local.                                                                                                                                                                                                                                                                                                               |
| `<SYNTAX>`             | Phase throws this for a parse-time grammar error, and for a wide range of runtime shape errors: a wrong argument count for an intrinsic, an invalid `SET`/`KILL` target, `CALL`/`&ref` misuse, an `EVALUATE`/`WHEN` arity mismatch, a macro argument-count mismatch, `RETRY` outside a `HANDLE` clause, an undeclared `SIGNAL` field, or an unparseable time-travel bracket value. |
| `<SET ON READONLY>`    | `SET` targets a time-traveled (`^[...]`) global reference.                                                                                                                                                                                                                                                                                                                         |
| `<RESERVED GLOBAL>`    | `SET`/`KILL` targets `^MACLIB` directly.                                                                                                                                                                                                                                                                                                                                           |
| `<INDEX>`              | `$INDEXLOOKUP`/`REBUILD INDEX` references an undeclared index name.                                                                                                                                                                                                                                                                                                                |
| `<TRAVERSAL-LIMIT>`    | A graph traversal (`PATH`/`SHORTEST`) exceeds its `LIMIT`. `HANDLE`/`CATCH` cannot catch this today.                                                                                                                                                                                                                                                                               |
| `<NOTIMPL>`            | Code runs an unsupported statement, `@` indirection, time-traveled `$ORDER`, the `?` pattern operator, or one of the explicitly stubbed intrinsics. Check Part XV before assuming this is a bug.                                                                                                                                                                                   |
| `<INTERNAL>`           | A defensive "this should not happen" branch runs: an unknown AST node, a malformed operator enum, or `RETRY`/`HANDLE`/`CATCH` reached with no enclosing frame. Report this if you see it.                                                                                                                                                                                          |
| `<IO ERROR>`           | An `IOException` surfaces from the storage layer, during a write, an index operation, or a macro operation.                                                                                                                                                                                                                                                                        |

Catchable **conditions** (`<CONFLICT>`, `<LOCK-TIMEOUT>`, `<UNDEFINED>`,
`<DIVIDE-BY-ZERO>`, `<INDEX-BUILD-FAILED>`) form a separate, structured
mechanism, layered on top of several rows in this table. See Part III §1.

---

# Appendix B, Operator precedence quick reference

Loosest to tightest:

```
&  !                          logical AND / OR (not short-circuiting)
=  <  >  [  ]]  ?  ('-negatable)   comparison / contains / sorts-after / pattern-match
_                              string concatenation
+  -                           additive
*  /  \  #                     multiplicative / integer-divide / modulo
**                             exponentiation (right-associative)
-  +  '  @                     unary prefix (highest)
```

`s[start:end]` slice syntax binds tighter than every operator above, as a
postfix wrap directly on the value that comes before it.