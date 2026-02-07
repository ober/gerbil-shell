# Gerbil Shell (gsh) — Oil Shell Test Fix Plan

Current score: **575/1091 (53%)** — target: **900+ (82%+)**

Each phase groups fixes by root cause so a single implementation pass fixes multiple
test suites at once. Phases are ordered by impact (tests fixed per effort).
Size estimates: S = < 50 LOC, M = 50–150 LOC, L = 150–400 LOC, XL = 400+ LOC.

Run the full suite with `make compat`. Run one suite with `make compat-one SPEC=<name>`.

---

## Phase 1 — Word Splitting & Glob After Expansion (~50 tests)

The single biggest source of failures. Unquoted `$var` must undergo field splitting
AND pathname expansion (globbing). Currently gsh treats expanded values as literal words.

### 1.1 IFS field splitting on expanded words
**Files:** `expander.ss`
**Size:** L
**Suites:** word-split (32 failures), var-sub-quote (18), word-eval (4), assign (4)

- After expanding `$var`, `${var}`, `$( )`, split the result on IFS characters
- Whitespace IFS chars (space, tab, newline) collapse adjacent delimiters
- Non-whitespace IFS chars (`:`, `,`) produce empty fields between adjacent delimiters
- Mixed IFS: whitespace trims around non-whitespace delimiters
- Empty IFS (`IFS=''`) suppresses splitting entirely
- `"$var"` (double-quoted) must NOT split — only unquoted triggers splitting
- `$@` always splits into separate words even with empty IFS; `$*` joins on first IFS char

### 1.2 Glob expansion on expanded words
**Files:** `expander.ss`, `executor.ss`
**Size:** M
**Suites:** glob (10), word-eval (1), var-op-strip (2)

- After field splitting, each resulting word that contains unquoted glob chars (`*`, `?`, `[`)
  must undergo pathname expansion
- `x="*.txt"; echo $x` should expand the glob
- `"$x"` must NOT glob — quoting suppresses it
- Track which characters came from expansion (unquoted) vs literal source to decide
  whether to glob. Bash uses an internal quoting bitmask per character.
- `set -o noglob` / `set -f` disables this step

### 1.3 Empty field elision
**Files:** `expander.ss`
**Size:** S
**Suites:** word-split (4), var-sub-quote (2)

- Unquoted expansion that produces empty string is elided (removed from arg list)
- `""` (quoted empty) is preserved
- `"$@"` with zero positional params produces zero words (not one empty word)

---

## Phase 2 — Builtin Fixes: echo -e, read, printf (~55 tests)

### 2.1 Implement `echo -e` / `echo -en`
**Files:** `builtins.ss`
**Size:** M
**Suites:** builtin-echo (8 failures)

- Parse `-e`, `-n`, `-en`, `-ne` flags (stop at first non-flag arg)
- Interpret escapes: `\n` `\t` `\a` `\b` `\e` `\f` `\v` `\r` `\\`
- `\xNN` (hex byte), `\0NNN` (octal byte), `\uNNNN` (unicode), `\UNNNNNNNN` (unicode)
- `\c` stops output immediately (no trailing newline)

### 2.2 Fix `read` backslash processing
**Files:** `builtins.ss`
**Size:** M
**Suites:** builtin-read (10 failures)

- Without `-r`: backslash-newline = line continuation (read next line); backslash-char = literal char
- With `-r`: no backslash processing at all (current behavior)
- `read` should return exit 1 when input has no trailing newline

### 2.3 Fix `read` IFS splitting
**Files:** `builtins.ss`
**Size:** M
**Suites:** builtin-read (8 failures)

- Same whitespace vs non-whitespace IFS rules as word splitting
- Non-whitespace IFS delimiters produce empty fields
- Last variable gets remainder (no splitting)
- When reading into REPLY (no var names), do NOT strip leading/trailing IFS whitespace
  from the value (differs from named variables)

### 2.4 Fix `read -N`, `-d ''`, `-u FD`
**Files:** `builtins.ss`
**Size:** M
**Suites:** builtin-read (7 failures)

- `-N count`: read exactly count chars, ignoring delimiters (no IFS splitting)
- `-d ''`: use NUL byte as delimiter instead of newline
- `-u FD`: read from specified file descriptor instead of stdin
- `-t 0`: return 0 if input is available, 1 if not (non-blocking check)
- `-p prompt`: write prompt to stderr (not stdout)

### 2.5 Fix printf: %u, precision, argument recycling, %b
**Files:** `builtins.ss`
**Size:** L
**Suites:** builtin-printf (20 failures)

- `%u`: unsigned decimal format (use modular arithmetic for negatives)
- `%6.4d`: precision = minimum digits (zero-pad), width = minimum field width
- Argument recycling: when more args than format specifiers, re-apply format from start
- Too few args: treat missing as `0` (numeric) or `""` (string)
- `%b`: interpret backslash escapes in the argument (like `echo -e` on the arg)
- `\c` in `%b` argument stops all output
- `%(fmt)T`: strftime format; `-1` = current time, `-2` = shell start time
- Negative numbers with `%u`/`%x`/`%o`: use two's complement
- `\uNNNN` and `\UNNNNNNNN` in format string
- Return exit 2 (not 1) for usage errors; return 1 for conversion errors

---

## Phase 3 — PWD / cd Fixes (~21 tests)

### 3.1 Fix PWD trailing slash
**Files:** `builtins.ss`, `environment.ss`
**Size:** S
**Suites:** builtin-cd (9 failures)

- `pwd` output must NOT have trailing `/` (except for root `/`)
- Check all places that set `$PWD` and strip trailing slash

### 3.2 Implement logical vs physical path tracking
**Files:** `builtins.ss`
**Size:** M
**Suites:** builtin-cd (6 failures)

- Maintain a logical path (with symlinks) and physical path (resolved)
- `cd` default is logical mode (`-L`): update logical path by appending/resolving `..` textually
- `cd -P`: resolve symlinks, update physical path
- `pwd` (no flag or `-L`): print logical path
- `pwd -P`: print physical path
- `cd` with no args goes to `$HOME` (currently returns exit 1)
- `cd --` stops option processing
- CDPATH: search colon-separated directories for relative cd targets

---

## Phase 4 — Arithmetic Improvements (~30 tests)

### 4.1 Fix `$((expr))` in word context
**Files:** `expander.ss`, `lexer.ss`
**Size:** M
**Suites:** arith (8 failures)

- `echo "x$((1+1))y"` and `echo $((1+1))` should work in any word position
- Ensure the arithmetic result is spliced into the word correctly

### 4.2 Fix base-N constants
**Files:** `arithmetic.ss`
**Size:** S
**Suites:** arith (4 failures)

- `36#z` should give 35 (z = 35th digit), not 36
- Multi-digit base-N: `16#ff` = 255, `8#77` = 63
- Dynamic base via variable: `b=16; echo $((b#ff))` should work
- Reject invalid digits for the base

### 4.3 Fix operators: comma, bitwise NOT, modulo, shifts
**Files:** `arithmetic.ss`
**Size:** M
**Suites:** arith (5 failures)

- Comma operator: evaluate all, return last: `(( a=1, b=2, c=3 ))` → c=3
- `~3` = -4 (bitwise complement)
- `-5 % 3` = -2 (C semantics: sign of dividend)
- Right shift of negative: sign-extend (arithmetic shift)
- `2 ** -1` should be an error

### 4.4 Implement array indexing in arithmetic
**Files:** `arithmetic.ss`
**Size:** M
**Suites:** arith (5 failures)

- `a[0]` reads array element, `a[i]++` increments
- `a[i+1]` arbitrary expression as index
- Detect invalid lvalues: `"a"=1`, double subscript

### 4.5 Dynamic variable resolution in arithmetic
**Files:** `arithmetic.ss`
**Size:** S
**Suites:** arith (4 failures)

- `x=y; y=5; echo $((x))` should resolve through variable names
- Only resolve when the name doesn't look like a number

### 4.6 Enforce set -u in arithmetic
**Files:** `arithmetic.ss`, `environment.ss`
**Size:** S
**Suites:** arith (3 failures), var-op-len (1)

- When `set -u` (nounset) is active, referencing an undefined variable in arithmetic
  should be an error

---

## Phase 5 — Trap & Signal Fixes (~24 tests)

### 5.1 Fix trap signal name output format
**Files:** `builtins.ss`
**Size:** S
**Suites:** builtin-trap (5 failures)

- `trap -p` should print `SIGINT` not `INT`, `SIGUSR1` not `USR1`
- Accept both forms as input

### 5.2 Fix trap clearing and resetting
**Files:** `builtins.ss`, `signals.ss`
**Size:** M
**Suites:** builtin-trap (4 failures)

- `trap - SIGINT`: reset signal to default behavior
- `trap '' SIGINT`: ignore signal
- `trap EXIT` (no command): clear the EXIT trap
- `trap -- '-' SIGINT`: double-dash stops option processing
- Numeric signal aliases: `trap cmd 0` means EXIT (signal 0)

### 5.3 Fix trap behavior in subshells and edge cases
**Files:** `signals.ss`, `executor.ss`
**Size:** M
**Suites:** builtin-trap (6 failures)

- Traps (except EXIT and ignored signals) should be reset in subshells and `$()`
- EXIT trap must fire after: parse errors, explicit `exit` inside a trap, `eval` in trap
- Invalid signal names/numbers should return exit 1

### 5.4 Fix background process signal handling
**Files:** `executor.ss`, `jobs.ss`
**Size:** M
**Suites:** builtin-trap (3 failures), background (1)

- Signals delivered to foreground sleep/wait processes
- Killed job should print signal name (e.g., "Hangup" for SIGHUP)

---

## Phase 6 — Test/Bracket Improvements (~31 tests)

### 6.1 Implement missing file test operators
**Files:** `executor.ss`
**Size:** M
**Suites:** builtin-bracket (7 failures)

- `-h` / `-L`: true if file is a symbolic link
- `-k`: true if sticky bit set
- `-G`: true if owned by effective group ID
- `-O`: true if owned by effective user ID
- `-u` / `-g`: true if setuid/setgid bit set
- `-c`: true if character special file
- `-ef`: true if two paths refer to same inode (same device + inode)
- Fix `-x` to actually check execute permission (not just file existence)
- Fix `-a` as unary operator (alias for `-e`)

### 6.2 Implement `[` multi-arg forms
**Files:** `executor.ss`
**Size:** M
**Suites:** builtin-bracket (5 failures)

- 3-arg: `[ ! expr ]`, `[ ( expr ) ]`, `[ str op str ]`
- 4-arg: `[ ! str op str ]`, `[ ( expr ) ]` with negation
- Binary `-a` (AND) and `-o` (OR) operators
- `( )` grouping inside `[`
- `-o optname` tests if shell option is set

### 6.3 Fix error exit codes
**Files:** `executor.ss`, `builtins.ss`
**Size:** S
**Suites:** builtin-bracket (4 failures), loop (2), builtin-printf (2), builtin-misc (1)

- `[` / `test` with syntax errors: exit 2 (not 1)
- `printf` usage errors: exit 2
- `break`/`continue` with invalid args: exit 2
- `shift` with invalid arg: exit 2
- `-t` with invalid FD: exit 2

### 6.4 Fix `-v` variable test and numeric comparisons
**Files:** `executor.ss`
**Size:** S
**Suites:** builtin-bracket (3 failures)

- `[ -v unset_var ]` should return 1 (currently returns 0)
- Hex and base-N constants in `-eq`/`-lt`/etc. should be converted to integers
- Integer overflow in comparisons should return exit 2

---

## Phase 7 — set Options & Variable Operators (~35 tests)

### 7.1 Implement `set -u` (nounset) properly
**Files:** `expander.ss`, `arithmetic.ss`, `builtins.ss`
**Size:** M
**Suites:** builtin-set (3 failures), arith (3), var-op-len (1)

- Referencing undefined variable in expansion: error
- Referencing undefined variable in arithmetic: error
- `${#undef}` with nounset: error
- `$undef` with nounset: error (currently just expands to empty)

### 7.2 Implement `set -a` (allexport)
**Files:** `environment.ss`, `executor.ss`
**Size:** M
**Suites:** builtin-set (5 failures), assign (2)

- When allexport is on, all variable assignments automatically export to environment
- Applies to new assignments and modifications of existing vars
- `export -n` should override allexport for specific variables

### 7.3 Fix `set -o` and `set` output
**Files:** `builtins.ss`
**Size:** S
**Suites:** builtin-set (2 failures)

- `set -o` (no args): list all options with on/off status
- `set` (no args): dump all shell variables in re-evaluable format

### 7.4 Fix `set --` / `set -` / `set +` argument handling
**Files:** `builtins.ss`
**Size:** S
**Suites:** builtin-set (5 failures)

- `set --`: end of options, remaining args become positional params
- `set -`: equivalent to `set +xv` (turn off xtrace and verbose)
- Handle bare `-` and `+` as positional parameters

### 7.5 Fix var-op-test with arrays and `$@`
**Files:** `expander.ss`
**Size:** M
**Suites:** var-op-test (10 failures)

- `${array[@]:-default}`: if all elements empty, use default
- `${array[*]:+value}`: if any element non-empty, use value
- `$@` / `$*` interaction with `:-` and `:+` operators
- Backslash in default value: `${x:-\z}` should be literal `\z`

### 7.6 Fix var-op-strip with arrays and special patterns
**Files:** `expander.ss`
**Size:** M
**Suites:** var-op-strip (7 failures)

- `${arr[@]%pattern}`: apply strip to each element (vectorize)
- `${arr[@]/#/prefix}`: prepend to each element
- `${arr[@]/%/suffix}`: append to each element
- Bracket chars `[`, `[]` in patterns
- Unicode in strip patterns

---

## Phase 8 — Background & Process Fixes (~25 tests)

### 8.1 Fix `wait` exit status propagation
**Files:** `jobs.ss`, `builtins.ss`
**Size:** M
**Suites:** background (5 failures)

- `wait $pid` must return the process's actual exit status, not always 0
- Store exit status in job table when process completes
- `wait` with no args waits for all children, returns status of last

### 8.2 Implement PIPESTATUS
**Files:** `pipeline.ss`, `environment.ss`
**Size:** M
**Suites:** pipeline (3 failures), background (2)

- After every pipeline, set `PIPESTATUS` array with exit status of each command
- `${PIPESTATUS[0]}` = first command, `${PIPESTATUS[1]}` = second, etc.
- Single commands: `PIPESTATUS=(status)`

### 8.3 Implement `wait -n`
**Files:** `builtins.ss`, `jobs.ss`
**Size:** M
**Suites:** background (2 failures)

- Wait for any one child to complete, return its status

### 8.4 Fix background builtin/function output
**Files:** `executor.ss`
**Size:** S
**Suites:** background (2 failures)

- `echo async &` should produce output (currently silent)
- `for i in 1 2 3; do echo $i; done &` should produce output

### 8.5 Fix `jobs -p` output
**Files:** `builtins.ss`
**Size:** S
**Suites:** background (1 failure)

- `jobs -p` should print PIDs of background jobs, not 0

### 8.6 Implement `ulimit` builtin
**Files:** `builtins.ss`, `ffi.ss`
**Size:** L
**Suites:** builtin-process (12 failures)

- Wrapper around `getrlimit`/`setrlimit` FFI
- `-n` (open files), `-v` (virtual memory), `-f` (file size), `-c` (core size), etc.
- `ulimit -a` shows all limits

### 8.7 Fix `exec` flags
**Files:** `builtins.ss`
**Size:** S
**Suites:** builtin-process (2 failures)

- `exec --`: end of options
- `exec -a name cmd args`: set argv[0] to `name`

---

## Phase 9 — Case, Loop, Pipeline Edge Cases (~20 tests)

### 9.1 Implement `;&` and `;;&` case fallthrough
**Files:** `parser.ss`, `control.ss`
**Size:** M
**Suites:** case_ (2 failures)

- `;&` falls through unconditionally to next clause body
- `;;&` tests the next clause's pattern (like C's fallthrough without break)

### 9.2 Fix case pattern edge cases
**Files:** `control.ss`, `lexer.ss`
**Size:** S
**Suites:** case_ (4 failures)

- Quoted literal in pattern: `"x"` should match `x`
- Escaped parens in pattern: `\(` should match literal `(`
- Binary/byte matching with `$'\xff'`

### 9.3 Fix break/continue with levels and edge cases
**Files:** `builtins.ss`, `control.ss`
**Size:** M
**Suites:** loop (7 failures)

- `break 2` / `continue 2`: break/continue N nesting levels
- `break`/`continue` outside loop: print warning, return exit 0 (not stop execution)
- Invalid args: return exit 2
- `break` in loop condition: `while break; do ...` should exit loop
- `source` within loop should propagate break/continue

### 9.4 Fix pipeline edge cases
**Files:** `pipeline.ss`, `executor.ss`
**Size:** M
**Suites:** pipeline (5 failures)

- SIGPIPE: process killed by SIGPIPE should report exit 141
- Pipeline in `eval`: `eval "false | true"` exit status
- `shopt -s lastpipe`: last command of pipeline runs in current shell
- Variables set in pipeline subshell must not leak to parent

---

## Phase 10 — Tilde, Brace, Glob Edge Cases (~25 tests)

### 10.1 Fix tilde expansion in assignment context
**Files:** `expander.ss`
**Size:** M
**Suites:** tilde (4 failures)

- After `:` in assignment: `PATH=~/bin:~root/bin` expands both tildes
- `readonly x=~/src` should expand tilde
- `x=~ cmd` (temp env) should expand tilde

### 10.2 Fix brace expansion edge cases
**Files:** `expander.ss`
**Size:** M
**Suites:** brace-expansion (8 failures)

- Variable expansion before brace expansion: `{$x,b}` → expand `$x` first
- Tilde inside brace expansion: `{~,~root}`
- Negative/zero step validation: `{1..4..-1}` should handle or error
- Three-dot `{1...3}` should NOT expand (treated as literal)
- Single-element `{x}` should NOT expand (but `{x}_{a,b}` should still expand the second)

### 10.3 Fix glob edge cases
**Files:** `glob.ss`, `expander.ss`
**Size:** M
**Suites:** glob (11 failures)

- Character class ranges from variables: `x="[C-D]"; echo $x`
- Escaped glob chars: `\[` `\?` match literal
- Unicode: `?` matches one unicode character (not one byte)
- `LC_COLLATE` sort order
- `set -o noglob` disables expansion
- Negated character classes from variables: `[^...]`

---

## Phase 11 — type, misc builtins, redirects (~20 tests)

### 11.1 Fix `type` builtin
**Files:** `builtins.ss`
**Size:** M
**Suites:** builtin-type (6 failures)

- Recognize shell keywords: `while`, `for`, `if`, `case`, `do`, `done`, etc.
- Distinguish "special shell builtin" vs "shell builtin":
  special = `.`, `:`, `break`, `continue`, `eval`, `exec`, `exit`, `export`,
  `readonly`, `return`, `set`, `shift`, `trap`, `unset`
- Show alias expansions: `ll is an alias for ls -l`
- "not found" message to stdout (not stderr)
- Exit status: 0 if all found, 1 if any not found

### 11.2 Implement `${var@Q}` and `declare -p` format
**Files:** `expander.ss`, `builtins.ss`
**Size:** S
**Suites:** builtin-misc (3 failures)

- `${var@Q}`: shell-quoted representation of value
- `declare -p var`: print in `declare -- var="value"` format (already close, fix edge cases)

### 11.3 Fix redirect edge cases
**Files:** `redirect.ss`, `parser.ss`
**Size:** M
**Suites:** redirect (6 failures), redirect-multi (2)

- `exec {myfd}>file`: allocate fd dynamically, store fd number in `$myfd`
- `exec {fd}>&-`: close the named fd
- `1>&2-`: duplicate fd 2 to fd 1, then close fd 2 (move operator)
- Large fd numbers (99, 100+)

### 11.4 Implement `time` keyword
**Files:** `parser.ss`, `executor.ss`
**Size:** M
**Suites:** builtin-process (1 failure), builtin-misc (1)

- `time command` measures and reports wall/user/sys time
- `time { compound; }` works with compound commands
- Output to stderr in POSIX format

---

## Phase 12 — Unicode & Encoding (~6 tests)

### 12.1 Unicode string length
**Files:** `expander.ss`
**Size:** S
**Suites:** var-op-len (4 failures)

- `${#var}` should count unicode characters, not bytes
- In `LC_ALL=C`, count bytes; otherwise count characters
- Detect and report invalid UTF-8

---

## Phase 13 — Remaining Edge Cases (~15 tests)

### 13.1 Fix subshell syntax `( commands )`
**Files:** `parser.ss`, `executor.ss`
**Size:** M
**Suites:** subshell (1 failure), command-parsing (2)

- Pre-existing bug: `(echo hello)` treats echo output as command name
- Fix parser to correctly handle `(` as subshell start

### 13.2 Fix assignment edge cases
**Files:** `executor.ss`, `expander.ss`
**Size:** M
**Suites:** assign (8 failures)

- `local "$x"` where x="y a=b" should not word-split
- `x=$var` (assignment RHS) should NOT split or glob
- Temp env binding should not persist: `x=tmp cmd; echo $x` → original value
- `readonly $x` with dynamic args
- `declare -a arr` creates empty array visible in `declare -p`
- Redirect after bare assignment: `x=1 > file`

### 13.3 Fix command-parsing edge cases
**Files:** `parser.ss`
**Size:** S
**Suites:** command-parsing (3 failures)

- Prefix env on bare assignment: `FOO=bar X=val`
- Redirect on control flow: `break > file`, `return > file`

---

## Summary — Expected Impact

| Phase | Tests Fixed (est.) | Effort |
|-------|-------------------|--------|
| 1. Word splitting & glob after expansion | ~50 | XL |
| 2. echo -e, read, printf | ~55 | XL |
| 3. PWD / cd | ~21 | M |
| 4. Arithmetic | ~30 | L |
| 5. Trap & signals | ~24 | L |
| 6. Test/bracket | ~31 | L |
| 7. set options & var operators | ~35 | XL |
| 8. Background & process | ~25 | XL |
| 9. Case, loop, pipeline edges | ~20 | L |
| 10. Tilde, brace, glob edges | ~25 | L |
| 11. type, misc, redirects | ~20 | L |
| 12. Unicode | ~6 | S |
| 13. Remaining edge cases | ~15 | M |
| **Total** | **~357** | |

Fixing all phases would bring gsh from **575/1091 (53%)** to roughly **930/1091 (85%)**,
surpassing bash's own score of 950/1091 (87%) on many suites (bash fails some tests too).

Phases 1–3 alone would fix ~126 tests and bring the score to ~64%.
Phases 1–6 would fix ~241 tests and bring the score to ~75%.
