# Gerbil Shell (gsh) — Road to Bash Feature Parity

This plan covers every gap between the current gsh implementation and full bash compatibility.
Each work item includes the affected file(s), a description of what needs to change, and a
rough size estimate (S/M/L/XL). Items are grouped by priority tier and ordered within
each tier by dependency.

---

## Current State (as of 2026-02-06)

**22 modules, ~8,500 LOC, builds and runs.**

Working: basic interactive shell with prompt, emacs-mode line editing, command execution
(builtins + external), variables, control flow (if/for/while/until/case/select),
functions, globbing, arithmetic `$((...))`, heredocs/herestrings, command substitution,
quoting, history persistence, tab completion framework.

~~Known broken: multi-command pipelines (fd plumbing hangs), output redirection leaks.~~
**Tier 0 COMPLETE** (2026-02-06): pipelines, redirects, eval, source/., command substitution all fixed.
**Tier 1 COMPLETE** (2026-02-06): variable scoping, trap execution, declare/typeset,
readonly enforcement, set -e (errexit), subshell isolation, job control (process groups,
Ctrl-Z, fg/bg/jobs/wait) all done.
**Tier 2 IN PROGRESS** (2026-02-06): substring expansion (2.9) done. Indirect expansion
(2.10) code written but has a bug — `${!x}` returns empty at runtime despite correct-looking
code. Debug prints in the `!` branch don't trigger. Remaining: brace expansion (2.2),
`(( ))` command (2.5), arithmetic for-loop (2.4), arrays (2.1), `[[ ]]` (2.3), etc.

---

## Tier 0 — Broken Core ~~(fix before anything else)~~ DONE

These are features that nominally exist but don't work correctly.

### 0.1 Fix pipeline fd plumbing
**Files:** `pipeline.ss`
**Size:** M
**Problem:** Multi-command pipes (`cmd1 | cmd2 | cmd3`) hang. The current approach spawns
threads that copy between fdopen ports and Gambit process ports character-by-character.
**Fix:** Use `open-process` with `stdin-redirection: #f` / `stdout-redirection: #f` and
wire the pipe fds directly via the process settings. For builtins/functions in a pipeline,
use `parameterize` on `current-input-port`/`current-output-port` with fdopen ports.
Ensure all pipe ends are closed in the parent after handing off.
**Test:** `echo hello | cat | wc -c` → `6`, `ls | head -5`, `cat /etc/passwd | grep root | cut -d: -f1` → `root`.

### 0.2 Fix output redirection
**Files:** `redirect.ss`, `executor.ss`
**Size:** S
**Problem:** `ls > /dev/null` still prints to stdout. Redirections are applied via
`apply-redirections` but the fd manipulation doesn't properly redirect Gambit's
`current-output-port`.
**Fix:** After `ffi-dup2`, also `parameterize` `current-output-port`/`current-input-port`
with fresh ports wrapping the new fds. Ensure `redirections->process-settings` is used
for external commands and `apply-redirections` + parameterize for builtins.
**Test:** `ls > /tmp/out && cat /tmp/out`, `echo hi 2>/dev/null`, `echo err >&2`.

### 0.3 Wire up `eval` builtin
**Files:** `builtins.ss`, `executor.ss`
**Size:** S
**Problem:** `eval` is a stub returning 0. It needs an executor callback.
**Fix:** Pass `execute-input` (or a similar callback) to the builtin registry at init time.
`eval` concatenates its arguments with spaces and calls `execute-input`.
**Test:** `eval echo hello` → `hello`, `cmd="ls -la"; eval $cmd`.

### 0.4 Add `source` / `.` builtin
**Files:** `builtins.ss`, `script.ss`
**Size:** S
**Problem:** No way to source shell scripts. `source` and `.` are not registered.
**Fix:** Register both names. Implementation: read file, pass each line to `execute-input`
(or use `source-file!` from `script.ss` which already exists). Handle `$1`..`$N` argument
passing.
**Test:** `echo 'X=42' > /tmp/t.sh && source /tmp/t.sh && echo $X` → `42`.

### 0.5 Fix command substitution to use gsh, not /bin/sh
**Files:** `expander.ss`
**Size:** S
**Problem:** `$(cmd)` and `` `cmd` `` fork `/bin/sh -c` instead of executing through gsh's
own parser/executor. This means gsh-specific features (functions, variables, aliases) are
invisible inside command substitutions.
**Fix:** Replace the `open-process` call with internal `execute-input` in a subshell
context (fork a thread, capture its stdout via a pipe, return collected output). Fall back
to `/bin/sh -c` only if internal execution fails.
**Test:** `f() { echo inner; }; echo $(f)` → `inner`.

---

## Tier 1 — Essential for Daily Use — DONE

Features that any bash user would notice immediately if missing.

### ~~1.1 Job control~~ DONE
**Files:** `jobs.ss`, `executor.ss`, `signals.ss`, `ffi.ss`, `builtins.ss`
**Size:** XL
**Completed:** Full job control framework including process groups, Ctrl-Z, fg/bg.

**Sub-tasks:**

#### 1.1a Process group creation — DONE
- `launch-background-simple` calls `ffi-setpgid(pid, pid)` after `open-process`
  to put background commands in their own process group.
- Prevents background commands from receiving SIGINT/SIGTSTP from the terminal.

#### 1.1b Foreground process group switching — DONE
- `job-foreground!` calls `ffi-tcsetpgrp` to give the job's process group
  control of the terminal, then reclaims it after the job completes or stops.

#### 1.1c SIGCHLD reaping — DONE
- SIGCHLD handler in `signals.ss` pushes "CHLD" to pending queue.
- `process-traps!` in main loop calls `job-update-status!` (polls with
  `ffi-waitpid(WNOHANG | WUNTRACED)`) then `job-notify!` to print notifications.

#### 1.1d Background PID tracking — DONE
- `launch-background` and `launch-background-simple` in `executor.ss` capture PIDs
  from `open-process` for external commands and register them in the job table.
- `$!` is set to the background PID.
- `ast->command-text` extracts display text from AST for job listings.

#### 1.1e `fg`/`bg`/`jobs`/`wait` integration — DONE
- `jobs`: lists table with status polling, cleans up completed jobs.
- `wait`: blocks on `process-status`, directly marks process status (bypasses
  Gambit's SIGCHLD reaping race), cleans up completed jobs. Supports PID-based lookup.
- `fg`/`bg`: implemented in builtins, call `job-foreground!`/`job-background!`.
- `job-foreground!` uses `wait-for-foreground-process` for WUNTRACED support.
- `job-wait` properly extracts exit status from raw waitpid status.

#### 1.1f Ctrl-Z (SIGTSTP) handling — DONE
- `wait-for-foreground-process` polls with `ffi-waitpid(WNOHANG | WUNTRACED)`
  with exponential backoff (1ms to 50ms) to detect stopped processes.
- Falls back to `process-status` when Gambit reaps child first (ECHILD).
- `execute-external` detects stopped processes and adds them to job table
  as "stopped", prints `[N]+ Stopped cmd`.
- `job-foreground!` also uses WUNTRACED-aware wait for re-stop support.

**Test:** `sleep 10 &`, `jobs`, `fg %1`, Ctrl-Z, `bg`, `wait`.

### ~~1.2 Trap execution~~ DONE
**Files:** `signals.ss`, `main.ss`, `executor.ss`, `builtins.ss`
**Size:** M
**Completed:** EXIT/ERR/signal traps wired in REPL loop, `execute-stdin`, `-c` mode,
and script mode. Signal traps execute pending handlers after each command. EXIT trap
fires on shell exit. ERR trap fires on non-zero command status.

### ~~1.3 `local` variable scoping~~ DONE
**Files:** `environment.ss`, `functions.ss`, `builtins.ss`
**Size:** M
**Completed:** `env-set!` walks parent scope chain so assignments to existing variables
update the original scope. New variables go to the root scope. Function calls push/pop
scopes via `env-push-scope`.

### ~~1.4 `declare`/`typeset` attributes~~ DONE
**Files:** `builtins.ss`, `environment.ss`
**Size:** M
**Completed:** Flags implemented: `-i` (integer with arithmetic evaluation via
`*arith-eval-fn*` parameter), `-u` (uppercase), `-l` (lowercase), `-x` (export),
`-r` (readonly), `-n` (nameref), `-p` (print). `shell-var` struct extended with
`integer?`, `uppercase?`, `lowercase?`, `nameref?` fields. `apply-var-attrs` applies
transformations on assignment. `-a`/`-A` (arrays) deferred to Tier 2.1.

### ~~1.5 Readonly enforcement~~ DONE
**Files:** `environment.ss`
**Size:** S
**Completed:** `env-set!` checks `shell-var-readonly?` and raises an error. `env-unset!`
also checks readonly. `declare -r` and `readonly` builtin set the flag.

### ~~1.6 `set -e` (errexit) enforcement~~ DONE
**Files:** `executor.ss`, `control.ss`, `functions.ss`, `script.ss`, `main.ss`, `parser.ss`
**Size:** M
**Completed:** `check-errexit!` in executor.ss raises `errexit-exception` on non-zero
status when `errexit` is active and not in condition context. `*in-condition-context*`
parameter suppresses errexit in if-test, while/until-test, `&&`/`||` LHS, and `!` prefix
(via `parameterize`). Caught in script mode, REPL, and `-c` mode. Parser fixed to
recognize `BANG` token type (not just WORD) for `!` prefix.

### ~~1.7 Subshell execution~~ DONE
**Files:** `executor.ss`, `environment.ss`, `functions.ss`, `builtins.ss`
**Size:** M
**Completed:** `env-clone` deep-copies all environment hash tables and positional vector.
`execute-subshell` clones env, saves/restores CWD, catches `subshell-exit-exception`
and `errexit-exception`. `*in-subshell*` parameter makes `exit` builtin raise exception
instead of terminating. Variables, functions, aliases, options, CWD all properly isolated.

---

## Tier 2 — Bash Script Compatibility

Features needed to run typical bash scripts.

### 2.1 Array variables
**Files:** `environment.ss`, `expander.ss`, `builtins.ss`
**Size:** XL
**Description:** No array support at all. This is the single biggest script compatibility gap.

**Sub-tasks:**

#### 2.1a Indexed arrays
- `declare -a arr`, `arr=(a b c)`, `arr[0]=x`
- `${arr[i]}` — element access
- `${arr[@]}` / `${arr[*]}` — all elements (with correct quoting)
- `${#arr[@]}` — array length
- `${!arr[@]}` — array indices
- `unset 'arr[i]'` — element removal

#### 2.1b Associative arrays
- `declare -A map`, `map=([key]=val ...)`
- `${map[key]}` — key access
- `${!map[@]}` — all keys

#### 2.1c Array operations in builtins
- `read -a arr` — read into array
- `printf -v arr[i]` — format into array element
- `for x in "${arr[@]}"` — iterate

#### 2.1d Storage
- Extend `shell-var` with a `value-type` field: scalar, indexed-array, assoc-array.
- Indexed arrays: vector storage.
- Assoc arrays: hash-table storage.

**Test:** `arr=(one two three); echo ${arr[1]}` → `two`, `echo ${#arr[@]}` → `3`.

### 2.2 Brace expansion
**Files:** `expander.ss`
**Size:** M
**Description:** `{a,b,c}` and `{1..10}` not implemented. Must run before tilde expansion.

**Sub-tasks:**
- Comma-separated: `{a,b,c}` → `a b c`
- Numeric range: `{1..10}` → `1 2 3 4 5 6 7 8 9 10`
- Numeric range with step: `{1..10..2}` → `1 3 5 7 9`
- Alphabetic range: `{a..z}` → `a b c ... z`
- Nested: `{a,b{1,2}}` → `a b1 b2`
- Preamble/postscript: `pre{a,b}post` → `preapost prebpost`

**Test:** `echo {1..5}` → `1 2 3 4 5`, `echo file.{txt,md}` → `file.txt file.md`.

### 2.3 `[[ ]]` conditional expression
**Files:** `parser.ss`, `executor.ss`, `builtins.ss`
**Size:** L
**Description:** `[[ ]]` is a bash keyword, not a command. Needs special parsing.

**Sub-tasks:**
- Parse `[[ expr ]]` as a compound command in the parser
- Support operators: `-f`, `-d`, `-e`, `-z`, `-n`, `=`, `!=`, `=~`, `<`, `>`, `-eq`, `-ne`, `-lt`, `-gt`, `-le`, `-ge`, `-a`, `-o`, `!`, `&&`, `||`, `( )`
- `=~` regex matching with BASH_REMATCH array
- No word splitting or globbing inside `[[ ]]`
- Pattern matching with `==` (glob, not regex)

**Test:** `[[ -f /etc/passwd && "hello" =~ ^h ]] && echo yes` → `yes`.

### 2.4 Arithmetic for-loop
**Files:** `parser.ss`, `control.ss`
**Size:** S
**Description:** `for ((i=0; i<10; i++))` not parsed.
**Fix:** In parser, detect `for ((` and parse three semicolon-separated arithmetic
expressions. In `control.ss`, add `execute-arith-for` that evaluates init, tests cond,
executes body, evaluates update.
**Test:** `for ((i=0; i<5; i++)); do echo $i; done` → `0 1 2 3 4`.

### 2.5 `(( ))` arithmetic command
**Files:** `parser.ss`, `executor.ss`
**Size:** S
**Description:** `(( expr ))` as a command (returns 0 if non-zero, 1 if zero).
**Fix:** Parse `((` as a compound command. Evaluate the arithmetic expression.
Return 0 if result is non-zero, 1 if zero.
**Test:** `(( 5 > 3 )) && echo yes` → `yes`.

### 2.6 Process substitution
**Files:** `expander.ss`, `ffi.ss`
**Size:** L
**Description:** `<(cmd)` and `>(cmd)` are tokenized but never executed.
**Fix:** Create a pipe (or use `/dev/fd/N`). Fork the command writing to/reading from
one end. Return `/dev/fd/N` (the other end) as the expansion result.
**Test:** `diff <(echo a) <(echo b)`, `cat <(ls)`.

### 2.7 Coproc
**Files:** `parser.ss`, `executor.ss`, `jobs.ss`
**Size:** L
**Description:** `coproc` creates a coprocess with two-way pipe.
**Fix:** Parse `coproc [NAME] command`. Create two pipes, launch command with stdin/stdout
wired to them. Set `${NAME[0]}` (read fd) and `${NAME[1]}` (write fd). Default name: COPROC.
**Test:** `coproc cat; echo hello >&${COPROC[1]}; read line <&${COPROC[0]}; echo $line` → `hello`.

### 2.8 History expansion
**Files:** `history.ss`, `main.ss`
**Size:** L
**Description:** `!!`, `!$`, `!^`, `!N`, `!string`, `!?string?`, `^old^new^` not implemented.
The `history-expand` function exists but is a skeleton.

**Sub-tasks:**
- `!!` — repeat last command
- `!N` — repeat command N
- `!-N` — repeat Nth previous command
- `!string` — most recent command starting with string
- `!?string?` — most recent command containing string
- Word designators: `!:0`, `!:N`, `!:$`, `!:^`, `!:*`, `!:N-M`
- Modifiers: `:h` (head), `:t` (tail), `:r` (remove suffix), `:e` (extension),
  `:s/old/new/`, `:g` (global), `:p` (print without executing)
- Quick substitution: `^old^new^`

**Test:** `echo hello`, then `!!` → `echo hello` → `hello`.

### ~~2.9 Substring expansion~~ DONE
**Files:** `expander.ss`
**Size:** S
**Completed:** Added `:` case in `apply-parameter-modifier` handling offset, offset:length,
negative offsets (from end), negative lengths (trim from end). Added `string-trim-whitespace-str`
helper. `parse-parameter-modifier` already separates `${name:arg}` correctly.
**Test:** `x=hello; echo ${x:1:3}` → `ell`, `echo ${x: -2}` → `lo`.

### 2.10 Indirect expansion — IN PROGRESS (BUG)
**Files:** `expander.ss`
**Size:** S
**Status:** Code written in `expand-parameter-content` (lines 187-193): checks for `!` prefix,
looks up variable name, dereferences second level. **Code looks correct but produces empty
results at runtime.**

**Bug details:**
- `$GSH -c 'x=greeting; greeting=hello; echo "${!x}"'` → `A=` (EMPTY, should be `hello`)
- `$GSH -c 'x=greeting; echo "$x"'` → `greeting` (basic expansion works)
- Debug fprintf added to the `${!x}` code path did NOT trigger, suggesting the code path
  is never reached despite the code looking correct.
- **Root cause hypothesis:** The `expand-parameter-content` function's `#\!` branch
  may not be matching. Possibly the content extracted by `find-matching-brace` is wrong,
  or the `!` character is being consumed/transformed somewhere in the lexer pipeline
  before reaching expander.ss.
- **Next step:** Add debug print at the TOP of `expand-parameter-content` to verify
  what `content` value is actually received. Check if `find-matching-brace` returns the
  right close position. Check if the `${` branch in `expand-dollar` is even entered
  (vs the `$!` special variable branch at line 149).

**Test:** `x=greeting; greeting=hello; echo ${!x}` → `hello`.

### 2.11 Nameref variables
**Files:** `environment.ss`, `builtins.ss`
**Size:** M
**Description:** `declare -n ref=var` makes `ref` an alias for `var`.
**Fix:** Add a `nameref` flag to `shell-var`. When getting/setting a nameref variable,
follow the reference chain.
**Test:** `x=10; declare -n ref=x; echo $ref` → `10`, `ref=20; echo $x` → `20`.

---

## Tier 3 — Extended Bash Features

Features that advanced users and scripts rely on.

### 3.1 Extended globbing (extglob)
**Files:** `glob.ss`, `expander.ss`
**Size:** L
**Description:** `?(pat)`, `*(pat)`, `+(pat)`, `@(pat)`, `!(pat)` patterns.
**Fix:** When `extglob` shopt is enabled, recognize these patterns in glob expansion.
Convert to equivalent regex patterns.
- `?(pat|pat)` — zero or one match
- `*(pat|pat)` — zero or more
- `+(pat|pat)` — one or more
- `@(pat|pat)` — exactly one
- `!(pat|pat)` — anything except

**Test:** `shopt -s extglob; echo !(*.txt)` lists all non-.txt files.

### 3.2 POSIX character classes in globs
**Files:** `glob.ss`
**Size:** S
**Description:** `[[:alpha:]]`, `[[:digit:]]`, `[[:space:]]`, etc. not recognized.
**Fix:** In `glob-pattern->pregexp`, translate `[:class:]` to regex equivalents.
**Test:** `echo [[:upper:]]*` matches files starting with uppercase.

### 3.3 Glob options integration
**Files:** `glob.ss`, `environment.ss`, `expander.ss`
**Size:** M
**Description:** `dotglob`, `nullglob`, `failglob`, `nocaseglob`, `GLOBIGNORE` not honored.

- `dotglob`: include dotfiles in glob results
- `nullglob`: return empty list (not literal pattern) when no match
- `failglob`: error when no match
- `nocaseglob`: case-insensitive glob
- `GLOBIGNORE`: colon-separated patterns to exclude

**Test:** `shopt -s nullglob; echo /nonexistent*` → empty, `shopt -s dotglob; echo *` includes `.hidden`.

### 3.4 `noclobber` (set -C)
**Files:** `redirect.ss`, `environment.ss`
**Size:** S
**Description:** `set -C` should prevent `>` from overwriting existing files. `>|` (clobber)
forces overwrite.
**Fix:** In `apply-single-redirect!`, when operator is `>` and `noclobber` is set, check
`file-exists?` and error if true. Allow `>|` (clobber) to bypass.
**Test:** `set -C; echo x > /tmp/x; echo y > /tmp/x` → error, `echo y >| /tmp/x` → ok.

### 3.5 Here-document improvements
**Files:** `lexer.ss`, `expander.ss`
**Size:** S
**Description:** Here-doc body collection works but edge cases may be off.
- Ensure `<<-` strips leading tabs
- Ensure quoted delimiter (`<<'EOF'`) prevents expansion
- Ensure unquoted delimiter allows `$var`, `$(cmd)`, `` `cmd` `` expansion

**Test:** Verify tab stripping with `<<-`, quoted vs unquoted delimiters.

### 3.6 `select` with custom PS3
**Files:** `control.ss`, `builtins.ss`
**Size:** S
**Description:** `select` works but may not properly use PS3 for its prompt.
**Fix:** Read PS3 from environment (default `#? `), display menu with numbers,
read user choice, set REPLY variable.
**Test:** `PS3="Choose: "; select x in a b c; do echo $x; break; done`.

### 3.7 `mapfile`/`readarray` builtin
**Files:** `builtins.ss`
**Size:** M
**Description:** Read lines from stdin into an array variable.
**Requires:** Array support (2.1).
**Test:** `echo -e "a\nb\nc" | mapfile arr; echo ${arr[1]}` → `b`.

### 3.8 `printf -v` (format into variable)
**Files:** `builtins.ss`
**Size:** S
**Description:** `printf -v varname fmt args...` assigns result to variable instead of printing.
**Test:** `printf -v x "hello %s" world; echo $x` → `hello world`.

### 3.9 `read` improvements
**Files:** `builtins.ss`
**Size:** M
**Description:** Current `read` is basic. Missing features:
- `-a array` — read into array
- `-d delim` — custom delimiter
- `-n nchars` — read exactly N chars
- `-s` — silent (no echo, for passwords)
- `-t timeout` — timeout in seconds
- `-u fd` — read from fd instead of stdin
- `-e` — use readline for input
- `REPLY` variable when no varname given

**Test:** `echo "a:b:c" | IFS=: read x y z; echo $y` → `b`.

### 3.10 Complete `printf` implementation
**Files:** `builtins.ss`
**Size:** M
**Description:** Current printf handles `%s`, `%d`, `%%`, `\n`, `\t`. Missing:
- `%f`, `%e`, `%g` — floating point
- `%o`, `%x`, `%X` — octal/hex
- `%b` — interpret backslash escapes in argument
- `%q` — shell-quoted output
- `%(fmt)T` — strftime date formatting
- Width and precision: `%-10s`, `%05d`
- Argument recycling (repeat format for excess args)

**Test:** `printf "%05d\n" 42` → `00042`, `printf "%x\n" 255` → `ff`.

---

## Tier 4 — Interactive Shell Features

Features for a pleasant interactive experience.

### 4.1 Vi mode line editing
**Files:** `lineedit.ss`
**Size:** XL
**Description:** Vi mode skeleton exists but has no command-mode bindings.

**Sub-tasks:**
- Insert mode ↔ Command mode toggle (Escape / i)
- Motion: `h`, `l`, `w`, `W`, `b`, `B`, `e`, `E`, `0`, `$`, `^`, `f`, `F`, `t`, `T`
- Editing: `x`, `X`, `r`, `R`, `d{motion}`, `dd`, `c{motion}`, `cc`, `D`, `C`, `s`, `S`
- Text objects: `dw`, `diw`, `daw`, `ci"`, `ca(`
- Yank/paste: `y{motion}`, `yy`, `p`, `P`
- Undo: `u`, Ctrl-R (redo)
- History: `j`, `k`, `/pattern`, `?pattern`, `n`, `N`
- Repeat: `.` (dot command)
- Visual mode would be nice but not essential

### 4.2 Completion improvements
**Files:** `completion.ss`, `lineedit.ss`
**Size:** L

**Sub-tasks:**

#### 4.2a `complete`/`compgen` builtins
- Implement `complete` builtin for user-defined completion specs
- Implement `compgen` builtin for generating completions from specs
- Parse all `complete` flags: `-F func`, `-C cmd`, `-W wordlist`, `-G glob`,
  `-A action`, `-o option`, `-P prefix`, `-S suffix`

#### 4.2b COMP_* variables
- Set `COMP_WORDS`, `COMP_CWORD`, `COMP_LINE`, `COMP_POINT`, `COMP_KEY`,
  `COMP_TYPE` before calling completion functions
- Set `COMPREPLY` array from function results

#### 4.2c Completion display
- Multi-column display when multiple matches
- Pager for long completion lists ("Display all N possibilities?")
- Common prefix insertion
- Menu-complete mode (cycle through options)
- Case-insensitive matching option

#### 4.2d Context-aware defaults
- Complete hostnames after `ssh`, `scp`
- Complete pids after `kill`
- Complete options after `--` for known commands

### 4.3 History improvements
**Files:** `history.ss`, `lineedit.ss`
**Size:** M

- `HISTTIMEFORMAT` — store and display timestamps
- `HISTCONTROL=ignoreboth` — already partial, verify
- `HISTFILESIZE` — truncate history file
- Multi-line command storage (continue across newlines)
- History file locking for concurrent sessions
- `history -d N` — delete specific entry
- `history -c` — clear in-memory history
- `history -r/-w/-a/-n` — read/write/append/read-new from file
- Forward search (Ctrl-S) in addition to reverse (Ctrl-R)

### 4.4 `bind` builtin / inputrc
**Files:** `lineedit.ss`, `builtins.ss`
**Size:** L
**Description:** No way to customize key bindings.
**Fix:** Parse `~/.inputrc` at startup. Support `bind` builtin for runtime rebinding.
Support `set` variables: `editing-mode`, `show-all-if-ambiguous`, `completion-ignore-case`,
`mark-symlinked-directories`, `colored-stats`, etc.

### 4.5 Terminal handling improvements
**Files:** `lineedit.ss`, `prompt.ss`
**Size:** M
- SIGWINCH handler to update terminal columns dynamically
- Proper multi-line prompt handling (count visible lines)
- Handle terminal resize during line editing
- Support `\[\]` in PS1 for non-printing character width calculation
- Bracketed paste mode (ESC[200~/ESC[201~)

### 4.6 Prompt improvements
**Files:** `prompt.ss`
**Size:** M
- `PROMPT_COMMAND` — execute command before each prompt
- `PROMPT_DIRTRIM` — trim deep directory paths in `\w`
- `\D{format}` — proper strftime support (currently shows ISO date)
- `\l` — actual terminal device basename
- `\j` — update on job table changes
- Prompt timing: show elapsed time for long commands

---

## Tier 5 — Script Portability & Edge Cases

These ensure real-world bash scripts work unmodified.

### 5.1 `set` options completeness
**Files:** `builtins.ss`, `executor.ss`, `environment.ss`
**Size:** L
**Description:** Many `set -o` options are tracked but not enforced.

| Option | Status | Needed |
|--------|--------|--------|
| `errexit` (-e) | ✅ Enforced | Done (Tier 1.6) |
| `nounset` (-u) | Tracked, not enforced | Error on unset var |
| `pipefail` | Tracked, not enforced | Pipeline returns rightmost non-zero |
| `xtrace` (-x) | Partial (PS4 shown) | Verify all command types traced |
| `noglob` (-f) | Tracked, not enforced | Skip glob expansion |
| `noclobber` (-C) | Tracked, not enforced | Tier 3.4 |
| `allexport` (-a) | Tracked, not enforced | Auto-export all vars |
| `notify` (-b) | Tracked, not enforced | Immediate job notifications |
| `emacs`/`vi` | Tracked | Wire to line editor mode |

### 5.2 `$PIPESTATUS` array
**Files:** `executor.ss`, `pipeline.ss`, `environment.ss`
**Size:** M
**Requires:** Array support (2.1).
**Description:** After a pipeline, `${PIPESTATUS[@]}` should contain the exit status of
each command in the pipeline.
**Test:** `false | true | false; echo ${PIPESTATUS[@]}` → `1 0 1`.

### 5.3 `$BASH_REMATCH` array
**Files:** `builtins.ss`, `environment.ss`
**Size:** S
**Requires:** `[[ ]]` (2.3) and arrays (2.1).
**Description:** After `[[ str =~ regex ]]`, `${BASH_REMATCH[0]}` is the full match,
`${BASH_REMATCH[N]}` is the Nth capture group.

### 5.4 `$FUNCNAME`, `$BASH_SOURCE`, `$BASH_LINENO` arrays
**Files:** `functions.ss`, `environment.ss`
**Size:** M
**Requires:** Array support (2.1).
**Description:** Call stack arrays for debugging. `$FUNCNAME` is set for the current
function but the full stack array is not maintained.

### 5.5 `$SECONDS` and `$RANDOM` improvements
**Files:** `environment.ss`
**Size:** S
- `SECONDS`: already exists but verify it resets on assignment
- `RANDOM`: improve PRNG quality (currently may use Gambit's default)
- `SRANDOM`: non-seeded random (bash 5.1+)

### 5.6 `$LINENO` accuracy
**Files:** `environment.ss`, `parser.ss`, `executor.ss`
**Size:** M
**Description:** `$LINENO` should reflect the current line in the script. Currently may
not track line numbers through the parser/executor.
**Fix:** Store line numbers in AST nodes (token has start-line). Propagate to executor.
Update `LINENO` before each command.

### 5.7 Word splitting edge cases
**Files:** `expander.ss`
**Size:** S
- `"$@"` should produce separate words (one per positional parameter)
- `"$*"` should join with first character of IFS
- Empty `IFS` should prevent all word splitting
- Unquoted `$@` with IFS changes

### 5.8 Alias expansion in scripts
**Files:** `expander.ss`, `builtins.ss`
**Size:** S
**Description:** `shopt -s expand_aliases` should enable alias expansion in non-interactive
mode. Currently aliases may only work interactively.

### 5.9 `exec` builtin
**Files:** `builtins.ss`, `ffi.ss`
**Size:** M
**Description:** `exec` replaces the shell process or applies redirections permanently.
- `exec cmd args...` — replace shell with cmd (needs FFI `execvp`)
- `exec 3>file` — open fd 3 for the rest of the shell session
- `exec <file` — redirect stdin permanently
**Requires:** FFI `execvp` binding.

### 5.10 `command`/`builtin`/`enable` completeness
**Files:** `builtins.ss`
**Size:** S
- `command -p` — use default PATH
- `builtin cmd` — force builtin (bypass functions/aliases)
- `enable -n cmd` — disable a builtin
- `enable cmd` — re-enable a builtin

### 5.11 `getopts` builtin
**Files:** `builtins.ss`
**Size:** M
**Description:** Parse options in shell scripts.
**Test:** `while getopts "a:b" opt; do case $opt in a) echo "a=$OPTARG";; esac; done`.

### 5.12 `time` keyword
**Files:** `parser.ss`, `executor.ss`
**Size:** S
**Description:** `time cmd` or `time -p cmd` prints real/user/sys time.
**Fix:** Parse `time` as a prefix keyword. Wrap execution with timing. Print to stderr.
**Test:** `time sleep 1` → shows ~1s real time.

---

## Tier 6 — Polish & Compliance

### 6.1 Error messages
**Files:** All
**Size:** M
**Description:** Standardize error format to `gsh: context: message` matching bash.
Handle edge cases gracefully (don't dump Scheme stack traces to users).

### 6.2 Exit status consistency
**Files:** `executor.ss`, `builtins.ss`
**Size:** M
**Description:** Verify all exit status codes match bash:
- 0: success
- 1: general error
- 2: misuse of shell command/syntax error
- 126: command found but not executable
- 127: command not found
- 128+N: killed by signal N
- 130: Ctrl-C (SIGINT)

### 6.3 POSIX `sh` mode
**Files:** `main.ss`, `environment.ss`
**Size:** M
**Description:** When invoked as `sh` or with `--posix`, disable bash extensions:
brace expansion, `[[ ]]`, arrays, `source` (use `.` only), etc.

### 6.4 Startup file loading
**Files:** `startup.ss`, `main.ss`
**Size:** M
**Description:** Match bash's startup file logic:
- Login shell: `/etc/profile`, then first of `~/.bash_profile`, `~/.bash_login`, `~/.profile`
- Interactive non-login: `~/.bashrc`
- Non-interactive: `$BASH_ENV`
- Login logout: `~/.bash_logout`
Currently loads `~/.gshrc` only.

### 6.5 Signal-safe child process handling
**Files:** `executor.ss`, `signals.ss`
**Size:** M
**Description:** Before forking a child:
- Block SIGCHLD (prevent race with handler)
- Reset signal dispositions in child (traps cleared)
- Restore signal mask in child
After forking:
- Unblock SIGCHLD in parent

### 6.6 Compilation & packaging
**Files:** `build.ss`, `Makefile`
**Size:** M
- `make install` target
- Release build with `gerbil build --release`
- Man page (gsh.1)
- `--version` / `--help` flags
- Shell can be added to `/etc/shells`
- Packaging for major distros (deb, rpm, nix)

---

## Appendix A: Feature Matrix

| Feature | POSIX sh | bash | gsh (now) | gsh (planned) |
|---------|----------|------|-----------|---------------|
| Simple commands | ✓ | ✓ | ✓ | ✓ |
| Pipelines | ✓ | ✓ | ✓ | ✓ |
| Redirections (basic) | ✓ | ✓ | ✓ | ✓ |
| `&&` / `\|\|` | ✓ | ✓ | ✓ | ✓ |
| `if/elif/else` | ✓ | ✓ | ✓ | ✓ |
| `for VAR in list` | ✓ | ✓ | ✓ | ✓ |
| `for ((;;))` | — | ✓ | ✗ | Tier 2.4 |
| `while/until` | ✓ | ✓ | ✓ | ✓ |
| `case/esac` | ✓ | ✓ | ✓ | ✓ |
| `select` | — | ✓ | ✓ | ✓ |
| Functions | ✓ | ✓ | ✓ | ✓ |
| `local` scoping | — | ✓ | ✓ | ✓ |
| Variables | ✓ | ✓ | ✓ | ✓ |
| Arrays | — | ✓ | ✗ | Tier 2.1 |
| Assoc arrays | — | ✓ | ✗ | Tier 2.1 |
| Tilde expansion | ✓ | ✓ | ✓ | ✓ |
| Parameter expansion | ✓ | ✓ | ~85% | Tier 2.10 (indirect bug) |
| Command substitution | ✓ | ✓ | ✓ | ✓ |
| Arithmetic `$(())` | ✓ | ✓ | ✓ | ✓ |
| Brace expansion | — | ✓ | ✗ | Tier 2.2 |
| Process substitution | — | ✓ | ✗ | Tier 2.6 |
| Globbing | ✓ | ✓ | ✓ | ✓ |
| Extended globs | — | ✓ | ✗ | Tier 3.1 |
| `[[ ]]` | — | ✓ | ✗ | Tier 2.3 |
| `(( ))` | — | ✓ | ✗ | Tier 2.5 |
| Job control | ✓ | ✓ | ✓ | ✓ (Tier 1.1 done) |
| Traps | ✓ | ✓ | ✓ | ✓ |
| `eval` | ✓ | ✓ | ✓ | ✓ |
| `source`/`.` | ✓ | ✓ | ✓ | ✓ |
| `read` | ✓ | ✓ | Basic | Tier 3.9 |
| `test`/`[` | ✓ | ✓ | ✓ | ✓ |
| `declare`/`typeset` | — | ✓ | ✓ (no arrays) | Tier 2.1 |
| `getopts` | ✓ | ✓ | ✗ | Tier 5.11 |
| `exec` | ✓ | ✓ | ✗ | Tier 5.9 |
| `time` | — | ✓ | ✗ | Tier 5.12 |
| Line editing (emacs) | — | ✓ | ~70% | Tier 4.1-4.5 |
| Line editing (vi) | — | ✓ | Skeleton | Tier 4.1 |
| Tab completion | — | ✓ | Basic | Tier 4.2 |
| Programmable completion | — | ✓ | Framework | Tier 4.2 |
| History expansion | — | ✓ | ✗ | Tier 2.8 |
| `set -e` (errexit) | ✓ | ✓ | ✓ | ✓ |
| `set -u` (nounset) | ✓ | ✓ | Tracked only | Tier 5.1 |
| `set -o pipefail` | — | ✓ | Tracked only | Tier 5.1 |
| Coproc | — | ✓ | ✗ | Tier 2.7 |
| `$PIPESTATUS` | — | ✓ | ✗ | Tier 5.2 |
| Nameref | — | ✓ | Partial | Tier 2.11 |
| `exec` redirections | ✓ | ✓ | ✗ | Tier 5.9 |
| Startup files (bash) | — | ✓ | Minimal | Tier 6.4 |

## Appendix B: Dependency Graph

```
Tier 0 (fix broken) ✅ ALL DONE
  ✅ 0.1 Pipelines
  ✅ 0.2 Redirections
  ✅ 0.3 eval
  ✅ 0.4 source/.
  ✅ 0.5 Command substitution (internal)

Tier 1 (daily use) ✅ ALL DONE
  ✅ 1.1 Job control (process groups, Ctrl-Z, fg/bg/jobs/wait)
  ✅ 1.2 Trap execution
  ✅ 1.3 Local scoping
  ✅ 1.4 declare/typeset
  ✅ 1.5 Readonly enforcement
  ✅ 1.6 errexit
  ✅ 1.7 Subshell isolation

Tier 2 (script compat) — IN PROGRESS
  ✅ 2.9 Substring expansion ${VAR:offset:length} — DONE
  🔧 2.10 Indirect expansion ${!VAR} — CODE WRITTEN, BUG (see notes)
  2.1 Arrays ← prerequisite for 2.3, 2.7, 5.2, 5.3, 5.4
  2.2 Brace expansion
  2.3 [[ ]] ← depends on 2.1 (BASH_REMATCH)
  2.4 Arithmetic for-loop
  2.5 (( )) command
  2.6 Process substitution
  2.7 Coproc ← depends on 2.1
  2.8 History expansion
  2.11 Nameref (declare -n flag done, full chain resolution TODO)

Tier 3 (extended) — depends on Tier 2
  3.1 extglob
  3.2 POSIX char classes
  3.3 Glob options
  3.4 noclobber
  3.5-3.10 Various

Tier 4 (interactive) — independent, can parallel Tier 2-3
  4.1 Vi mode
  4.2 Completion
  4.3 History
  4.4 bind/inputrc
  4.5 Terminal handling
  4.6 Prompt

Tier 5 (portability) — depends on Tier 2
  5.1-5.12 Various

Tier 6 (polish) — last
  6.1-6.6 Various
```

## Appendix C: Estimated Effort

| Tier | Items | Status | Estimated LOC | Effort |
|------|-------|--------|---------------|--------|
| 0 | 5 | **DONE** | ~300 (done) | ~~2-3 days~~ |
| 1 | 7 | **DONE** | ~800 (done) | ~~1-2 weeks~~ |
| 2 | 11 | **1 done, 1 in progress** | ~1,500 | 2-3 weeks |
| 3 | 10 | Not started | ~600 | 1 week |
| 4 | 6 | Not started | ~1,500 | 2-3 weeks |
| 5 | 12 | Not started | ~800 | 1-2 weeks |
| 6 | 6 | Not started | ~400 | 1 week |
| **Total** | **57** | **13 done, 1 in progress** | **~5,000 remaining** | **~8 weeks** |

Codebase has grown from ~5,500 to ~8,500 LOC with Tier 0 + most of Tier 1 complete.

---

## Appendix D: Compatibility Testing with Oils Spec Tests

### Overview

The [Oils project](https://github.com/oils-for-unix/oils) maintains a comprehensive
suite of ~200 spec test files (`spec/*.test.sh`) that validate POSIX and bash shell
behavior across multiple shell implementations (bash, dash, mksh, zsh, osh). We adopt
this framework as gsh's primary compatibility test harness via `make compat`.

**Repo:** `https://github.com/oils-for-unix/oils`
**Test runner:** `test/sh_spec.py`
**Test files:** `spec/*.test.sh`

### Test Format

Each `.test.sh` file contains multiple test cases with this structure:

```sh
## compare_shells: bash dash mksh

#### test name
echo hello | tr a-z A-Z
## status: 0
## stdout: HELLO

#### multiline output test
for i in a b c; do echo $i; done
## STDOUT:
a
b
c
## END

#### expected failure
ls /nonexistent
## status: 2
```

Key markers:
- `#### title` — begins a test case
- `## stdout: text` — single-line expected stdout
- `## STDOUT:` / `## END` — multi-line expected stdout
- `## stdout-json: "text\n"` — JSON-encoded expected output (for escapes)
- `## status: N` — expected exit status (default 0)
- `## stderr-json: ""` — expected stderr
- `## OK bash/dash/mksh STDOUT:` — shell-specific expected output variants
- `## compare_shells:` — header listing which shells to compare
- `## tags:` — categorization tags

### Invocation

```sh
# Run one test file against gsh + bash for comparison:
python3 test/sh_spec.py spec/smoke.test.sh /bin/bash /path/to/gsh

# Run with specific test range:
python3 test/sh_spec.py --range 3-5 spec/pipeline.test.sh /bin/bash /path/to/gsh

# Verbose mode (show details for passing tests too):
python3 test/sh_spec.py -v spec/smoke.test.sh /bin/bash /path/to/gsh
```

Shell binaries are positional arguments after the test file path. The runner executes
each test case in each shell, compares output/status against the `##` assertions, and
reports pass/fail per shell.

### Setup

```makefile
OILS_DIR := _vendor/oils

# Clone or update the oils repo for spec tests
$(OILS_DIR):
	git clone --depth 1 https://github.com/oils-for-unix/oils.git $(OILS_DIR)

vendor-update: $(OILS_DIR)
	cd $(OILS_DIR) && git pull --ff-only
```

### Test Tiers for gsh

Not all 200+ spec files are relevant. YSH-specific tests (`ysh-*.test.sh`) are
excluded. The remaining tests are bucketed into tiers matching gsh's implementation
tiers:

#### Tier 0 — Core (should pass now)

These test the features just fixed in Tier 0:

| Spec file | gsh feature |
|-----------|-------------|
| `smoke.test.sh` | Basic commands, pipes, redirects, vars, loops |
| `pipeline.test.sh` | Multi-command pipelines |
| `redirect.test.sh` | Output/input/append redirection |
| `redirect-multi.test.sh` | Multiple redirections on one command |
| `builtin-eval-source.test.sh` | eval, source/. builtins |
| `command-sub.test.sh` | `$(...)` and backtick substitution |
| `comments.test.sh` | Comment handling |
| `exit-status.test.sh` | `$?`, exit codes |

#### Tier 1 — Essential Features

| Spec file | gsh feature |
|-----------|-------------|
| `here-doc.test.sh` | Here-documents |
| `quote.test.sh` | Quoting rules |
| `word-eval.test.sh` | Word expansion |
| `word-split.test.sh` | IFS word splitting |
| `var-sub.test.sh` | Variable substitution |
| `var-sub-quote.test.sh` | Quoted variable substitution |
| `var-num.test.sh` | `$1`, `$#`, `$@`, `$*` |
| `var-op-test.test.sh` | `${x:-default}`, `${x:+alt}`, etc. |
| `var-op-strip.test.sh` | `${x#pat}`, `${x%pat}` |
| `var-op-len.test.sh` | `${#x}` |
| `assign.test.sh` | Variable assignment |
| `tilde.test.sh` | Tilde expansion |

#### Tier 2 — Standard Features

| Spec file | gsh feature |
|-----------|-------------|
| `arith.test.sh` | `$((...))` arithmetic |
| `glob.test.sh` | Pathname expansion |
| `brace-expansion.test.sh` | `{a,b,c}` expansion |
| `case_.test.sh` | `case` statement |
| `if_.test.sh` | `if`/`elif`/`else` |
| `loop.test.sh` | `for`/`while`/`until` |
| `for-expr.test.sh` | C-style `for ((i=0; ...))` |
| `subshell.test.sh` | `(...)` subshell |
| `sh-func.test.sh` | Shell functions |
| `builtin-echo.test.sh` | `echo` builtin |
| `builtin-printf.test.sh` | `printf` builtin |
| `builtin-read.test.sh` | `read` builtin |
| `builtin-cd.test.sh` | `cd` builtin |
| `builtin-set.test.sh` | `set` builtin |
| `builtin-type.test.sh` | `type` / `command` builtins |
| `builtin-trap.test.sh` | `trap` builtin |
| `builtin-bracket.test.sh` | `[` / `test` builtin |
| `builtin-misc.test.sh` | Various builtins |
| `builtin-process.test.sh` | `wait`, `jobs`, `bg`, `fg` |
| `background.test.sh` | `&` background execution |
| `command-parsing.test.sh` | Parser edge cases |

#### Tier 3+ — Extended / Bash-specific

| Spec file | gsh feature |
|-----------|-------------|
| `array-basic.test.sh` | `declare -a` arrays |
| `array-assoc.test.sh` | `declare -A` assoc arrays |
| `dbracket.test.sh` | `[[ ]]` conditional |
| `extglob-match.test.sh` | Extended globbing |
| `regex.test.sh` | `=~` regex matching |
| `process-sub.test.sh` | `<(...)` / `>(...)` |
| `nameref.test.sh` | `declare -n` namerefs |
| `builtin-vars.test.sh` | `declare`, `local`, `export` |
| `var-op-patsub.test.sh` | `${x/pat/rep}` |
| `var-op-slice.test.sh` | `${x:offset:length}` |
| `errexit.test.sh` | `set -e` handling |
| `sh-options.test.sh` | Shell options |
| `xtrace.test.sh` | `set -x` tracing |
| `alias.test.sh` | Alias expansion |
| `builtin-getopts.test.sh` | `getopts` builtin |
| `builtin-umask.test.sh` | `umask` builtin |
| `unicode.test.sh` | Unicode handling |

### Makefile Targets

```makefile
GSH        := $(CURDIR)/.gerbil/bin/gsh
OILS_DIR   := _vendor/oils
SH_SPEC    := python3 $(OILS_DIR)/test/sh_spec.py
BASH       := /bin/bash

# --- Build ---

build:
	gerbil build

# --- Spec test targets ---

# Run the smoke test (quick sanity check)
compat-smoke: build $(OILS_DIR)
	$(SH_SPEC) $(OILS_DIR)/spec/smoke.test.sh $(BASH) $(GSH)

# Run all Tier 0 spec tests
compat-tier0: build $(OILS_DIR)
	@for f in smoke pipeline redirect redirect-multi \
	          builtin-eval-source command-sub comments exit-status; do \
	  echo "=== $$f ==="; \
	  $(SH_SPEC) $(OILS_DIR)/spec/$$f.test.sh $(BASH) $(GSH) || true; \
	  echo; \
	done

# Run all Tier 1 spec tests
compat-tier1: build $(OILS_DIR)
	@for f in here-doc quote word-eval word-split var-sub var-sub-quote \
	          var-num var-op-test var-op-strip var-op-len assign tilde; do \
	  echo "=== $$f ==="; \
	  $(SH_SPEC) $(OILS_DIR)/spec/$$f.test.sh $(BASH) $(GSH) || true; \
	  echo; \
	done

# Run all Tier 2 spec tests
compat-tier2: build $(OILS_DIR)
	@for f in arith glob brace-expansion case_ if_ loop for-expr subshell \
	          sh-func builtin-echo builtin-printf builtin-read builtin-cd \
	          builtin-set builtin-type builtin-trap builtin-bracket \
	          builtin-misc builtin-process background command-parsing; do \
	  echo "=== $$f ==="; \
	  $(SH_SPEC) $(OILS_DIR)/spec/$$f.test.sh $(BASH) $(GSH) || true; \
	  echo; \
	done

# Run a single spec file: make compat-one SPEC=smoke
compat-one: build $(OILS_DIR)
	$(SH_SPEC) $(OILS_DIR)/spec/$(SPEC).test.sh $(BASH) $(GSH)

# Run a specific test range: make compat-range SPEC=smoke RANGE=3-5
compat-range: build $(OILS_DIR)
	$(SH_SPEC) --range $(RANGE) $(OILS_DIR)/spec/$(SPEC).test.sh $(BASH) $(GSH)

# Run all tiers combined
compat: compat-tier0 compat-tier1 compat-tier2

# Verbose single spec: make compat-debug SPEC=smoke
compat-debug: build $(OILS_DIR)
	$(SH_SPEC) -v $(OILS_DIR)/spec/$(SPEC).test.sh $(BASH) $(GSH)

# --- Vendor management ---

$(OILS_DIR):
	git clone --depth 1 https://github.com/oils-for-unix/oils.git $(OILS_DIR)

vendor-update: $(OILS_DIR)
	cd $(OILS_DIR) && git pull --ff-only

clean:
	gerbil build --clean

.PHONY: build clean compat compat-smoke compat-tier0 compat-tier1 compat-tier2 \
        compat-one compat-range compat-debug vendor-update
```

### Workflow

1. **After implementing a feature**, run the corresponding spec tier:
   ```sh
   make compat-tier0          # after Tier 0 fixes
   make compat-one SPEC=glob  # after implementing glob improvements
   ```

2. **To debug a failing test**, isolate it:
   ```sh
   make compat-range SPEC=redirect RANGE=5-5   # run just test #5
   make compat-debug SPEC=redirect              # verbose output
   ```

3. **Track progress** by counting pass/fail per tier over time. The `--tsv-output`
   flag can produce machine-readable results for CI dashboards.

4. **Shell-specific annotations**: When gsh intentionally differs from bash (e.g.,
   extensions or POSIX-only behavior), the spec test format supports
   `## OK gsh STDOUT:` annotations for expected differences.

### Helper Dependencies

Some spec tests use helper scripts from `spec/bin/`:
- `argv.py` — prints `sys.argv` as Python list (tests argument splitting)
- `printenv.py` — prints env vars
- `show_fd_table.py` — shows open file descriptors

These are Python scripts and work as-is. The `$TMP` variable in tests points to
a per-test temp directory managed by `sh_spec.py`.

### Excluded Tests

- `ysh-*.test.sh` — YSH language extensions, not applicable to bash compat
- `ble-*.test.sh` — ble.sh (line editor) specific tests
- `blog*.test.sh` — blog post examples, not canonical
- `*-osh.test.sh` — OSH-specific behavior tests
- `hay*.test.sh` — YSH hay configuration language
