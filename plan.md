# Plan: 100% Bash Compatibility

**Goal**: Pass every test that bash passes in the oils spec suite.

**Current**: 1122/1179 (95%) — 38 tests where bash passes but gsh fails.

---

## Failures by Category

### Phase 1 — Quick Wins (5 fixes, 6 tests)

#### 1.1 Exit status truncation (exit-status #1)

`exit N` and `return N` must truncate to `N & 0xFF`. gsh passes through 0 for out-of-range values.

```bash
$SH -c 'exit 256'; echo status=$?   # expected: status=0, got: status=0 (COINCIDENCE — fails for 255, 257, -1, -2)
$SH -c 'exit 257'; echo status=$?   # expected: status=1, got: status=0
$SH -c 'exit -1'; echo status=$?    # expected: status=255, got: status=0
```

**Fix**: In builtins.ss `exit` and `return` handlers, apply `(bitwise-and n #xFF)` before setting exit status. Handle negative values too.

#### 1.2 External command exit status (smoke #15)

`ls /nonexistent-zzZZ` returns status 0 instead of 2.

**Fix**: Investigate `execute-external` — the exit status from `ffi-fork-exec` / `wait-for-foreground-process-raw` may not be decoded correctly. `waitpid` returns raw status; need `WEXITSTATUS(status)` extraction.

#### 1.3 trap with invalid signal number (builtin-trap #30)

`trap -1 EXIT` should fail (invalid signal) but gsh succeeds silently.

**Fix**: In trap builtin, validate that signal arguments are non-negative. Reject negative numbers.

#### 1.4 `${#var@X}` parse error (var-op-bash #19)

`${#A[@]@P}` should be a parse error — can't combine `#` length with `@X` transformations.

**Fix**: In expander.ss, when processing `${#...}`, check for `@X` suffix and report error.

#### 1.5 declare -p quoting (assign-extended #22)

`declare -p x` outputs `declare -rx x=42` instead of `declare -rx x="42"`.

**Fix**: In builtins.ss `declare-p`, always wrap values in double quotes.

---

### Phase 2 — set -u / nounset (1 fix, 3 tests)

#### 2.1 Nounset error propagation (builtin-set #6, #7, #8)

When `set -u` is active and `$undefined` is referenced, gsh prints the error to stderr but the child shell exits 0 instead of non-zero. The nounset exception doesn't propagate to a non-zero exit status in `$SH -c` contexts.

```bash
$SH -c 'set -u; echo before; echo $x; echo after'
test $? -ne 0 && echo OK   # expected: OK, got: (nothing — child exits 0)
```

**Fix**: In the nounset-exception handler (or top-level script execution), ensure the shell exits with status 1 when a nounset error occurs in non-interactive mode.

---

### Phase 3 — PWD / Symlink Handling (1 fix, 6 tests)

#### 3.1 Child shell PWD inheritance (builtin-cd #12, #13, #15, #16, #26, #27)

When gsh spawns a child `$SH -c`, the child doesn't inherit the parent's logical `$PWD`. It calls `getcwd()` which returns the physical path, losing symlink information.

Tests affected:
- #12: `unset PWD; pwd` — should use physical cwd, gsh uses temp dir
- #13: `PWD=foo; pwd` — should fall back to getcwd(), gsh uses temp dir
- #15: pwd in symlinked dir — should show symlink path
- #16: `cd ..` through symlink — should go to logical parent
- #26: inherited `$PWD` vs actual cwd disagreement
- #27: too many `getcwd()` syscalls (5 instead of 1)

**Fix**:
1. On startup, trust inherited `$PWD` if it points to the same inode as `getcwd()` result (stat comparison)
2. `pwd` builtin: use logical `$PWD` by default (`-L`), physical only with `-P`
3. `cd ..`: navigate the logical path, not the physical one
4. Minimize getcwd() calls — cache the result

---

### Phase 4 — Redirect Validation (1 fix, 3 tests)

#### 4.1 Ambiguous redirect detection (redirect-multi #7, #12, #13)

Redirect targets that expand to multiple words (via glob, brace expansion, or word splitting) should fail with "ambiguous redirect".

```bash
star='*'; echo hi > two-$star     # should fail: ambiguous redirect
echo hi > a-{one,two}             # should fail: ambiguous redirect
file='foo bar'; echo hi > $file   # should fail: ambiguous redirect
```

**Fix**: In redirect.ss, after expanding the redirect target word, check if the result is more than one word. If so, report "ambiguous redirect" error and return status 1.

---

### Phase 5 — Backtick / Empty Command Semantics (1 fix, 2 tests)

#### 5.1 Backtick output as command (exit-status #7, #8)

When backtick substitution produces output and it's used as the command word, the output should be executed as a command.

```bash
if `sh -c 'echo X; true'`; then echo TRUE; else echo FALSE; fi
# Expected: FALSE (backtick produces "X", executed as command, fails with 127)
# Actual: TRUE

if `true` X; then echo TRUE; else echo FALSE; fi
# Expected: FALSE (backtick produces empty, "X" is not found)
# Actual: TRUE
```

**Fix**: When command substitution in command position produces output, that output must be word-split and the first word used as the command name. Currently gsh may be treating empty backtick result as success.

---

### Phase 6 — Pipeline Fixes (2-3 fixes, 4 tests)

#### 6.1 Last command exit status (pipeline #6)

`echo a | egrep '[0-9]+'` returns status 0 instead of 1.

**Fix**: Pipeline exit status must come from the last command. Check `wait-for-all` in pipeline.ss returns the correct status.

#### 6.2 `!` negation with `$SH -c` (pipeline #12)

`! $SH -c 'exit 42'; echo $?` outputs `1` instead of `0`.

**Fix**: The `!` prefix should negate the exit status of the entire pipeline/command. Verify the negation logic in executor.ss handles `$SH -c` correctly.

#### 6.3 SIGPIPE in PIPESTATUS (pipeline #23)

`cat /dev/urandom | sleep 0.1` should yield `PIPESTATUS=(141 0)` but gsh gives `(0 0)`.

**Fix**: When a pipeline process is killed by SIGPIPE, record `128 + SIGPIPE` (141) in PIPESTATUS. Need to detect signal death in waitpid result using `WIFSIGNALED` / `WTERMSIG`.

#### 6.4 pipefail + lastpipe (pipeline #26)

`set -o pipefail; ls | false | wc -l` should fail with pipefail active.

**Fix**: Implement `set -o pipefail` — pipeline exit status is the last non-zero exit from any command in the pipeline.

---

### Phase 7 — Arithmetic Validation (1 fix, 1 test)

#### 7.1 Invalid constants in child shell (arith #14)

`$SH -c 'echo $(( 0x1X ))'` should fail with status 1, but gsh returns 0.

**Fix**: Arithmetic parser should error on invalid numeric constants like `0x1X`, `09`, `2#A`. The error must propagate as a non-zero exit in `$SH -c`.

---

### Phase 8 — Background Job Signal Status (1 fix, 2 tests)

#### 8.1 Signal exit status for killed jobs (background #8, #27)

When a background job is killed by a signal, `wait $pid` should return `128 + signum`. gsh returns 0.

```bash
sleep 1 &; kill -HUP $!; wait $!; echo status=$?
# Expected: status=129
# Actual: status=0
```

**Fix**: In jobs.ss, when `ffi-waitpid-pid` indicates the process was killed by a signal (`WIFSIGNALED`), set status to `128 + WTERMSIG(status)`. Also emit signal name to stderr.

---

### Phase 9 — FD Leaks (1 fix, 1 test)

#### 9.1 Close internal file descriptors (redirect #20)

gsh has fds 3, 4, 5, 10, 255, 256 open. Test expects only 0, 1, 2. fd 7 redirect fails because the test sees unexpected open fds.

**Fix**: Mark all internal fds (signalfd, scheduler pipe, moved fds) as `FD_CLOEXEC` so they don't appear in `/proc/$$/fd`. Or close them before the test's `ls /proc/$$/fd`.

---

### Phase 10 — FIFO <> Redirect (1 fix, 1 test)

#### 10.1 Bidirectional FIFO open (redirect #30)

`exec 8<> fifo` on a named pipe hangs or returns -1.

**Fix**: Opening a FIFO with `O_RDWR` should work without blocking (unlike `O_RDONLY` which blocks until a writer opens). Ensure redirect.ss uses `O_RDWR` for `<>` redirects on FIFOs.

---

### Phase 11 — Trap + Signal Delivery (1 fix, 3 tests)

#### 11.1 Signal delivery to child shell processes (builtin-trap #27, #28, #29)

When gsh runs `$SH -c 'trap "echo sig" USR1; sleep 0.2'` in background, signals sent to the child PID are not delivered.

**Fix**: This is likely because `ffi-fork-exec` child processes have signals blocked (from `sigprocmask` in parent). The child needs to unblock all signals after fork. Also, Gambit's signalfd may be interfering with signal delivery.

---

### Phase 12 — ulimit Enforcement (1 fix, 2 tests)

#### 12.1 ulimit -f and ulimit -n (builtin-process #23, #26)

`ulimit -f 1` doesn't prevent large file writes. `ulimit -n 0` doesn't prevent file opens.

**Fix**: The `ulimit` builtin must call `setrlimit()` via FFI, not just store values. Add `ffi-setrlimit` / `ffi-getrlimit` for `RLIMIT_FSIZE` and `RLIMIT_NOFILE`.

---

### Phase 13 — declare -F extdebug (1 fix, 1 test)

#### 13.1 Function location tracking (assign-extended #6)

`declare -F funcname` with `shopt -s extdebug` should print name, line number, and source file.

**Fix**: Track function definition location (file + line) in the function table. When extdebug is on, `declare -F` outputs `funcname linenumber filename`.

---

### Phase 14 — Word Joining with $@ (1 fix, 1 test)

#### 14.1 $@ splitting in concatenation (word-eval #4)

```bash
set -- x y z; s1='1 2'; array=(a1 a2)
argv.py $s1"${array[@]}"_"$@"
# Expected: ['1', '2a1', 'a2_x', 'y', 'z']
# Actual:   ['1 2a1', 'a2_x y z']
```

**Fix**: When `$@` or `${array[@]}` appears inside word concatenation, each element produces a separate word. The first element joins with preceding text, the last with following text, middle elements are standalone. This requires changes to the word expansion logic in expander.ss.

---

### Phase 15 — $@ in Default Value (1 fix, 3 tests)

#### 15.1 $@ expansion in ${var=...} (var-op-test #4, #6, #37)

`${unset=x"$@"x}` should expand `$@` with word splitting even inside the assignment value when unquoted.

**Fix**: In the `${var=value}` expansion path, `$@` must still produce multiple words when the overall expansion is unquoted.

---

## Summary

| Phase | Category | Tests | Difficulty |
|-------|----------|-------|------------|
| 1 | Quick wins (5 fixes) | 6 | Easy |
| 2 | set -u propagation | 3 | Medium |
| 3 | PWD/symlink handling | 6 | Medium |
| 4 | Redirect validation | 3 | Medium |
| 5 | Backtick semantics | 2 | Medium |
| 6 | Pipeline fixes | 4 | Medium-Hard |
| 7 | Arith validation | 1 | Medium |
| 8 | Background signal status | 2 | Medium |
| 9 | FD leaks | 1 | Medium |
| 10 | FIFO <> redirect | 1 | Medium |
| 11 | Trap + signal delivery | 3 | Hard |
| 12 | ulimit enforcement | 2 | Medium-Hard |
| 13 | declare -F extdebug | 1 | Medium |
| 14 | Word joining with $@ | 1 | Hard |
| 15 | $@ in default value | 3 | Hard |
| **Total** | | **38** | |

### Recommended Order

1. **Phase 1** — Quick wins: 6 tests for minimal effort
2. **Phase 2** — set -u: 3 tests, well-contained fix
3. **Phase 3** — PWD/symlinks: 6 tests, one coherent area
4. **Phase 4** — Redirect validation: 3 tests, straightforward
5. **Phase 6** — Pipeline: 4 tests, high-impact
6. **Phase 8** — Background signals: 2 tests, related to pipeline work
7. **Phase 5** — Backtick semantics: 2 tests
8. **Phase 7** — Arith validation: 1 test
9. **Phase 9** — FD leaks: 1 test
10. **Phase 10** — FIFO redirect: 1 test
11. **Phase 12** — ulimit: 2 tests, needs FFI
12. **Phase 13** — declare -F extdebug: 1 test
13. **Phase 11** — Trap + signals: 3 tests, hardest
14. **Phase 14** — Word joining: 1 test, subtle
15. **Phase 15** — $@ in defaults: 3 tests, subtle
