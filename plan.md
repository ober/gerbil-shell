# Gerbil Shell (gsh) — Remaining Compat Test Failures

Last updated: 2025-02-20

## Current State

The compat test suite (`make compat`) tests gsh against the Oils test corpus.
A test is considered a gsh failure only if **bash passes** but gsh does not.
Tests where both bash and gsh fail are not targets.

**16 genuine gsh-only failures remain** (bash passes all 16).

---

## Tests Where Both Bash and gsh Fail (NOT targets)

These are test framework expectations that bash itself doesn't match. No action needed.

| Suite           | Test #        | Description                    |
|-----------------|---------------|--------------------------------|
| arith           | #41, #51, #71 | Various arithmetic edge cases  |
| builtin-printf  | #34           | printf edge case               |
| builtin-misc    | #3            | misc builtin edge case         |
| glob            | #32           | glob edge case                 |
| brace-expansion | #43           | brace expansion edge case      |
| var-op-len      | #4, #5        | var length edge cases          |
| builtin-process | #17           | process builtin edge case      |
| var-op-bash     | #25           | bash-specific var op           |
| redirect        | #28           | redirect edge case             |
| assign-extended | #5, #10, #30  | extended assignment edge cases |

---

## Genuine gsh-Only Failures (16 total)

### Category 1: Arrays (not yet implemented)

These failures require array variable support (`declare -a`, `${arr[@]}`, etc.).

| # | Suite       | Test | Description                          | Root Cause                              |
|---|-------------|------|--------------------------------------|-----------------------------------------|
| 1 | var-op-test | #4   | Unquoted with array as default value | `${x:-${a[@]}}` — needs array expansion |
| 2 | var-op-test | #6   | Assign default with array            | `${x:=${a[@]}}` — needs array expansion |
| 3 | var-op-test | #37  | `${a[*]:-empty}` with IFS=           | Needs array subscript `[*]` support     |
| 4 | word-eval   | #4   | Word joining                         | Array word joining `"${a[@]}"`          |

**Fix approach:** Implement arrays (Tier 4 in original plan). All 4 tests will pass once `declare -a`, indexed arrays, and `${arr[@]}` / `${arr[*]}` expansion work.

**Effort:** XL (400+ LOC across parser, expander, environment, builtins)

---

### Category 2: Here-doc with Multiple Descriptors

| # | Suite    | Test | Description                                | Root Cause                                 |
|---|----------|------|--------------------------------------------|--------------------------------------------|
| 5 | here-doc | #4   | Here doc from another input fd             | `cat 0<<EOF` — here-doc on non-stdin fd    |
| 6 | here-doc | #5   | Multiple here docs with different fds      | `cat 0<<EOF \n cat 0<<EOF` — two here-docs |
| 7 | here-doc | #33  | Multiple here docs in pipeline             | `cat <<EOF1 \| cat <<EOF2`                 |
| 8 | here-doc | #34  | Multiple here docs in pipeline (multiline) | Same as #33, multiline variant             |

**Fix approach:** The parser currently handles a single here-doc per command. Need to:
1. Allow here-doc redirects on arbitrary fd numbers (not just implicit stdin)
2. Queue multiple here-doc bodies per compound command
3. Assign each queued body to its corresponding redirect fd

**Effort:** M (50-150 LOC in parser.ss, redirect.ss)

---

### Category 3: declare -p / declare -F Formatting

| #  | Suite           | Test | Description                           | Root Cause                                                                                                                    |
|----|-----------------|------|---------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| 9  | assign-extended | #6   | `declare -F` with `shopt -s extdebug` | `declare -F` should show function name + line + file when extdebug is on                                                      |
| 10 | assign-extended | #22  | `declare -p UNDEF`                    | Should print error to stderr; also `declare -p` needs double-quoted scalar values AND readonly/export -p sections need fixing |

**Fix approach:**
- #6: Track source file/line for function definitions. When `extdebug` is on, `declare -F` prints `funcname linenumber filename`.
- #22: Two changes needed together: (a) `declare -p` should double-quote scalar values (`declare -- x="val"` not `declare -- x=val`), (b) `readonly -p` and `export -p` output format needs to match bash's format for the BUG bash override to match.

**Effort:** M (50-150 LOC in builtins.ss, functions.ss)

---

### Category 4: Named Pipe Read/Write

| #  | Suite    | Test | Description                        | Root Cause                                   |
|----|----------|------|------------------------------------|----------------------------------------------|
| 11 | redirect | #30  | `<>` for read/write on named pipes | `exec 8<> /tmp/f.pipe` then `read <&8` hangs |

**Fix approach:** The `<>` (read-write) redirect opens with `O_RDWR|O_NONBLOCK` for FIFOs but the subsequent `read` hangs. The issue is that opening a FIFO with `O_RDWR` should work for both reading and writing, but the read blocks because no data has been written yet and the fd is likely being opened in blocking mode after the initial non-blocking open. Need to either: keep non-blocking mode and handle EAGAIN in read, or restructure the FIFO read/write to handle the bidirectional case.

**Effort:** M (50-150 LOC in redirect.ss, builtins.ss)

---

### Category 5: Quote Escapes

| # | Suite | Test | Description | Root Cause |
|---|-------|------|-------------|------------|
| 12 | quote | #28 | `$'\0NNN'` octal escapes | Octal escapes in `$'...'` don't produce correct output — likely leading zero handling |

**Fix approach:** Check the `$'...'` escape processing in lexer.ss. Octal `\0NNN` should produce the byte value. Verify against bash's behavior for `$'\0101'` (should be 'A' = 65 = octal 101).

**Effort:** S (<50 LOC in lexer.ss)

---

### Category 6: Process Resource Limits

| # | Suite | Test | Description | Root Cause |
|---|-------|------|-------------|------------|
| 13 | builtin-process | #23 | Write big file with ulimit | Needs `ulimit -f` (file size limit) support |

**Fix approach:** Implement `ulimit` builtin with at least `-f` (file size) support via `getrlimit`/`setrlimit` FFI. The test sets a file size limit and then tries to write a large file, expecting the write to fail.

**Effort:** L (150-400 LOC in builtins.ss, ffi.ss)

---

### Category 7: Background Job Wait

| # | Suite | Test | Description | Root Cause |
|---|-------|------|-------------|------------|
| 14 | background | #8 | Wait for N parallel jobs, check failure | Thread output ordering — background jobs running as threads produce output in non-deterministic order |
| 15 | background | #18 | `wait -n` | FFI thread-safety issue — `_waitpid_last_status` and `_waitpid_last_pid` are global C statics that race between threads |

**Fix approach:**
- #8: Background builtins/functions run as Gambit threads, so their output interleaves non-deterministically. May need to buffer output per-job or use mutexes for output ordering.
- #18: Make `ffi-waitpid-pid` return both PID and status atomically (single FFI call returning a pair/struct) instead of two separate calls that can race. Or use a mutex around the waitpid + status retrieval sequence.

**Effort:** M-L (100-200 LOC in jobs.ss, ffi.ss, executor.ss)

---

### Category 8: getcwd Behavior

| # | Suite | Test | Description | Root Cause |
|---|-------|------|-------------|------------|
| 16 | builtin-cd | #27 | Survey of getcwd() syscall | Tests PWD behavior after directory is deleted/renamed underneath the shell |

**Fix approach:** The test does `mkdir -p /tmp/d; cd /tmp/d; rmdir /tmp/d; pwd`. Bash's `pwd` returns error when the cwd has been removed. gsh likely doesn't handle this case — need to catch the error from `getcwd()` and report it properly.

**Effort:** S (<50 LOC in builtins.ss)

---

## Priority Order (by impact and effort)

| Priority | Category | Tests Fixed | Effort | Notes |
|----------|----------|-------------|--------|-------|
| 1 | Here-doc multi-fd (#5-8) | 4 | M | Parser/redirect work, no new subsystems |
| 2 | Quote escapes (#12) | 1 | S | Quick lexer fix |
| 3 | getcwd behavior (#16) | 1 | S | Quick builtin fix |
| 4 | declare -p/-F (#9-10) | 2 | M | Builtin formatting |
| 5 | Named pipe <> (#11) | 1 | M | Redirect fix |
| 6 | Background wait (#14-15) | 2 | M-L | FFI thread safety |
| 7 | ulimit (#13) | 1 | L | New FFI + builtin |
| 8 | Arrays (#1-4) | 4 | XL | Major new subsystem |

**Total: 16 tests across 8 categories**

Fixing categories 1-5 (9 tests, effort S-M) would be the most efficient path.
Arrays (category 8) would fix 4 tests but require the largest implementation effort.
