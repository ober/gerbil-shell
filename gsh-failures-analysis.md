# gsh Failing Tests Analysis

This document catalogs every test where bash passes but gsh fails, grouped by
root cause and estimated fix difficulty.

**Total failures: 57 tests across 15 suites**

---

## Category 1: Exit Status Truncation (mod 256)

**Root cause:** gsh does not truncate exit/return codes to 8 bits (mod 256). Bash
truncates `exit N` and `return N` to `N & 0xFF`. gsh passes through 0 for
out-of-range values.

**Estimated difficulty:** Easy -- single fix in the exit/return builtin code.

### exit-status #1 -- Truncating 'exit' status

```bash
$SH -c 'exit 255'
echo status=$?
$SH -c 'exit 256'
echo status=$?
$SH -c 'exit 257'
echo status=$?
echo ===
$SH -c 'exit -1'
echo status=$?
$SH -c 'exit -2'
echo status=$?
```

**Expected:**
```
status=255
status=0
status=1
===
status=255
status=254
```

**Actual gsh:**
```
status=0
status=0
status=0
===
status=0
status=0
```

---

## Category 2: Empty Command / Backtick Evaluation Semantics

**Root cause:** When backtick command substitution produces output, bash treats
the output as a command to execute. If the output is a non-empty string that
is not a valid command, the result is FALSE. gsh appears to not execute the
backtick result as a command properly in conditional contexts.

**Estimated difficulty:** Medium -- requires understanding how empty argv
interacts with `$?` in conditional contexts.

### exit-status #7 -- If subshell true WITH OUTPUT is different

```bash
if `sh -c 'echo X; true'`; then echo TRUE; else echo FALSE; fi
```

**Expected:** `FALSE` (because the backtick produces "X", which is executed
as a command and fails with status 127, making the if-condition false)

**Actual gsh:** `TRUE`

### exit-status #8 -- If subshell true WITH ARGUMENT

```bash
if `true` X; then echo TRUE; else echo FALSE; fi
```

**Expected:** `FALSE` (backtick produces empty string, so command becomes
just `X`, which is not found)

**Actual gsh:** `TRUE`

---

## Category 3: Pipeline Exit Status and SIGPIPE

**Root cause:** Multiple pipeline-related issues: (1) `egrep` returning
non-zero not propagated; (2) `!` negation not working with `$SH -c`
invocations; (3) SIGPIPE status (141) not captured in PIPESTATUS; (4)
`pipefail` not working with `lastpipe`.

**Estimated difficulty:** Medium to Hard

### pipeline #6 -- Exit code is last status

```bash
echo a | egrep '[0-9]+'
```

**Expected status:** 1 (egrep finds no match)

**Actual gsh status:** 0

### pipeline #12 -- ! turns non-zero into zero

```bash
! $SH -c 'exit 42'; echo $?
```

**Expected:** `0` (the `!` negates the non-zero exit)

**Actual gsh:** `1`

### pipeline #23 -- SIGPIPE causes pipeline to die

```bash
cat /dev/urandom | sleep 0.1
echo ${PIPESTATUS[@]}
```

**Expected:** `141 0`

**Actual gsh:** `0 0`

### pipeline #26 -- shopt -s lastpipe and shopt -s no_last_fork interaction

```bash
$SH -c '
shopt -s lastpipe
set -o errexit
set -o pipefail
ls | false | wc -l'
echo status=$?
# (repeated with no_fork_last)
```

**Expected:** `0\nstatus=1\n0\nstatus=1`

**Actual gsh:** `0\nstatus=0\n0\nstatus=0`

---

## Category 4: `ls` Exit Status for Nonexistent Files

**Root cause:** `ls /nonexistent-zzZZ` returns exit status 2 on most systems.
gsh appears to return 0 instead.

**Estimated difficulty:** Easy -- likely gsh is not propagating the external
command exit status correctly in this specific case.

### smoke #15 -- failed command

```bash
ls /nonexistent-zzZZ
```

**Expected status:** 2

**Actual gsh status:** 0

---

## Category 5: File Descriptor Leak / /proc/fd Detection

**Root cause:** gsh leaks extra file descriptors (fds 3, 4, 5, 10, 255, 256)
that are visible in `/proc/$$/fd`. This causes test #20 to see fd 7 as open
when it shouldn't be (test scans /proc/fd and finds unexpected fds).

**Estimated difficulty:** Medium -- need to audit which fds gsh keeps open
and close them or mark them close-on-exec.

### redirect #20 -- Redirect to file descriptor that's not open

```bash
opened=$(ls /proc/$$/fd)
if echo "$opened" | egrep '^7$'; then
  echo "FD 7 shouldn't be open"
  echo "OPENED:"
  echo "$opened"
fi
echo hi 1>&7
```

**Expected:** empty stdout, status 1

**Actual gsh:** Prints "FD 7 shouldn't be open" plus the list of open fds
(0, 1, 2, 3, 4, 5, 10, 255, 256), because gsh has extra fds open.

---

## Category 6: Named Pipe (FIFO) <> Read/Write

**Root cause:** `exec 8<> "$TMP/f.pipe"` on a FIFO hangs or fails in gsh
(returns status -1).

**Estimated difficulty:** Medium -- FIFO handling with `<>` bidirectional
redirect needs investigation; may be related to how gsh opens fifos.

### redirect #30 -- <> for read/write named pipes

```bash
rm -f "$TMP/f.pipe"
mkfifo "$TMP/f.pipe"
exec 8<> "$TMP/f.pipe"
echo first >&8
echo second >&8
read line1 <&8
read line2 <&8
exec 8<&-
echo line1=$line1 line2=$line2
```

**Expected:** `line1=first line2=second`

**Actual gsh:** empty output, status -1

---

## Category 7: Redirect Validation (Glob, Brace, Word Split)

**Root cause:** gsh does not fail when redirect targets expand to multiple
words via globbing, brace expansion, or word splitting. Bash returns status 1
("ambiguous redirect") in these cases.

**Estimated difficulty:** Medium -- redirect target expansion needs to check
for multiple words and report an error.

### redirect-multi #7 -- File redirect to $var with glob char

```bash
touch two-bar
star='*'
echo hi > two-$star
echo status=$?
head two-bar two-\*
```

**Expected status:** 1 (ambiguous redirect because `two-*` globs to a file)

**Actual gsh status:** 0

### redirect-multi #12 -- Redirect with brace expansion isn't allowed

```bash
echo hi > a-{one,two}
echo status=$?
head a-*
echo status=$?
```

**Expected:** `status=1\nstatus=1`

**Actual gsh:** `status=1\nstatus=0` (second `head a-*` finds `a-{one,two}` literal file)

### redirect-multi #13 -- File redirects have word splitting too!

```bash
file='foo bar'
echo hi > $file
echo status=$?
cat "$file"
echo status=$?
```

**Expected:** `status=1\nstatus=1` (ambiguous redirect due to word splitting)

**Actual gsh:** `status=1\nstatus=0`

---

## Category 8: Word Joining with `$@` in Non-Array Context

**Root cause:** When `$@` appears inside word concatenation
(`$s1"${array[@]}"_"$@"`), gsh does not split the result into separate words
at `$@` boundaries. Instead it joins everything into fewer words.

**Estimated difficulty:** Hard -- this is a subtle word-splitting/joining
issue involving `$@` expansion with adjacent literal text.

### word-eval #4 -- Word joining

```bash
set -- x y z
s1='1 2'
array=(a1 a2)
argv.py $s1"${array[@]}"_"$@"
```

**Expected:** `['1', '2a1', 'a2_x', 'y', 'z']`

**Actual gsh:** `['1 2a1', 'a2_x y z']`

---

## Category 9: `$@` in Default Value Expansion

**Root cause:** `${unset=x"$@"x}` should expand `$@` with word splitting in
unquoted context. gsh treats it as a single word instead of splitting at `$@`
boundaries.

**Estimated difficulty:** Hard -- related to Category 8; `$@` expansion
inside `${var=...}` default-value context.

### var-op-test #4 -- Unquoted with array as default value

```bash
set -- '1 2' '3 4'
argv.py X${unset=x"$@"x}X
argv.py X${unset=x$@x}X
```

**Expected:** `['Xx1', '2', '3', '4xX']\n['Xx1', '2', '3', '4xX']`

**Actual gsh:** `['Xx1 2 3 4xX']\n['Xx1', '2', '3', '4xX']`

### var-op-test #6 -- Assign default with array

```bash
set -- '1 2' '3 4'
argv.py X${unset=x"$@"x}X
argv.py "$unset"
```

**Expected:** `['Xx1', '2', '3', '4xX']\n['x1 2 3 4x']`

**Actual gsh:** `['Xx1 2 3 4xX']\n['x1 2 3 4x']`

### var-op-test #37 -- op-test for unquoted ${a[*]:-empty} with IFS=

```bash
IFS=
a=("" "")
argv.py ${a[*]:-empty}
```

**Expected:** `[]` (unquoted `${a[*]}` with empty IFS joins to empty string,
which gets elided, so the `:` form triggers default... but the result should
still be empty after word splitting)

**Actual gsh:** `['empty']`

---

## Category 10: Arithmetic Validation and Nounset

**Root cause:** Multiple arithmetic issues: (1) `$SH -c` not propagating
errors from invalid arithmetic constants; (2) nounset not triggering for
undefined variables in arithmetic contexts.

**Estimated difficulty:** Medium

### arith #14 -- Integer constant validation

```bash
check() {
  $SH -c "shopt --set strict_arith; echo $1"
  echo status=$?
}
check '$(( 0x1X ))'
check '$(( 09 ))'
check '$(( 2#A ))'
check '$(( 02#0110 ))'
```

**Expected:** `status=1` for all four checks

**Actual gsh:** `status=0` for all four checks (gsh does not fail on invalid
constants in child `$SH -c`)

---

## Category 11: PWD/Symlink Handling in Child Shells

**Root cause:** When gsh spawns a child shell via `$SH -c`, the child does
not properly inherit/reconstruct PWD. It uses physical paths instead of
logical paths through symlinks.

**Estimated difficulty:** Medium -- need to ensure child shell inherits and
trusts the parent's `$PWD` when the directory hasn't changed.

### builtin-cd #12 -- 'unset PWD; pwd' before any cd

```bash
dir=/tmp/oil-spec-test/pwd-2
mkdir -p $dir
cd $dir
$SH -c 'unset PWD; pwd'
```

**Expected:** `/tmp/oil-spec-test/pwd-2`

**Actual gsh:** `/tmp/tmph_7_fx_b` (gsh uses a temp dir or physical path)

### builtin-cd #13 -- lie about PWD; pwd before any cd

```bash
dir=/tmp/oil-spec-test/pwd-3
mkdir -p $dir
cd $dir
$SH -c 'PWD=foo; pwd'
```

**Expected:** `/tmp/oil-spec-test/pwd-3`

**Actual gsh:** `/tmp/tmpnrjw_un3`

### builtin-cd #15 -- pwd in symlinked dir on shell initialization

```bash
tmp=$TMP/builtins-pwd-2
mkdir -p $tmp/target
ln -s -f $tmp/target $tmp/symlink
cd $tmp/symlink
$SH -c 'basename $(pwd)'
unset PWD
$SH -c 'basename $(pwd)'
```

**Expected:** `symlink\ntarget`

**Actual gsh:** Uses temp dir name for both

### builtin-cd #16 -- Test cd .. involving symlinks

```bash
dir=$TMP/symlinktest
mkdir -p $dir
cd $dir
mkdir -p a/b/c
mkdir -p a/b/d
ln -s -f a/b/c c > /dev/null
cd c
cd ..
ls
```

**Expected:** `a\nc` (we're back in symlinktest)

**Actual gsh:** `a\nc\nsymlinktest\n_tmp` (ls shows too many entries -- wrong
directory)

### builtin-cd #26 -- What happens when inherited $PWD and current dir disagree?

```bash
# Uses python2 to chdir before spawning $SH
```

**Expected:** gsh picks up the correct CWD from the process

**Actual gsh:** Uses tmp dir paths instead of expected paths

### builtin-cd #27 -- Survey of getcwd() syscall

```bash
strace -e getcwd -- $SH -c 'echo hi; pwd; echo $PWD' 1> /dev/null 2> err.txt
wc -l err.txt
```

**Expected:** `1 err.txt` (one getcwd call)

**Actual gsh:** `5 err.txt` (five getcwd calls -- gsh calls getcwd too often)

---

## Category 12: set -u (nounset) Error Propagation

**Root cause:** When `set -u` is active and an undefined variable is
referenced, gsh does not set a non-zero exit status for the spawned child
shell (`$SH -c`). The child process exits 0 instead of non-zero.

**Estimated difficulty:** Medium -- nounset error needs to cause the shell to
exit with non-zero status.

### builtin-set #6 -- set -u with undefined variable exits the interpreter

```bash
$SH -c 'set -u; echo before; echo $x; echo after'
if test $? -ne 0; then
  echo OK
fi
$SH -i -c 'set -u; echo before; echo $x; echo after'
if test $? -ne 0; then
  echo OK
fi
```

**Expected:** `before\nOK\nbefore\nOK`

**Actual gsh:** `before\nbefore` (no OK -- child exits 0)

### builtin-set #7 -- set -u in interactive shell does NOT exit the interpreter

```bash
$SH -c 'set -u; echo before; echo $x; echo after
echo line2
'
if test $? -ne 0; then
  echo OK
fi
$SH -i -c 'set -u; echo before; echo $x; echo after
echo line2
'
if test $? -ne 0; then
  echo OK
fi
```

**Expected:** `before\nOK\nbefore\nline2` (non-interactive exits, interactive
continues to next line)

**Actual gsh:** `before\nbefore\nline2` (non-interactive case doesn't exit
with error)

### builtin-set #8 -- set -u error can break out of nested evals

```bash
$SH -c '
set -u
test_function_2() { x=$blarg; }
test_function() { eval "test_function_2"; }
echo before
eval test_function
echo after
'
if test $? -ne 0; then
  echo OK
fi
```

**Expected:** `before\nOK`

**Actual gsh:** `before` (child exits 0, so no OK printed)

---

## Category 13: Trap + Signal + Sleep Interaction

**Root cause:** When gsh runs a script that sets a trap and then sleeps, and
an external signal is sent to the child process, gsh does not properly
deliver the signal or run the trap handler. The child script produces no
output at all.

**Estimated difficulty:** Hard -- involves signal delivery to child processes
and trap handler execution in background jobs.

### builtin-trap #27 -- trap USR1, sleep, SIGINT: non-interactively

```bash
# In builtin-trap-usr1.sh:
kill=$(command -v kill)
$SH -c 'trap "echo usr1" USR1; sleep 0.2' &
sleep 0.1
$kill -USR1 $!
wait
echo status=$?
```

**Expected:** `usr1\nstatus=0`

**Actual gsh:** (empty output)

### builtin-trap #28 -- trap INT, sleep, SIGINT: non-interactively

```bash
# In builtin-trap-int.sh:
kill=$(command -v kill)
$SH -c 'trap "echo int" INT; sleep 0.2' &
sleep 0.1
$kill -INT $!
wait
echo status=$?
```

**Expected:** `status=0`

**Actual gsh:** (empty output)

### builtin-trap #29 -- trap EXIT, sleep, SIGINT: non-interactively

```bash
# In builtin-trap-exit.sh:
kill=$(command -v kill)
$SH -c 'trap "echo on exit" EXIT; sleep 0.2' &
sleep 0.1
$kill -INT $!
wait
echo status=$?
```

**Expected:** `on exit\nstatus=0`

**Actual gsh:** (empty output)

---

## Category 14: trap - Removal with Unsigned Integer Strings

**Root cause:** When `trap -1 EXIT` is used (invalid signal number), gsh does
not detect the error and report non-zero status. Specifically `trap -1 EXIT`
should fail but gsh succeeds.

**Estimated difficulty:** Easy

### builtin-trap #30 -- Remove trap with an unsigned integer

```bash
$SH -e -c '
trap "echo trap-exit" EXIT
trap -1 EXIT
echo bad
'
if test $? -ne 0; then
  echo failure
fi
```

**Expected:** `trap-exit\nfailure` (the `trap -1 EXIT` fails, and `-e` causes exit)

**Actual gsh:** `trap-exit` (no `failure` -- the child exits 0)

---

## Category 15: ulimit Enforcement

**Root cause:** gsh's ulimit implementation doesn't properly enforce file size
limits (ulimit -f) or file descriptor limits (ulimit -n 0).

**Estimated difficulty:** Medium to Hard

### builtin-process #23 -- write big file with ulimit

```bash
{ echo 'ulimit -f 1'
  python2 -c 'print("echo " + "X"*9000 + " >out.txt")'
  echo 'echo inner=$?'
} > big.sh
$SH big.sh
echo outer=$?
```

**Expected:** `outer=153` (SIGXFSZ kills the process)

**Actual gsh:** `XXXXXXXecho inner=0\nouter=0` (ulimit not enforced,
output corrupted)

### builtin-process #26 -- ulimit -n limits file descriptors

```bash
$SH -c 'ulimit -n 64; echo hi >out'
echo status=$?
$SH -c 'ulimit -n 0; echo hi >out'
echo status=$?
```

**Expected:** `status=0\nstatus=1`

**Actual gsh:** `status=0\nstatus=0` (ulimit -n 0 doesn't prevent file opens)

---

## Category 16: Background Job Signal Reporting

**Root cause:** When a background job is killed with a signal, gsh does not
report the correct exit status (128 + signal number) and does not produce the
signal name on stderr.

**Estimated difficulty:** Medium

### background #8 -- wait for N parallel jobs and check failure

```bash
set -o errexit
pids=''
for i in 3 2 1; do
  { sleep 0.0$i; echo $i; exit $i; } &
  pids="$pids $!"
done
for pid in $pids; do
  set +o errexit
  wait $pid
  status=$?
  set -o errexit
  echo status=$status
done
```

**Expected:** `1\n2\n3\nstatus=3\nstatus=2\nstatus=1`

**Actual gsh:** Output differs (timing/ordering issue with background jobs)

### background #27 -- Signal message for killed background job

```bash
case $SH in dash|mksh) exit ;; esac
sleep 1 &
kill -HUP $!
wait $! 2>err.txt
echo status=$?
grep -o "Hangup" err.txt
```

**Expected:** `status=129\nHangup`

**Actual gsh:** `status=0` (killed job reports 0, no signal message)

---

## Category 17: var-op-bash `${#var@X}` Parse Error

**Root cause:** `${#A[@]@P}` should be a parse error (you can't combine
`#` length operator with `@X` transformations). gsh doesn't detect this as
an error.

**Estimated difficulty:** Easy -- parser needs to reject `${#var@X}` syntax.

### var-op-bash #19 -- ${#var@X} is a parse error

```bash
$SH -c 'declare -A A=(["x"]="y"); echo ${#A[@]@P}'
if test $? -ne 0; then echo fail; fi
$SH -c 'declare -A A=(["x"]="y"); echo ${#A[@]@Q}'
if test $? -ne 0; then echo fail; fi
$SH -c 'declare -A A=(["x"]=y); echo ${#A[@]@a}'
if test $? -ne 0; then echo fail; fi
```

**Expected:** `fail\nfail\nfail`

**Actual gsh:** (empty -- gsh executes the expressions instead of erroring)

---

## Category 18: declare -F with extdebug

**Root cause:** `declare -F funcname` with `shopt -s extdebug` should print
the function name, line number, and source file. gsh only prints the function
name.

**Estimated difficulty:** Medium -- need to track function definition
locations (file + line number).

### assign-extended #6 -- declare -F with shopt -s extdebug and main file

```bash
$SH $REPO_ROOT/spec/testdata/extdebug.sh | sed "s;$REPO_ROOT;ROOT;g"
```

**Expected:**
```
declare -f add
declare -f g

add 5 ROOT/spec/testdata/extdebug.sh
g 3 ROOT/spec/testdata/bash-source-2.sh
```

**Actual gsh:**
```
declare -f add
declare -f g

g
add
```

---

## Category 19: declare -p Quoting

**Root cause:** `declare -p` in gsh does not quote string values with double
quotes like bash does. Bash outputs `declare -rx x="42"` while gsh outputs
`declare -rx x=42`.

**Estimated difficulty:** Easy -- add double-quote wrapping in declare -p
output.

### assign-extended #22 -- declare -p UNDEF (and typeset) prints something to stderr

```bash
x=42
readonly x
export x
declare -p x undef1 undef2 2> de
typeset -p x undef1 undef2 2> ty
files='de ty'
wc -l $files
```

**Expected:**
```
declare -rx x="42"
declare -rx x="42"
  2 de
  2 ty
  4 total
```

**Actual gsh:** Same but `x=42` instead of `x="42"`

---

## Summary Table

| # | Category | Tests | Difficulty | Description |
|---|----------|-------|------------|-------------|
| 1 | Exit status truncation | exit-status #1 | Easy | `exit N` not truncated to N & 0xFF |
| 2 | Backtick evaluation | exit-status #7, #8 | Medium | Backtick output not executed as command in if-condition |
| 3 | Pipeline status/SIGPIPE | pipeline #6, #12, #23, #26 | Medium-Hard | SIGPIPE, `!` negation, pipefail issues |
| 4 | External cmd status | smoke #15 | Easy | `ls /nonexistent` returning 0 |
| 5 | FD leaks | redirect #20 | Medium | Extra fds visible in /proc/$$/ |
| 6 | FIFO <> redirect | redirect #30 | Medium | `exec N<> fifo` hangs/fails |
| 7 | Redirect validation | redirect-multi #7, #12, #13 | Medium | Ambiguous redirect not detected |
| 8 | Word joining with $@ | word-eval #4 | Hard | $@ splitting in concatenation |
| 9 | $@ in default value | var-op-test #4, #6, #37 | Hard | $@ expansion in ${var=...} |
| 10 | Arith validation | arith #14 | Medium | Invalid constants not erroring in child |
| 11 | PWD/symlink in child | builtin-cd #12, #13, #15, #16, #26, #27 | Medium | Child shell PWD inheritance |
| 12 | set -u propagation | builtin-set #6, #7, #8 | Medium | Nounset error exit status |
| 13 | Trap + signal + sleep | builtin-trap #27, #28, #29 | Hard | Signal delivery to child processes |
| 14 | trap -1 validation | builtin-trap #30 | Easy | Invalid signal number detection |
| 15 | ulimit enforcement | builtin-process #23, #26 | Medium-Hard | File size / fd count limits |
| 16 | Background signal | background #8, #27 | Medium | Signal exit status for killed jobs |
| 17 | ${#var@X} parse error | var-op-bash #19 | Easy | Parser rejects # + @X combo |
| 18 | declare -F extdebug | assign-extended #6 | Medium | Function definition location tracking |
| 19 | declare -p quoting | assign-extended #22 | Easy | Add double quotes around values |

---

## Recommended Fix Order

### Quick wins (Easy, 6 tests fixed):

1. **Exit status truncation** (exit-status #1) -- Apply `& 0xFF` to
   exit/return values
2. **smoke #15** -- Investigate why external command exit status is 0
3. **trap -1 validation** (builtin-trap #30) -- Check for negative signal
   numbers
4. **${#var@X} parse error** (var-op-bash #19) -- Add parser check
5. **declare -p quoting** (assign-extended #22) -- Quote values in output

### Medium fixes (Medium, ~25 tests fixed):

6. **set -u propagation** (builtin-set #6, #7, #8) -- Ensure nounset errors
   cause non-zero exit
7. **PWD/symlink handling** (builtin-cd #12, #13, #15, #16, #26, #27) --
   Fix child shell PWD inheritance
8. **Redirect validation** (redirect-multi #7, #12, #13) -- Check for
   ambiguous redirects
9. **Arith validation** (arith #14) -- Propagate arithmetic errors in
   child shells
10. **Background signal status** (background #8, #27) -- Report 128+signum
11. **FD leaks** (redirect #20) -- Close internal fds
12. **FIFO <> redirect** (redirect #30) -- Fix bidirectional FIFO open
13. **Backtick evaluation** (exit-status #7, #8) -- Execute backtick output
    as command
14. **declare -F extdebug** (assign-extended #6) -- Track function locations

### Hard fixes (~10 tests fixed):

15. **Pipeline status/SIGPIPE** (pipeline #6, #12, #23, #26) -- SIGPIPE
    handling, `!` with $SH, pipefail
16. **Trap + signal + sleep** (builtin-trap #27, #28, #29) -- Signal
    delivery to background child shells
17. **Word joining with $@** (word-eval #4) -- $@ splitting in
    concatenation context
18. **$@ in default value** (var-op-test #4, #6, #37) -- $@ expansion
    in ${var=...}
19. **ulimit enforcement** (builtin-process #23, #26) -- File size and
    fd limits
