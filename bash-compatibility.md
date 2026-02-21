# Shell Compatibility Report

Generated: 2026-02-21

## Summary

| Shell | Pass | Total | Rate |
|-------|------|-------|------|
| bash | 1037 | 1179 | 88% |
| gsh | 1156 | 1179 | 98% |

## Results by Tier

### Tier 0 — Core

| Suite | Description | bash | gsh |
|-------|-------------|-----|-----|
| smoke | Basic shell operations | **18/18** | **18/18** |
| pipeline | Pipe operator and pipelines | 25/26 | **26/26** |
| redirect | I/O redirection (>, <, >>, etc.) | 38/41 | 40/41 |
| redirect-multi | Multiple and complex redirections | 11/13 | **13/13** |
| builtin-eval-source | eval and source/. builtins | 22/23 | **23/23** |
| command-sub | Command substitution $() and `` | 29/30 | **30/30** |
| comments | Shell comments | **2/2** | **2/2** |
| exit-status | Exit status and $? | 9/11 | **11/11** |

### Tier 1 — Expansion & Variables

| Suite | Description | bash | gsh |
|-------|-------------|-----|-----|
| here-doc | Here-documents (<<, <<-, <<< ) | 35/36 | **36/36** |
| quote | Quoting (single, double, $'...') | 31/35 | **35/35** |
| word-eval | Word evaluation and expansion | **8/8** | **8/8** |
| word-split | IFS word splitting | 50/55 | **55/55** |
| var-sub | Variable substitution ($var, ${var}) | 5/6 | **6/6** |
| var-sub-quote | Variable substitution in quoting contexts | 40/41 | **41/41** |
| var-num | Numeric/special variables ($#, $?, $$, etc.) | **7/7** | **7/7** |
| var-op-test | Variable operators (${var:-default}, etc.) | 33/37 | 33/37 |
| var-op-strip | Variable pattern stripping (${var#pat}, etc.) | 28/29 | **29/29** |
| var-op-len | Variable length ${#var} | 5/9 | 7/9 |
| assign | Variable assignment | 36/48 | **48/48** |
| tilde | Tilde expansion (~, ~user) | 12/14 | **14/14** |

### Tier 2 — Builtins & Advanced

| Suite | Description | bash | gsh |
|-------|-------------|-----|-----|
| arith | Arithmetic expansion $(( )) and (( )) | 61/74 | 71/74 |
| glob | Filename globbing (*, ?, [...]) | 36/39 | 38/39 |
| brace-expansion | Brace expansion ({a,b}, {1..5}) | 46/55 | 54/55 |
| case_ | case statement | **13/13** | **13/13** |
| if_ | if/elif/else statement | **5/5** | **5/5** |
| loop | while, until, for loops | 23/29 | **29/29** |
| for-expr | C-style for ((i=0; ...)) | 8/9 | **9/9** |
| subshell | Subshell execution (...) | **2/2** | **2/2** |
| sh-func | Shell functions | 10/12 | **12/12** |
| builtin-echo | echo builtin | **27/27** | **27/27** |
| builtin-printf | printf builtin | 54/63 | 62/63 |
| builtin-read | read builtin | 58/64 | **64/64** |
| builtin-cd | cd builtin | 27/30 | 29/30 |
| builtin-set | set and shopt builtins | **24/24** | **24/24** |
| builtin-type | type/command/which builtins | 4/6 | **6/6** |
| builtin-trap | trap builtin | 31/33 | **33/33** |
| builtin-bracket | [[ ]] and [ ] test operators | 49/52 | **52/52** |
| builtin-misc | Misc builtins (true, false, colon, etc.) | 3/7 | 6/7 |
| builtin-process | Process builtins (kill, wait, ulimit, etc.) | 18/26 | 24/26 |
| background | Background jobs (&, wait, jobs) | 24/27 | 25/27 |
| command-parsing | Command parsing edge cases | 4/5 | **5/5** |
| var-op-bash | Bash-specific variable operations | 24/27 | 26/27 |
| var-op-slice | Variable slicing ${var:offset:length} | 19/22 | **22/22** |
| assign-extended | declare/typeset/local/export | 23/39 | 36/39 |

## Failing Tests — gsh

Tests where gsh fails but bash passes.

### Tier 0 — Core

| Suite | # | Test | Reason |
|-------|---|------|--------|
| redirect | 30 | <> for read/write named pipes | status: expected 0, got -1; stdout mismatch |

### Tier 1 — Expansion & Variables

| Suite | # | Test | Reason |
|-------|---|------|--------|
| var-op-test | 4 | Unquoted with array as default value | stdout mismatch |
| var-op-test | 6 | Assign default with array | stdout mismatch |
| var-op-test | 34 | op-test for ${a[@]} and ${a[*]} | stdout mismatch |
| var-op-test | 37 | op-test for unquoted ${a[*]:-empty} with IFS= | stdout mismatch |

### Tier 2 — Builtins & Advanced

| Suite | # | Test | Reason |
|-------|---|------|--------|
| builtin-cd | 27 | Survey of getcwd() syscall | stdout mismatch |
| builtin-process | 23 | write big file with ulimit | stdout mismatch |
| background | 8 | wait for N parallel jobs and check failure | stdout mismatch |
| background | 27 | Signal message for killed background job | status: expected 0, got 1; stdout mismatch |
| assign-extended | 22 | declare -p UNDEF (and typeset) -- prints something to stderr | stdout mismatch |

## Bonus: Tests where gsh passes but bash fails

| Suite | # | Test |
|-------|---|------|
| pipeline | 20 | bash/dash/mksh run the last command is run in its own process |
| redirect | 9 | Descriptor redirect with filename |
| redirect | 28 | 1>&2- (Bash bug: fail to restore closed fd) |
| redirect | 33 | can't mention big file descriptor |
| redirect-multi | 3 | ysh behavior when glob doesn't match |
| redirect-multi | 11 | Non-file redirects don't respect glob args (we differe from bash) |
| builtin-eval-source | 5 | eval YSH block with 'break continue return error' |
| command-sub | 29 | Syntax errors with double quotes within backticks |
| exit-status | 3 | subshell OverflowError https://github.com/oilshell/oil/issues/996 |
| exit-status | 4 | func subshell OverflowError https://github.com/oilshell/oil/issues/996 |
| here-doc | 7 | Here doc with bad comsub delimiter |
| quote | 20 | $? split over multiple lines |
| quote | 21 | Unterminated single quote |
| quote | 22 | Unterminated double quote |
| quote | 35 | \c' is an escape, unlike bash |
| word-split | 40 | IFS='' with ${!prefix@} and ${!prefix*} (bug #627) |
| word-split | 41 | IFS='' with ${!a[@]} and ${!a[*]} (bug #627) |
| word-split | 49 | IFS=x and '' and $@ - same bug as spec/toysh-posix case #12 |
| word-split | 50 | IFS=x and '' and $@ (#2) |
| word-split | 51 | IFS=x and '' and $@ (#3) |
| var-sub | 1 | Bad var sub |
| var-sub-quote | 39 | Right Brace as argument (similar to #702) |
| var-op-test | 13 | "${array[@]} with set -u (bash is outlier) |
| var-op-test | 25 | Error when empty |
| var-op-test | 26 | Error when unset |
| var-op-test | 36 | op-test for ${!array} with array="a[@]" or array="a[*]" |
| var-op-strip | 26 | strip none unicode |
| var-op-len | 7 | Length of undefined variable with nounset |
| var-op-len | 8 | Length operator can't be followed by test operator |
| assign | 15 | Env binding in readonly/declare is NOT exported!  (pitfall) |
| assign | 17 | dynamic local variables (and splitting) |
| assign | 19 | 'local x' does not set variable |
| assign | 20 | 'local -a x' does not set variable |
| assign | 25 | Reveal existence of "temp frame" (All shells disagree here!!!) |
| assign | 27 | Using ${x-default} after unsetting local shadowing a global |
| assign | 28 | Using ${x-default} after unsetting a temp binding shadowing a global |
| assign | 31 | assignment using dynamic keyword (splits in most shells, not in zsh/osh) |
| assign | 32 | assignment using dynamic var names doesn't split |
| assign | 35 | readonly $x where x='b c' |
| assign | 41 | redirect after bare assignment |
| assign | 46 | "declare -a arr" and "readonly -a a" creates an empty array (OSH) |
| tilde | 4 | No tilde expansion in word that looks like assignment but isn't |
| tilde | 13 | strict tilde |
| arith | 8 | Constant with quotes like '1' |
| arith | 12 | Invalid string to int with strict_arith |
| arith | 21 | Increment undefined variables with nounset |
| arith | 29 | No floating point |
| arith | 44 | Invalid LValue |
| arith | 45 | Invalid LValue that looks like array |
| arith | 46 | Invalid LValue: two sets of brackets |
| arith | 66 | Invalid constant |
| arith | 69 | Negative numbers with bit shift |
| arith | 74 | s[0] with string '12 34' |
| glob | 20 | : escaped |
| glob | 24 | set -o noglob |
| brace-expansion | 12 | double expansion with simple var -- bash bug |
| brace-expansion | 14 | double expansion with literal and simple var |
| brace-expansion | 18 | { in expansion |
| brace-expansion | 33 | Ascending number range expansion with negative step is invalid |
| brace-expansion | 34 | regression: -1 step disallowed |
| brace-expansion | 35 | regression: 0 step disallowed |
| brace-expansion | 36 | Descending number range expansion with positive step is invalid |
| brace-expansion | 42 | Char ranges with steps of the wrong sign |
| loop | 3 | for loop with invalid identifier |
| loop | 15 | continue in subshell |
| loop | 16 | continue in subshell aborts with errexit |
| loop | 17 | bad arg to break |
| loop | 18 | too many args to continue |
| loop | 24 | top-level break/continue/return (without strict_control_flow) |
| for-expr | 7 | Arith lexer mode |
| sh-func | 9 | return "" (a lot of disagreement) |
| sh-func | 12 | Scope of global variable when sourced in function (Shell Functions aren't Closures) |
| builtin-printf | 28 | Too large |
| builtin-printf | 33 | printf backslash escapes |
| builtin-printf | 35 | printf unicode backslash escapes |
| builtin-printf | 41 | printf %q |
| builtin-printf | 42 | printf %6q (width) |
| builtin-printf | 53 | printf positive integer overflow |
| builtin-printf | 54 | printf negative integer overflow |
| builtin-printf | 63 | Arbitrary base 64#a is rejected (unlike in shell arithmetic) |
| builtin-read | 12 | read -n with invalid arg |
| builtin-read | 36 | read -t -0.5 is invalid |
| builtin-read | 38 | read -u syntax error |
| builtin-read | 44 | read usage |
| builtin-read | 49 | mapfile from directory (bash doesn't handle errors) |
| builtin-read | 64 | read bash bug |
| builtin-cd | 4 | cd with 2 or more args is allowed (strict_arg_parse disabled) |
| builtin-cd | 28 | chdir is a synonym for cd - busybox ash |
| builtin-cd | 30 | pwd errors out on args with strict_arg_parse |
| builtin-type | 5 | special builtins are called out |
| builtin-type | 6 | more special builtins |
| builtin-trap | 1 | traps are not active inside subshells $() ()  trap | cat |
| builtin-trap | 17 | exit 1 when trap code string is invalid |
| builtin-bracket | 31 | [ -t invalid ] |
| builtin-bracket | 43 | Overflow error |
| builtin-bracket | 51 | Looks like octal, but digit is too big |
| builtin-misc | 1 | history builtin usage |
| builtin-misc | 2 | Print shell strings with weird chars: set and printf %q and ${x@Q} |
| builtin-misc | 7 | Invalid shift argument |
| builtin-process | 8 | Exit builtin with invalid arg |
| builtin-process | 9 | Exit builtin with too many args |
| builtin-process | 10 | time with brace group argument |
| builtin-process | 12 | ulimit too many args |
| builtin-process | 15 | ulimit -a doesn't take arg |
| builtin-process | 16 | ulimit doesn't accept multiple flags - reduce confusion between shells |
| builtin-process | 22 | ulimit -f 1 prevents files larger 512 bytes |
| background | 6 | wait with invalid arg |
| background | 25 | YSH wait --all |
| background | 26 | YSH wait --verbose |
| command-parsing | 1 | Prefix env on assignment |
| var-op-bash | 20 | ${!A@a} and ${!A[@]@a} |
| var-op-bash | 26 | Array expansion with nullary var op @P |
| var-op-slice | 10 | Slice undefined |
| var-op-slice | 12 | Slice string with invalid UTF-8 results in empty string and warning |
| var-op-slice | 13 | Slice string with invalid UTF-8 with strict_word_eval |
| assign-extended | 5 | declare -F with shopt -s extdebug prints more info |
| assign-extended | 8 | declare |
| assign-extended | 9 | declare -p |
| assign-extended | 11 | declare -p var |
| assign-extended | 12 | declare -p arr |
| assign-extended | 14 | declare -pnrx |
| assign-extended | 16 | declare -pnrx var |
| assign-extended | 17 | declare -pg |
| assign-extended | 18 | declare -pg var |
| assign-extended | 25 | typeset -r makes a string readonly |
| assign-extended | 26 | typeset -ar makes it readonly |
| assign-extended | 29 | Env bindings shouldn't contain array assignments |
| assign-extended | 33 | dynamic array parsing is not allowed |
| assign-extended | 36 | typeset +r removes read-only attribute (TODO: documented in bash to do nothing) |
