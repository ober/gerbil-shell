# Shell Compatibility Report

Generated: 2026-02-21

## Summary

| Shell | Pass | Total | Rate |
|-------|------|-------|------|
| bash | 1037 | 1179 | 88% |
| zsh | 808 | 1179 | 69% |
| gsh | 1122 | 1179 | 95% |

## Results by Tier

### Tier 0 — Core

| Suite | Description | bash | zsh | gsh |
|-------|-------------|-----|-----|-----|
| smoke | Basic shell operations | **18/18** | 17/18 | 17/18 |
| pipeline | Pipe operator and pipelines | 25/26 | 21/26 | 22/26 |
| redirect | I/O redirection (>, <, >>, etc.) | 38/41 | 37/41 | 38/41 |
| redirect-multi | Multiple and complex redirections | 11/13 | 4/13 | 10/13 |
| builtin-eval-source | eval and source/. builtins | 22/23 | 15/23 | **23/23** |
| command-sub | Command substitution $() and `` | 29/30 | 25/30 | **30/30** |
| comments | Shell comments | **2/2** | **2/2** | **2/2** |
| exit-status | Exit status and $? | 9/11 | 8/11 | 6/11 |

### Tier 1 — Expansion & Variables

| Suite | Description | bash | zsh | gsh |
|-------|-------------|-----|-----|-----|
| here-doc | Here-documents (<<, <<-, <<< ) | 35/36 | 30/36 | **36/36** |
| quote | Quoting (single, double, $'...') | 31/35 | 28/35 | **35/35** |
| word-eval | Word evaluation and expansion | **8/8** | 5/8 | 7/8 |
| word-split | IFS word splitting | 50/55 | 20/55 | **55/55** |
| var-sub | Variable substitution ($var, ${var}) | 5/6 | 3/6 | **6/6** |
| var-sub-quote | Variable substitution in quoting contexts | 40/41 | 30/41 | **41/41** |
| var-num | Numeric/special variables ($#, $?, $$, etc.) | **7/7** | **7/7** | **7/7** |
| var-op-test | Variable operators (${var:-default}, etc.) | 33/37 | 24/37 | 34/37 |
| var-op-strip | Variable pattern stripping (${var#pat}, etc.) | 28/29 | 24/29 | **29/29** |
| var-op-len | Variable length ${#var} | 5/9 | 7/9 | 7/9 |
| assign | Variable assignment | 36/48 | 39/48 | **48/48** |
| tilde | Tilde expansion (~, ~user) | 12/14 | 11/14 | **14/14** |

### Tier 2 — Builtins & Advanced

| Suite | Description | bash | zsh | gsh |
|-------|-------------|-----|-----|-----|
| arith | Arithmetic expansion $(( )) and (( )) | 61/74 | 51/74 | 69/74 |
| glob | Filename globbing (*, ?, [...]) | 36/39 | 24/39 | 38/39 |
| brace-expansion | Brace expansion ({a,b}, {1..5}) | 46/55 | 40/55 | 54/55 |
| case_ | case statement | **13/13** | 10/13 | **13/13** |
| if_ | if/elif/else statement | **5/5** | 4/5 | **5/5** |
| loop | while, until, for loops | 23/29 | 22/29 | **29/29** |
| for-expr | C-style for ((i=0; ...)) | 8/9 | 8/9 | **9/9** |
| subshell | Subshell execution (...) | **2/2** | **2/2** | **2/2** |
| sh-func | Shell functions | 10/12 | **12/12** | **12/12** |
| builtin-echo | echo builtin | **27/27** | 21/27 | **27/27** |
| builtin-printf | printf builtin | 54/63 | 44/63 | 62/63 |
| builtin-read | read builtin | 58/64 | 30/64 | **64/64** |
| builtin-cd | cd builtin | 27/30 | 26/30 | 24/30 |
| builtin-set | set and shopt builtins | **24/24** | 16/24 | 21/24 |
| builtin-type | type/command/which builtins | 4/6 | 2/6 | **6/6** |
| builtin-trap | trap builtin | 31/33 | 19/33 | 29/33 |
| builtin-bracket | [[ ]] and [ ] test operators | 49/52 | 44/52 | **52/52** |
| builtin-misc | Misc builtins (true, false, colon, etc.) | 3/7 | 3/7 | 6/7 |
| builtin-process | Process builtins (kill, wait, ulimit, etc.) | 18/26 | 19/26 | 23/26 |
| background | Background jobs (&, wait, jobs) | 24/27 | 15/27 | 25/27 |
| command-parsing | Command parsing edge cases | 4/5 | **5/5** | **5/5** |
| var-op-bash | Bash-specific variable operations | 24/27 | 3/27 | 24/27 |
| var-op-slice | Variable slicing ${var:offset:length} | 19/22 | 16/22 | **22/22** |
| assign-extended | declare/typeset/local/export | 23/39 | 15/39 | 34/39 |

## Failing Tests — zsh

Tests where zsh fails but bash passes.

### Tier 0 — Core

| Suite | # | Test | Reason |
|-------|---|------|--------|
| smoke | 14 | $@ $* | stdout mismatch |
| pipeline | 4 | Redirect in Pipeline | stdout mismatch |
| pipeline | 8 | PIPESTATUS | stdout mismatch |
| pipeline | 9 | PIPESTATUS is set on simple commands | stdout mismatch |
| pipeline | 10 | PIPESTATUS with shopt -s lastpipe | stdout mismatch |
| pipeline | 23 | SIGPIPE causes pipeline to die (regression for issue #295) | stdout mismatch |
| redirect | 4 | 2&>1 (is it a redirect or is it like a&>1) | stdout mismatch |
| redirect | 12 | Double digit fd (20> file) | status: expected 0, got 127; stdout mismatch |
| redirect | 27 | 1>&2- to move file descriptor | stdout mismatch |
| redirect-multi | 2 | File redirect without matching any file | status: expected 0, got 1; stdout mismatch |
| redirect-multi | 4 | File redirect without matching any file, with failglob | status: expected 0, got 1; stdout mismatch |
| redirect-multi | 7 | File redirect to $var with glob char | status: expected 1, got 0; stdout mismatch |
| redirect-multi | 8 | File redirect that globs to more than one file (bash and zsh only) | stdout mismatch |
| redirect-multi | 9 | File redirect with extended glob | status: expected 0, got 1; stdout mismatch |
| redirect-multi | 10 | Extended glob that doesn't match anything | status: expected 0, got 1; stdout mismatch |
| redirect-multi | 12 | Redirect with brace expansion isn't allowed | stdout mismatch |
| redirect-multi | 13 | File redirects have word splitting too! | stdout mismatch |
| builtin-eval-source | 3 | eval usage | stdout mismatch |
| builtin-eval-source | 10 | Source nonexistent | stdout mismatch |
| builtin-eval-source | 11 | Source with no arguments | stdout mismatch |
| builtin-eval-source | 14 | Source with syntax error | stdout mismatch |
| builtin-eval-source | 17 | Eval in bash does tilde expansion in array | status: expected 0, got 1; stdout mismatch |
| builtin-eval-source | 18 | source works for files in current directory (bash only) | stdout mismatch |
| builtin-eval-source | 22 | source doesn't crash when targeting a directory | stdout mismatch |
| command-sub | 8 | Making keyword out of command sub should NOT work | status: expected 2, got 1 |
| command-sub | 21 | Quoting \ within `` | status: expected 0, got 1; stdout mismatch |
| command-sub | 25 | Quoting non-special characters within `` | status: expected 0, got 1; stdout mismatch |
| command-sub | 28 | More levels of double quotes in backticks | stdout mismatch |
| exit-status | 1 | Truncating 'exit' status | status: expected 0, got 1; stdout mismatch |
| exit-status | 2 | Truncating 'return' status | status: expected 0, got 1; stdout mismatch |

### Tier 1 — Expansion & Variables

| Suite | # | Test | Reason |
|-------|---|------|--------|
| here-doc | 8 | Here doc and < redirect -- last one wins | stdout mismatch |
| here-doc | 9 | < redirect and here doc -- last one wins | stdout mismatch |
| here-doc | 15 | Two here docs -- first is ignored; second ones wins! | stdout mismatch |
| here-doc | 16 | Here doc with line continuation, then pipe.  Syntax error. | status: expected 2, got 1 |
| here-doc | 19 | Here doc with builtin 'read' | status: expected 0, got 1; stdout mismatch |
| here-doc | 25 | Two compound commands with two here docs | status: expected 0, got 1; stdout mismatch |
| quote | 12 | Storing backslashes and then echoing them | stdout mismatch |
| quote | 15 | C-style backslash escapes inside double quoted string | stdout mismatch |
| quote | 24 | No tab escapes within single quotes | stdout mismatch |
| quote | 30 | OSH allows invalid backslashes | stdout mismatch |
| quote | 32 | $"" is a synonym for "" | stdout mismatch |
| quote | 34 | $'' supports \cA escape for Ctrl-A - mask with 0x1f | stdout mismatch |
| word-eval | 3 | Word splitting | stdout mismatch |
| word-eval | 4 | Word joining | stdout mismatch |
| word-eval | 7 | Globbing after splitting | stdout mismatch |
| word-split | 1 | IFS is scoped | stdout mismatch |
| word-split | 2 | Tilde sub is not split, but var sub is | stdout mismatch |
| word-split | 3 | Word splitting | stdout mismatch |
| word-split | 4 | Word splitting 2 | stdout mismatch |
| word-split | 5 | $* | stdout mismatch |
| word-split | 7 | $@ | stdout mismatch |
| word-split | 11 | Word elision with space | stdout mismatch |
| word-split | 12 | Word elision with non-whitespace IFS | stdout mismatch |
| word-split | 13 | Leading/trailing word elision with non-whitespace IFS | stdout mismatch |
| word-split | 14 | Leading ' ' vs leading ' _ ' | stdout mismatch |
| word-split | 15 | Multiple non-whitespace IFS chars. | stdout mismatch |
| word-split | 16 | IFS with whitespace and non-whitepace. | stdout mismatch |
| word-split | 19 | unquoted whitespace arg is elided | stdout mismatch |
| word-split | 20 | empty literals are not elided | stdout mismatch |
| word-split | 22 | default value can yield multiple words | stdout mismatch |
| word-split | 23 | default value can yield multiple words with part joining | stdout mismatch |
| word-split | 24 | default value with unquoted IFS char | stdout mismatch |
| word-split | 26 | IFS unset behaves like $' \t\n' | stdout mismatch |
| word-split | 27 | IFS='\' | stdout mismatch |
| word-split | 28 | IFS='\ ' | stdout mismatch |
| word-split | 29 | IFS characters are glob metacharacters | stdout mismatch |
| word-split | 31 | Empty IFS (regression for bug) | status: expected 0, got 1; stdout mismatch |
| word-split | 32 | Unset IFS (regression for bug) | status: expected 0, got 1; stdout mismatch |
| word-split | 35 | IFS and joining arrays by assignments | stdout mismatch |
| word-split | 42 | Bug #628 split on : with : in literal word | stdout mismatch |
| word-split | 44 | Bug #1664, \\ with noglob | stdout mismatch |
| word-split | 47 | 4 x 3 table: (default IFS, IFS='', IFS=zx) x ( $* "$*" $@ "$@" ) | stdout mismatch |
| word-split | 48 | 4 x 3 table - with for loop | stdout mismatch |
| word-split | 52 | ""$A"" - empty string on both sides - derived from spec/toysh-posix #15 | status: expected 0, got 1; stdout mismatch |
| word-split | 53 | Regression: "${!v*}"x should not be split | status: expected 0, got 1; stdout mismatch |
| word-split | 54 | Regression: ${!v} should be split | stdout mismatch |
| word-split | 55 | Regression: "${v:-AxBxC}"x should not be split | stdout mismatch |
| var-sub | 4 | Filename redirect with "$@" | status: expected 1, got 0 |
| var-sub | 5 | Descriptor redirect to bad "$@" | status: expected 1, got 0 |
| var-sub | 6 | Here doc with bad "$@" delimiter | status: expected 2, got 1 |
| var-sub-quote | 4 | substitution of IFS character, quoted and unquoted | stdout mismatch |
| var-sub-quote | 11 | Multiple words: no quotes | stdout mismatch |
| var-sub-quote | 17 | Mixed inner quotes | stdout mismatch |
| var-sub-quote | 19 | part_value tree with multiple words | stdout mismatch |
| var-sub-quote | 21 | Var with multiple words: no quotes | stdout mismatch |
| var-sub-quote | 27 | No outer quotes, Multiple internal quotes | stdout mismatch |
| var-sub-quote | 28 | Strip a string with single quotes, unquoted | stdout mismatch |
| var-sub-quote | 31 | The string to strip can be single quoted, outer is unquoted | stdout mismatch |
| var-sub-quote | 32 | Syntax error for single quote in double quote | status: expected 2, got 0; stdout mismatch |
| var-sub-quote | 33 | "${undef-'c d'}" and "${foo%'c d'}" are parsed differently | stdout mismatch |
| var-sub-quote | 41 | Var substitution with \n in value | stdout mismatch |
| var-op-test | 4 | Unquoted with array as default value | stdout mismatch |
| var-op-test | 6 | Assign default with array | stdout mismatch |
| var-op-test | 16 | Nix idiom ${!hooksSlice+"${!hooksSlice}"} - was workaround for obsolete bash 4.3 bug | status: expected 0, got 1; stdout mismatch |
| var-op-test | 18 | array and - and + | status: expected 0, got 1; stdout mismatch |
| var-op-test | 19 | $@ (empty) and - and + | stdout mismatch |
| var-op-test | 20 | $@ ("") and - and + | stdout mismatch |
| var-op-test | 24 | assoc array and - and + | stdout mismatch |
| var-op-test | 29 | array ${arr[0]=x} | status: expected 0, got 1; stdout mismatch |
| var-op-test | 32 | "\e" as arg | stdout mismatch |
| var-op-test | 33 | op-test for ${a} and ${a[0]} | stdout mismatch |
| var-op-test | 34 | op-test for ${a[@]} and ${a[*]} | stdout mismatch |
| var-op-test | 35 | op-test for ${!array} with array="a" and array="a[0]" | status: expected 0, got 1; stdout mismatch |
| var-op-strip | 11 | Strip unicode prefix | stdout mismatch |
| var-op-strip | 16 | strip unquoted and quoted [ | status: expected 0, got 1; stdout mismatch |
| var-op-strip | 17 | strip unquoted and quoted [] | stdout mismatch |
| var-op-strip | 27 | Strip Right Brace (#702) | stdout mismatch |
| var-op-strip | 29 | extglob in pattern | stdout mismatch |
| assign | 10 | Env binding not allowed before compound command | status: expected 2, got 1 |
| assign | 11 | Trying to run keyword 'for' | status: expected 127, got 1 |
| assign | 16 | assignments / array assignments not interpreted after 'echo' | status: expected 0, got 1; stdout mismatch |
| assign | 21 | 'local x' and then array assignment | stdout mismatch |
| assign | 26 | Test above without 'local' (which is not POSIX) | stdout mismatch |
| assign | 30 | aliased assignment doesn't split | stdout mismatch |
| tilde | 6 | other user | status: expected 0, got 1; stdout mismatch |
| tilde | 10 | a[x]=foo:~ has tilde expansion | status: expected 0, got 1; stdout mismatch |

### Tier 2 — Builtins & Advanced

| Suite | # | Test | Reason |
|-------|---|------|--------|
| arith | 1 | Side Effect in Array Indexing | stdout mismatch |
| arith | 13 | Integer constant parsing | stdout mismatch |
| arith | 14 | Integer constant validation | stdout mismatch |
| arith | 20 | Increment and decrement array elements | stdout mismatch |
| arith | 30 | Array indexing in arith | stdout mismatch |
| arith | 32 | Constants in bases 2 to 64 | status: expected 0, got 1; stdout mismatch |
| arith | 35 | Octal constant | stdout mismatch |
| arith | 36 | Dynamic octal constant | stdout mismatch |
| arith | 50 | Negative exponent | status: expected 1, got 0; stdout mismatch |
| arith | 52 | Add integer to indexed array (a[0] decay) | status: expected 0, got 1; stdout mismatch |
| arith | 54 | Double subscript | stdout mismatch |
| arith | 55 | result of ArithSub -- array[0] decay | status: expected 0, got 1; stdout mismatch |
| arith | 56 | result of ArithSub -- assoc[0] decay | status: expected 0, got 1; stdout mismatch |
| arith | 57 | comma operator | status: expected 0, got 1; stdout mismatch |
| arith | 59 | array assignment with dynamic array name | stdout mismatch |
| arith | 61 | unary array assignment with dynamic var name | stdout mismatch |
| arith | 65 | 1 ? a=1 : b=2 ( bug fix) | status: expected 0, got 1; stdout mismatch |
| arith | 73 | s[0] with string 42 | stdout mismatch |
| glob | 5 | 0 char glob -- does NOT work | status: expected 0, got 1; stdout mismatch |
| glob | 6 | looks like glob at the start, but isn't | status: expected 0, got 1; stdout mismatch |
| glob | 7 | looks like glob plus negation at the start, but isn't | status: expected 0, got 1; stdout mismatch |
| glob | 9 | glob after var expansion | stdout mismatch |
| glob | 12 | glob after $@ expansion | stdout mismatch |
| glob | 13 | no glob after ~ expansion | status: expected 0, got 1; stdout mismatch |
| glob | 14 | store literal globs in array then expand | stdout mismatch |
| glob | 21 | Glob after var manipulation | stdout mismatch |
| glob | 22 | Glob after part joining | stdout mismatch |
| glob | 27 | Glob of unescaped [[] and []] | status: expected 0, got 1; stdout mismatch |
| glob | 33 | \ in unquoted substitutions does not match a backslash | stdout mismatch |
| glob | 37 | \ in unquoted substitutions escapes globchars | stdout mismatch |
| glob | 38 | pattern starting with . does not return . and .. | status: expected 0, got 1; stdout mismatch |
| glob | 39 | shopt -u globskipdots shows . and .. | status: expected 0, got 1; stdout mismatch |
| brace-expansion | 5 | } in expansion | status: expected 0, got 1; stdout mismatch |
| brace-expansion | 21 | Empty alternative | stdout mismatch |
| brace-expansion | 27 | no expansion with RHS assignment | status: expected 127, got 1 |
| brace-expansion | 37 | Descending number range expansion with negative step | stdout mismatch |
| brace-expansion | 39 | Singleton char ranges with steps | stdout mismatch |
| brace-expansion | 41 | Char range expansion with step | stdout mismatch |
| brace-expansion | 44 | Descending char range expansion | stdout mismatch |
| brace-expansion | 46 | Inconsistent fixed width number range expansion | stdout mismatch |
| brace-expansion | 53 | Side effect in expansion | stdout mismatch |
| brace-expansion | 55 | Invalid brace expansions mixing characters and numbers | stdout mismatch |
| case_ | 2 | Case statement with ;;& | status: expected 0, got 1; stdout mismatch |
| case_ | 6 | Match a literal with a glob character with a dynamic pattern | stdout mismatch |
| case_ | 13 | case \n bug regression | status: expected 2, got 1 |
| if_ | 5 | if break corner case | status: expected 0, got 1; stdout mismatch |
| loop | 14 | continue at top level | status: expected 0, got 1; stdout mismatch |
| loop | 23 | break/continue within source | stdout mismatch |
| loop | 29 | builtin,command break,continue,return,exit | status: expected 5, got 0; stdout mismatch |
| builtin-echo | 1 | echo dashes | stdout mismatch |
| builtin-echo | 2 | echo backslashes | stdout mismatch |
| builtin-echo | 4 | echo builtin should disallow typed args - literal | status: expected 2, got 1 |
| builtin-echo | 5 | echo builtin should disallow typed args - variable | status: expected 2, got 1 |
| builtin-echo | 7 | echo -ez (invalid flag) | stdout mismatch |
| builtin-echo | 22 | \x | stdout mismatch |
| builtin-printf | 1 | printf with no args | status: expected 2, got 1 |
| builtin-printf | 4 | printf -v a[1] | stdout mismatch |
| builtin-printf | 5 | printf -v syntax error | stdout mismatch |
| builtin-printf | 7 | dynamic declare instead of %q | status: expected 0, got 1; stdout mismatch |
| builtin-printf | 43 | printf negative numbers | stdout mismatch |
| builtin-printf | 46 | Runtime error for invalid integer | stdout mismatch |
| builtin-printf | 47 | %(strftime format)T | stdout mismatch |
| builtin-printf | 48 | %(strftime format)T doesn't respect TZ if not exported | stdout mismatch |
| builtin-printf | 49 | %(strftime format)T TZ in environ but not in shell's memory | stdout mismatch |
| builtin-printf | 50 | %10.5(strftime format)T | stdout mismatch |
| builtin-printf | 52 | bash truncates long strftime string at 128 | stdout mismatch |
| builtin-printf | 58 | printf %b supports octal escapes, both \141 and \0141 | stdout mismatch |
| builtin-printf | 59 | printf %b with truncated octal escapes | stdout mismatch |
| builtin-printf | 60 | printf %d %X support hex 0x5 and octal 055 | stdout mismatch |
| builtin-printf | 61 | printf %d with + prefix (positive sign) | stdout mismatch |
| builtin-printf | 62 | leading spaces are accepted in value given to %d %X, but not trailing spaces | stdout mismatch |
| builtin-read | 8 | read -n (with $REPLY) | stdout mismatch |
| builtin-read | 9 | IFS= read -n (OSH regression: value saved in tempenv) | stdout mismatch |
| builtin-read | 10 | read -n doesn't strip whitespace (bug fix) | stdout mismatch |
| builtin-read | 11 | read -d -n - respects delimiter and splits | stdout mismatch |
| builtin-read | 13 | read -n from pipe | stdout mismatch |
| builtin-read | 14 | read without args uses $REPLY, no splitting occurs (without -n) | stdout mismatch |
| builtin-read | 15 | read -n vs. -N | stdout mismatch |
| builtin-read | 16 | read -N ignores delimiters | stdout mismatch |
| builtin-read | 17 | read will unset extranous vars | stdout mismatch |
| builtin-read | 19 | read -r with other backslash escapes | stdout mismatch |
| builtin-read | 22 | read -r with \n | stdout mismatch |
| builtin-read | 23 | read -s from pipe, not a terminal | status: expected 0, got 1; stdout mismatch |
| builtin-read | 27 | read does not respect C backslash escapes | stdout mismatch |
| builtin-read | 29 | read -a reads into array | status: expected 0, got 2; stdout mismatch |
| builtin-read | 34 | read -t 0 tests if input is available | stdout mismatch |
| builtin-read | 41 | read -u 3 -d b -N 6 | stdout mismatch |
| builtin-read | 42 | read -N doesn't respect delimiter, while read -n does | status: expected 0, got 1; stdout mismatch |
| builtin-read | 43 | read -p (not fully tested) | stdout mismatch; stderr mismatch |
| builtin-read | 45 | read with smooshed args | stdout mismatch |
| builtin-read | 46 | read -r -d '' for NUL strings, e.g. find -print0 | stdout mismatch |
| builtin-read | 51 | read -n and backslash escape | stdout mismatch |
| builtin-read | 52 | read -n 4 with incomplete backslash | stdout mismatch |
| builtin-read | 53 | read -n 4 with backslash + delim | stdout mismatch |
| builtin-read | 54 | "backslash + newline" should be swallowed regardless of "-d <delim>" | stdout mismatch |
| builtin-read | 55 | empty input and splitting | stdout mismatch |
| builtin-read | 56 | IFS='x ' read -a: trailing spaces (unlimited split) | stdout mismatch |
| builtin-read | 58 | IFS='x ' read -a: intermediate spaces (unlimited split) | stdout mismatch |
| builtin-read | 60 | IFS='\ ' and backslash escaping | stdout mismatch |
| builtin-read | 62 | IFS=x read a b <<< xxxxxx | stdout mismatch |
| builtin-read | 63 | read and "\ " | stdout mismatch |
| builtin-cd | 5 | cd - without OLDPWD | stdout mismatch |
| builtin-cd | 6 | $OLDPWD | stdout mismatch |
| builtin-cd | 24 | CDPATH is respected | stdout mismatch |
| builtin-cd | 29 | arguments to pwd | status: expected 0, got 1 |
| builtin-set | 1 | can continue after unknown option | status: expected 0, got 1; stdout mismatch |
| builtin-set | 7 | set -u with undefined var in interactive shell does NOT exit the interpreter | stdout mismatch |
| builtin-set | 8 | set -u error can break out of nested evals | stdout mismatch |
| builtin-set | 14 | set - - and so forth | stdout mismatch |
| builtin-set | 15 | set - leading single dash is ignored, turns off xtrace verbose (#2364) | stdout mismatch |
| builtin-set | 16 | set - stops option processing like set -- | stdout mismatch |
| builtin-set | 17 | A single + is an ignored flag; not an argument | stdout mismatch |
| builtin-set | 18 | set - + and + - | stdout mismatch |
| builtin-type | 1 | type -> keyword builtin | stdout mismatch |
| builtin-type | 3 | type of relative path | stdout mismatch |
| builtin-type | 4 | type -> not found | stdout mismatch |
| builtin-trap | 4 | trap foo gives non-zero error | stdout mismatch |
| builtin-trap | 7 | print trap handler with multiple lines | stdout mismatch |
| builtin-trap | 8 | trap -p is like trap: it prints the handlers and full signal names | stdout mismatch |
| builtin-trap | 9 | Register the same handler for multiple signals | stdout mismatch |
| builtin-trap | 10 | Remove multiple handlers with trap - | stdout mismatch |
| builtin-trap | 11 | trap EXIT clears the EXIT trap | stdout mismatch |
| builtin-trap | 12 | trap 0 is equivalent to trap EXIT | stdout mismatch |
| builtin-trap | 15 | trap '' EXIT - printing state | stdout mismatch |
| builtin-trap | 20 | trap EXIT with PARSE error | status: expected 2, got 1; stdout mismatch |
| builtin-trap | 21 | trap EXIT with PARSE error and explicit exit | status: expected 0, got 1; stdout mismatch |
| builtin-trap | 28 | trap INT, sleep, SIGINT: non-interactively | stdout mismatch |
| builtin-trap | 30 | Remove trap with an unsigned integer | stdout mismatch |
| builtin-trap | 31 | trap '' sets handler to empty string (SIG_IGN) | stdout mismatch |
| builtin-trap | 32 | trap '' with multiple signals | stdout mismatch |
| builtin-bracket | 4 | -a as unary operator (alias of -e) | stdout mismatch |
| builtin-bracket | 14 | ( ) ! -a -o with system version of [ | stdout mismatch |
| builtin-bracket | 15 | == is alias for = | status: expected 0, got 1; stdout mismatch |
| builtin-bracket | 16 | == and = does not do glob | status: expected 0, got 1; stdout mismatch |
| builtin-bracket | 21 | -z '>' implies two token lookahead | status: expected 0, got 2; stdout mismatch |
| builtin-bracket | 40 | test -o for options | stdout mismatch |
| builtin-bracket | 50 | No octal, hex, base N conversion - leading 0 is a regular decimal | stdout mismatch |
| builtin-process | 5 | exec -- 2>&1 | status: expected 0, got 1; stdout mismatch |
| builtin-process | 21 | arg that would overflow 64 bits is detected | stdout mismatch |
| builtin-process | 24 | ulimit -S for soft limit (default), -H for hard limit | stdout mismatch |
| background | 2 | wait -n with arguments - arguments are respected | stdout mismatch |
| background | 8 | wait for N parallel jobs and check failure | status: expected 0, got 1; stdout mismatch |
| background | 11 | Start background pipeline, wait $pid | stdout mismatch |
| background | 13 | Wait for job and PIPESTATUS | stdout mismatch |
| background | 14 | Wait for job and PIPESTATUS - cat | stdout mismatch |
| background | 18 | wait -n | stdout mismatch |
| background | 22 | jobs prints one line per job | stdout mismatch |
| background | 23 | jobs -p prints one line per job | stdout mismatch |
| background | 27 | Signal message for killed background job | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 1 | Lower Case with , and ,, | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 2 | Upper Case with ^ and ^^ | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 3 | Case folding - Unicode characters | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 4 | Case folding - multi code point | status: expected 0, got 1 |
| var-op-bash | 5 | Case folding that depends on locale (not enabled, requires Turkish locale) | status: expected 0, got 1 |
| var-op-bash | 6 | Lower Case with constant string (VERY WEIRD) | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 7 | Lower Case glob | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 8 | ${x@u} U L - upper / lower case (bash 5.1 feature) | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 9 | ${x@Q} | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 10 | ${array@Q} and ${array[@]@Q} | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 11 | ${!prefix@} ${!prefix*} yields sorted array of var names | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 12 | ${!prefix@} matches var name (regression) | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 13 | ${var@a} for attributes | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 14 | ${var@a} error conditions | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 15 | undef and @P @Q @a | stdout mismatch |
| var-op-bash | 16 | argv array and @P @Q @a | stdout mismatch |
| var-op-bash | 17 | assoc array and @P @Q @a | stdout mismatch |
| var-op-bash | 21 | undef vs. empty string in var ops | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 23 | ${a[0]@a} and ${a@a} | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 24 | ${!r@a} with r='a[0]' (attribute for indirect expansion of an array element) | status: expected 0, got 1; stdout mismatch |
| var-op-bash | 27 | Array expansion with nullary var op @a | status: expected 0, got 1; stdout mismatch |
| var-op-slice | 2 | Cannot take length of substring slice | status: expected 1, got 0 |
| var-op-slice | 14 | Slice with an index that's an array -- silent a[0] decay | status: expected 0, got 1; stdout mismatch |
| var-op-slice | 15 | Slice with an assoc array | status: expected 0, got 1; stdout mismatch |
| var-op-slice | 17 | ${@:offset} and ${*:offset} | stdout mismatch |
| var-op-slice | 18 | ${@:offset:length} and ${*:offset:length} | stdout mismatch |
| var-op-slice | 22 | ${array[@]::} has implicit length of zero - for ble.sh | status: expected 0, got 1; stdout mismatch |
| assign-extended | 4 | declare -F prints function names | stdout mismatch |
| assign-extended | 6 | declare -F with shopt -s extdebug and main file | stdout mismatch |
| assign-extended | 13 | declare -p foo=bar doesn't make sense | stdout mismatch |
| assign-extended | 15 | declare -paA | stdout mismatch |
| assign-extended | 19 | ble.sh: eval -- "$(declare -p var arr)" | stdout mismatch |
| assign-extended | 20 | declare -p and value.Undef | stdout mismatch |
| assign-extended | 21 | eval -- "$(declare -p arr)" (restore arrays w/ unset elements) | stdout mismatch |
| assign-extended | 22 | declare -p UNDEF (and typeset) -- prints something to stderr | status: expected 0, got 1; stdout mismatch |
| assign-extended | 28 | Multiple assignments / array assignments on a line | status: expected 0, got 1; stdout mismatch |
| assign-extended | 32 | myvar=typeset (another form of dynamic assignment) | stdout mismatch |
| assign-extended | 37 | function name with / | status: expected 0, got 1; stdout mismatch |
| assign-extended | 39 | unset and shell funcs | status: expected 127, got 0; stdout mismatch |

## Failing Tests — gsh

Tests where gsh fails but bash passes.

### Tier 0 — Core

| Suite | # | Test | Reason |
|-------|---|------|--------|
| smoke | 15 | failed command | status: expected 2, got 0 |
| pipeline | 6 | Exit code is last status | status: expected 1, got 0 |
| pipeline | 12 | ! turns non-zero into zero | stdout mismatch |
| pipeline | 23 | SIGPIPE causes pipeline to die (regression for issue #295) | stdout mismatch |
| pipeline | 26 | shopt -s lastpipe and shopt -s no_last_fork interaction | stdout mismatch |
| redirect | 20 | Redirect to file descriptor that's not open | stdout mismatch |
| redirect | 30 | <> for read/write named pipes | status: expected 0, got -1; stdout mismatch |
| redirect-multi | 7 | File redirect to $var with glob char | status: expected 1, got 0 |
| redirect-multi | 12 | Redirect with brace expansion isn't allowed | stdout mismatch |
| redirect-multi | 13 | File redirects have word splitting too! | stdout mismatch |
| exit-status | 1 | Truncating 'exit' status | stdout mismatch |
| exit-status | 7 | If subshell true WITH OUTPUT is different | stdout mismatch |
| exit-status | 8 | If subshell true WITH ARGUMENT | stdout mismatch |

### Tier 1 — Expansion & Variables

| Suite | # | Test | Reason |
|-------|---|------|--------|
| word-eval | 4 | Word joining | stdout mismatch |
| var-op-test | 4 | Unquoted with array as default value | stdout mismatch |
| var-op-test | 6 | Assign default with array | stdout mismatch |
| var-op-test | 37 | op-test for unquoted ${a[*]:-empty} with IFS= | stdout mismatch |

### Tier 2 — Builtins & Advanced

| Suite | # | Test | Reason |
|-------|---|------|--------|
| arith | 14 | Integer constant validation | stdout mismatch |
| builtin-cd | 12 | 'unset PWD; pwd' before any cd (tickles a rare corner case) | stdout mismatch |
| builtin-cd | 13 | lie about PWD; pwd before any cd | stdout mismatch |
| builtin-cd | 15 | pwd in symlinked dir on shell initialization | stdout mismatch |
| builtin-cd | 16 | Test the current directory after 'cd ..' involving symlinks | stdout mismatch |
| builtin-cd | 26 | What happens when inherited $PWD and current dir disagree? | stdout mismatch |
| builtin-cd | 27 | Survey of getcwd() syscall | stdout mismatch |
| builtin-set | 6 | set -u with undefined variable exits the interpreter | stdout mismatch |
| builtin-set | 7 | set -u with undefined var in interactive shell does NOT exit the interpreter | stdout mismatch |
| builtin-set | 8 | set -u error can break out of nested evals | stdout mismatch |
| builtin-trap | 27 | trap USR1, sleep, SIGINT: non-interactively | stdout mismatch |
| builtin-trap | 28 | trap INT, sleep, SIGINT: non-interactively | stdout mismatch |
| builtin-trap | 29 | trap EXIT, sleep, SIGINT: non-interactively | stdout mismatch |
| builtin-trap | 30 | Remove trap with an unsigned integer | stdout mismatch |
| builtin-process | 23 | write big file with ulimit | stdout mismatch |
| builtin-process | 26 | ulimit -n limits file descriptors | stdout mismatch |
| background | 8 | wait for N parallel jobs and check failure | stdout mismatch |
| background | 27 | Signal message for killed background job | stdout mismatch |
| var-op-bash | 19 | ${#var@X} is a parse error | stdout mismatch |
| assign-extended | 6 | declare -F with shopt -s extdebug and main file | stdout mismatch |
| assign-extended | 22 | declare -p UNDEF (and typeset) -- prints something to stderr | stdout mismatch |

## Bonus: Tests where zsh passes but bash fails

| Suite | # | Test |
|-------|---|------|
| pipeline | 20 | bash/dash/mksh run the last command is run in its own process |
| redirect | 9 | Descriptor redirect with filename |
| redirect | 33 | can't mention big file descriptor |
| redirect-multi | 3 | ysh behavior when glob doesn't match |
| exit-status | 4 | func subshell OverflowError https://github.com/oilshell/oil/issues/996 |
| here-doc | 7 | Here doc with bad comsub delimiter |
| quote | 20 | $? split over multiple lines |
| quote | 21 | Unterminated single quote |
| quote | 22 | Unterminated double quote |
| word-split | 49 | IFS=x and '' and $@ - same bug as spec/toysh-posix case #12 |
| word-split | 50 | IFS=x and '' and $@ (#2) |
| var-sub | 1 | Bad var sub |
| var-sub-quote | 39 | Right Brace as argument (similar to #702) |
| var-op-test | 13 | "${array[@]} with set -u (bash is outlier) |
| var-op-test | 25 | Error when empty |
| var-op-test | 26 | Error when unset |
| var-op-strip | 26 | strip none unicode |
| var-op-len | 5 | String length with invalid utf-8 continuation bytes |
| var-op-len | 7 | Length of undefined variable with nounset |
| assign | 15 | Env binding in readonly/declare is NOT exported!  (pitfall) |
| assign | 17 | dynamic local variables (and splitting) |
| assign | 25 | Reveal existence of "temp frame" (All shells disagree here!!!) |
| assign | 27 | Using ${x-default} after unsetting local shadowing a global |
| assign | 28 | Using ${x-default} after unsetting a temp binding shadowing a global |
| assign | 31 | assignment using dynamic keyword (splits in most shells, not in zsh/osh) |
| assign | 32 | assignment using dynamic var names doesn't split |
| assign | 35 | readonly $x where x='b c' |
| assign | 41 | redirect after bare assignment |
| tilde | 4 | No tilde expansion in word that looks like assignment but isn't |
| arith | 8 | Constant with quotes like '1' |
| arith | 12 | Invalid string to int with strict_arith |
| arith | 41 | nounset with arithmetic |
| arith | 44 | Invalid LValue |
| arith | 51 | Comment not allowed in the middle of multiline arithmetic |
| arith | 66 | Invalid constant |
| arith | 69 | Negative numbers with bit shift |
| arith | 71 | undef[0] with nounset |
| glob | 20 | : escaped |
| glob | 32 | Glob ordering respects LC_COLLATE (zsh respects this too) |
| brace-expansion | 12 | double expansion with simple var -- bash bug |
| brace-expansion | 14 | double expansion with literal and simple var |
| brace-expansion | 18 | { in expansion |
| brace-expansion | 36 | Descending number range expansion with positive step is invalid |
| loop | 3 | for loop with invalid identifier |
| loop | 17 | bad arg to break |
| sh-func | 9 | return "" (a lot of disagreement) |
| sh-func | 12 | Scope of global variable when sourced in function (Shell Functions aren't Closures) |
| builtin-printf | 28 | Too large |
| builtin-printf | 33 | printf backslash escapes |
| builtin-printf | 34 | printf octal backslash escapes |
| builtin-printf | 35 | printf unicode backslash escapes |
| builtin-printf | 41 | printf %q |
| builtin-printf | 42 | printf %6q (width) |
| builtin-read | 12 | read -n with invalid arg |
| builtin-read | 38 | read -u syntax error |
| builtin-cd | 4 | cd with 2 or more args is allowed (strict_arg_parse disabled) |
| builtin-cd | 28 | chdir is a synonym for cd - busybox ash |
| builtin-cd | 30 | pwd errors out on args with strict_arg_parse |
| builtin-type | 6 | more special builtins |
| builtin-trap | 1 | traps are not active inside subshells $() ()  trap | cat |
| builtin-trap | 17 | exit 1 when trap code string is invalid |
| builtin-bracket | 31 | [ -t invalid ] |
| builtin-bracket | 43 | Overflow error |
| builtin-process | 9 | Exit builtin with too many args |
| builtin-process | 12 | ulimit too many args |
| builtin-process | 15 | ulimit -a doesn't take arg |
| builtin-process | 22 | ulimit -f 1 prevents files larger 512 bytes |
| command-parsing | 1 | Prefix env on assignment |
| var-op-slice | 10 | Slice undefined |
| var-op-slice | 12 | Slice string with invalid UTF-8 results in empty string and warning |
| var-op-slice | 13 | Slice string with invalid UTF-8 with strict_word_eval |
| assign-extended | 25 | typeset -r makes a string readonly |
| assign-extended | 26 | typeset -ar makes it readonly |
| assign-extended | 30 | syntax error in array assignment |
| assign-extended | 36 | typeset +r removes read-only attribute (TODO: documented in bash to do nothing) |

## Bonus: Tests where gsh passes but bash fails

| Suite | # | Test |
|-------|---|------|
| pipeline | 20 | bash/dash/mksh run the last command is run in its own process |
| redirect | 9 | Descriptor redirect with filename |
| redirect | 33 | can't mention big file descriptor |
| redirect-multi | 3 | ysh behavior when glob doesn't match |
| redirect-multi | 11 | Non-file redirects don't respect glob args (we differe from bash) |
| builtin-eval-source | 5 | eval YSH block with 'break continue return error' |
| command-sub | 29 | Syntax errors with double quotes within backticks |
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
| var-op-slice | 10 | Slice undefined |
| var-op-slice | 12 | Slice string with invalid UTF-8 results in empty string and warning |
| var-op-slice | 13 | Slice string with invalid UTF-8 with strict_word_eval |
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
