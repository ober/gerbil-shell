# gsh - Gerbil Shell

A POSIX-compatible shell written in [Gerbil Scheme](https://cons.io/), with bash extensions and an embedded Scheme REPL.

gsh aims for practical bash compatibility while running on the Gambit VM, giving you the full Gerbil standard library at the shell prompt via the `,` (comma) meta-command.

## Features

### Shell Fundamentals
- Pipelines, redirects, command substitution, process substitution
- And-or lists (`&&`, `||`), subshells, brace groups
- Compound commands: `if`, `for`, `while`, `until`, `case`, `select`
- Functions with local scope, `return`, and `break`/`continue` with nesting levels
- Full I/O redirection: `<`, `>`, `>>`, `2>&1`, `<>`, `<&-`, `>&-`, here-documents
- Background jobs with `&`, job control with `fg`, `bg`, `jobs`, `wait`
- Signal handling with `trap`

### Bash Extensions
- Brace expansion: `{a,b,c}`, `{1..10}`, `{1..10..2}`, nested, zero-padded
- Extended globs: `@(pat)`, `?(pat)`, `*(pat)`, `+(pat)`, `!(pat)` (with `shopt -s extglob`)
- POSIX character classes: `[[:alpha:]]`, `[[:digit:]]`, etc.
- `[[ ]]` extended conditionals and `(( ))` arithmetic commands
- C-style for loops: `for ((i=0; i<10; i++))`
- Process substitution: `<(cmd)` and `>(cmd)`
- Nameref variables: `declare -n ref=target`
- Indirect expansion: `${!VAR}`
- Substring extraction: `${VAR:offset:length}`
- `$'...'` ANSI-C quoting with escape sequences
- Indexed arrays: `arr=(a b c)`, `${arr[@]}`, `${#arr[@]}`
- `printf` with `-v var` for storing results
- `read` with `-e` (line editing), `-t` (timeout), `-d` (delimiter), `-N` (byte count)
- `mapfile`/`readarray` for bulk line reading
- History expansion: `!!`, `!N`, `!string`, `^old^new`

### Interactive Features
- Custom line editor with emacs-style keybindings (no readline dependency)
- Tab completion for commands, filenames, and variables
- Command history with search (`Ctrl-R`)
- Prompt customization with `\u`, `\h`, `\w`, `$()`, and [34 built-in color variables](docs/colors.md)

### Scheme Integration
- `,` (comma) prefix evaluates Gerbil Scheme expressions at the prompt
- Full access to the Gerbil standard library (`import`, `define`, etc.)
- Definitions persist across commands within a session
- See [docs/scheme-eval.md](docs/scheme-eval.md)

## Quick Start

```bash
# Build
make build

# Run
.gerbil/bin/gsh

# Or install to ~/.local/bin
make install
gsh
```

## Requirements

- [Gerbil Scheme](https://cons.io/) v0.18.1 or later (with Gambit v4.9.7+)
- [gerbil-pcre2](https://github.com/ober/gerbil-pcre2) (`gerbil pkg install github.com/ober/gerbil-pcre2`)
- libpcre2 (`apt install libpcre2-dev` or equivalent)

## Configuration

### Startup Files

| Shell Mode | Files Sourced |
|---|---|
| Interactive login | `/etc/profile`, then first of: `~/.gsh_profile`, `~/.gsh_login`, `~/.profile` |
| Interactive non-login | `~/.gshrc` |
| Non-interactive (script) | `$GSH_ENV` (if set) |
| Login logout | `~/.gsh_logout` |

### Prompt Customization

gsh supports standard prompt escape sequences in `PS1`, `PS2`, and `PS4`:

| Escape | Description |
|---|---|
| `\u` | Username |
| `\h` | Short hostname |
| `\H` | Full hostname |
| `\w` | Working directory (`~` for home) |
| `\W` | Basename of working directory |
| `\d` | Date (e.g., `Thu Feb 20`) |
| `\t` | 24-hour time `HH:MM:SS` |
| `\T` | 12-hour time `HH:MM:SS` |
| `\@` | 12-hour time with AM/PM |
| `\A` | 24-hour time `HH:MM` |
| `\j` | Number of active jobs |
| `\#` | Command number |
| `\!` | History number |
| `\$` | `#` if root, `$` otherwise |
| `\s` | Shell name (`gsh`) |
| `\v` | Version (`0.1`) |
| `\V` | Full version (`0.1.0`) |
| `\l` | Terminal basename |
| `\n` | Newline |
| `\[...\]` | Non-printing character brackets |
| `$(cmd)` | Command substitution |

### Color Variables

34 ANSI color variables are available automatically. Use them in prompts and scripts:

```bash
# ~/.gshrc — colored prompt
PS1='\[$_fg_norm_green\]\u@\h\[$_ansi_reset\]:\[$_fg_norm_blue\]\w\[$_ansi_reset\]\$ '
```

Available variable groups:

| Group | Variables | Codes |
|---|---|---|
| Attributes | `_ansi_reset`, `_ansi_bold` | Reset, bold |
| Foreground | `_fg_norm_{color}` | Standard 8 colors |
| Foreground bright | `_fg_bright_{color}` | Bright 8 colors |
| Background | `_bg_norm_{color}` | Standard 8 colors |
| Background bright | `_bg_bright_{color}` | Bright 8 colors |

Colors: `black_`, `red`, `green`, `yellow`, `blue`, `magenta`, `cyan`, `white`

See [docs/colors.md](docs/colors.md) for the full reference.

### Line Editing Keybindings

| Key | Action |
|---|---|
| `Ctrl-A` | Move to start of line |
| `Ctrl-E` | Move to end of line |
| `Ctrl-F` / `Right` | Move forward one character |
| `Ctrl-B` / `Left` | Move backward one character |
| `Ctrl-K` | Kill to end of line |
| `Ctrl-U` | Kill entire line |
| `Ctrl-D` | Delete character (or EOF on empty line) |
| `Ctrl-H` / `Backspace` | Delete previous character |
| `Ctrl-P` / `Up` | Previous history |
| `Ctrl-N` / `Down` | Next history |
| `Ctrl-R` | Reverse history search |
| `Tab` | Filename/command completion |

## Builtin Commands

### I/O
- `echo [-neE] [args...]` — print arguments
- `printf [-v var] format [args...]` — formatted output
- `read [-erdnNstp] [var...]` — read input

### Filesystem
- `cd [-LP] [dir]` — change directory (supports `CDPATH`)
- `pwd [-LP]` — print working directory
- `pushd [dir]` / `popd` / `dirs` — directory stack

### Variables
- `export [-p] [var=val...]` — export to environment
- `declare [-afinprx] [var=val...]` — declare with attributes
- `local [var=val...]` — function-local variables
- `readonly [-p] [var=val...]` — mark as readonly
- `unset [-fn] var...` — remove variables or functions
- `let expr` — arithmetic evaluation

### Shell Control
- `set [-eCeuo] [args...]` — shell options and positional params
- `shopt [-psu] [opt...]` — extended shell options
- `eval string` — execute string as command
- `exec [cmd]` — replace shell or set up redirects
- `source file` / `. file` — execute file in current shell
- `exit [code]` — exit shell
- `return [code]` — return from function

### Job Control
- `jobs [-lnpr]` — list background jobs
- `fg [%job]` — bring to foreground
- `bg [%job]` — resume in background
- `wait [-n] [pid...]` — wait for jobs
- `kill [-signal] pid...` — send signal
- `trap [action] signal...` — signal handlers

### Other
- `type [-afptP] cmd` — show command type
- `command [-pv] cmd` — run external command
- `builtin cmd` — run builtin only
- `alias [-p] [name=val...]` / `unalias [-a] name...` — aliases
- `test expr` / `[ expr ]` — condition evaluation
- `history [-wrac] [n]` — history management
- `shift [n]` — shift positional params
- `umask [mask]` — file creation mask
- `mapfile [-d delim] [-n count] [-s skip] [-t] array` — read lines into array
- `true`, `false`, `:` — exit status constants

## Parameter Expansion

```bash
${VAR:-default}       # Use default if unset/empty
${VAR:=default}       # Set and use default if unset/empty
${VAR:+alternate}     # Use alternate if set/non-empty
${VAR:?error}         # Error if unset/empty
${#VAR}               # String length
${VAR:offset:length}  # Substring
${VAR#pattern}        # Remove shortest prefix
${VAR##pattern}       # Remove longest prefix
${VAR%pattern}        # Remove shortest suffix
${VAR%%pattern}       # Remove longest suffix
${VAR/pat/replace}    # Replace first match
${VAR//pat/replace}   # Replace all matches
${!VAR}               # Indirect expansion
```

## Arithmetic

Full C-like arithmetic in `$(( ))` and `(( ))`:

```bash
echo $((2 ** 10))         # 1024
echo $((0xff))            # 255 (hex)
((count++))               # Increment
for ((i=0; i<5; i++)); do echo $i; done
```

Operators: `+` `-` `*` `/` `%` `**` `++` `--` `<<` `>>` `&` `|` `^` `~` `&&` `||` `!` `<` `>` `<=` `>=` `==` `!=` `?:` `=` `+=` `-=` `*=` `/=` `%=`

## Scheme at the Prompt

Prefix any line with `,` to evaluate Gerbil Scheme:

```
gsh$ ,(+ 1 2 3 4 5)
15
gsh$ ,(map (lambda (x) (* x x)) '(1 2 3 4 5))
(1 4 9 16 25)
gsh$ ,(import :std/text/json)
gsh$ ,(string->json-object "{\"key\": \"value\"}")
```

See [docs/scheme-eval.md](docs/scheme-eval.md) for details.

## Testing

gsh is tested against the [Oils](https://www.oilshell.org/) project's spec test suite:

```bash
make compat-smoke     # Quick sanity check
make compat-tier0     # Pipes, redirects, command substitution
make compat-tier1     # Job control, traps, variables, functions
make compat-tier2     # Brace expansion, arithmetic, extglob
make compat           # All tiers
make compat-one SPEC=smoke        # Run a specific spec
make compat-debug SPEC=smoke      # Verbose per-test output
```

## Project Structure

```
main.ss           Entry point, REPL, argument parsing
startup.ss        RC file loading
environment.ss    Variables, scoping, arrays, namerefs
lexer.ss          Tokenizer (quotes, heredoc, alias, extglob)
parser.ss         Recursive-descent parser producing AST
ast.ss            AST node definitions
expander.ss       Word expansion (brace, tilde, param, glob, etc.)
glob.ss           Pathname expansion and pattern matching
arithmetic.ss     C-like arithmetic evaluator
executor.ss       Command dispatch and process execution
pipeline.ss       Pipeline construction and fd wiring
redirect.ss       I/O redirection and here-documents
control.ss        Compound commands (if, for, while, case, select)
builtins.ss       ~35 builtin commands
functions.ss      User-defined function management
jobs.ss           Job table and job control (fg, bg, wait)
signals.ss        Signal handling and trap execution
lineedit.ss       Custom line editor (emacs-style)
completion.ss     Tab completion (commands, files, variables)
prompt.ss         Prompt escape sequence expansion
history.ss        Command history and history expansion
script.ss         Script/string execution
ffi.ss            POSIX FFI bindings (waitpid, dup2, setpgid, etc.)
util.ss           String helpers, path operations
```

## License

MIT
