# Prompt Customization in gsh

## Overview

gsh uses the `PS1` environment variable for the primary prompt, similar to bash. However, **ANSI escape sequences are NOT supported**. Instead, use the portable `tput` command for colors and formatting.

## Why tput instead of ANSI escapes?

- **Portable**: Works across all terminal emulators
- **Correct**: Queries terminfo database for the right sequences
- **Maintainable**: No hardcoded escape codes like `\[\033[32m\]`
- **Simpler**: No need for bash-specific `\[` and `\]` delimiters

## Supported Prompt Escapes

gsh supports the following backslash escapes in PS1:

| Escape | Description |
|--------|-------------|
| `\u` | Username |
| `\h` | Hostname (short) |
| `\H` | Hostname (full) |
| `\w` | Current working directory (with ~ for $HOME) |
| `\W` | Basename of current working directory |
| `\$` | `#` if root, `$` otherwise |
| `\n` | Newline |
| `\r` | Carriage return |
| `\a` | Bell character |
| `\\` | Literal backslash |
| `\d` | Date |
| `\t` | Time (24h HH:MM:SS) |
| `\T` | Time (12h HH:MM:SS) |
| `\@` | Time (12h am/pm) |
| `\A` | Time (24h HH:MM) |
| `\j` | Number of background jobs |
| `\#` | Command number |
| `\!` | History number |
| `\s` | Shell name (`gsh`) |
| `\v` | Shell version (short) |
| `\V` | Shell version (full) |

## Command Substitution

You can use `$(command)` in PS1 for dynamic content:

```bash
PS1='\u@\h:\w $(git branch 2>/dev/null | grep "^\*" | cut -d" " -f2)\$ '
```

## Color Examples

### Basic Two-Line Prompt with Colors

```bash
# Set colors using tput
FG_GREEN=$(tput setaf 2)
FG_CYAN=$(tput setaf 6)
RESET=$(tput sgr0)

# Two-line prompt: user@host:path on line 1, $ on line 2
PS1="${FG_GREEN}\u@\h:\w${RESET}
${FG_CYAN}\$ ${RESET}"
export PS1
```

### Prompt with Git Branch

```bash
# Colors
FG_GREEN=$(tput setaf 2)
FG_BLUE=$(tput setaf 4)
FG_YELLOW=$(tput setaf 3)
FG_CYAN=$(tput setaf 6)
RESET=$(tput sgr0)

# Function to get git branch
get_git_branch() {
  git branch 2>/dev/null | grep '^\*' | sed 's/\* //'
}

# Prompt with git branch
PS1="${FG_GREEN}\u@\h${RESET}:${FG_BLUE}\w${RESET}\$(
  branch=\$(get_git_branch)
  if [ -n \"\$branch\" ]; then
    printf \" ${FG_YELLOW}[\$branch]${RESET}\"
  fi
)
${FG_CYAN}\$ ${RESET}"
export PS1
```

### Simple Single-Line Prompt

```bash
FG_CYAN=$(tput setaf 6)
RESET=$(tput sgr0)

PS1="${FG_CYAN}\u@\h:\w\$ ${RESET}"
export PS1
```

## tput Color Reference

### Foreground Colors
```bash
tput setaf 0   # Black
tput setaf 1   # Red
tput setaf 2   # Green
tput setaf 3   # Yellow
tput setaf 4   # Blue
tput setaf 5   # Magenta
tput setaf 6   # Cyan
tput setaf 7   # White
```

### Background Colors
```bash
tput setab 0   # Black background
tput setab 1   # Red background
# ... etc (same numbers as setaf)
```

### Text Formatting
```bash
tput bold      # Bold text
tput dim       # Dim text
tput smul      # Underline start
tput rmul      # Underline end
tput rev       # Reverse video
tput sgr0      # Reset all attributes
```

## Configuration File

Place your prompt customization in `~/.gshrc`:

```bash
# ~/.gshrc example
FG_GREEN=$(tput setaf 2)
FG_CYAN=$(tput setaf 6)
RESET=$(tput sgr0)

PS1="${FG_GREEN}\u@\h:\w${RESET}
${FG_CYAN}\$ ${RESET}"
export PS1
```

## NOT Supported

The following bash-specific features are **NOT supported**:

- ❌ ANSI escape sequences (`\e[32m`, `\033[32m`, `\x1b[32m`)
- ❌ Non-printing delimiters (`\[`, `\]`)
- ❌ Octal escapes (`\0nnn`)
- ❌ Hex escapes (`\xNN`)

**Use `tput` instead!**
