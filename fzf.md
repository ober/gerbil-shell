# Native fzf Capabilities for gsh

## Overview

Built-in fuzzy finder for gsh — no external fzf dependency required.
All matching, scoring, and interactive TUI are native Gerbil Scheme.

## Architecture

```
fuzzy.ss          — Scoring engine + extended search syntax parser
fzf.ss            — Interactive TUI selector (full-screen, ANSI terminal)
history.ss        — Enhanced with timestamps + CWD (backward-compatible)
lineedit.ss       — Ctrl-R/Ctrl-T/Alt-C keybindings → fzf.ss
```

## Modules

### fuzzy.ss — Matching Engine

**Scoring algorithm** (ported from fzf's V1 greedy algorithm):
- Case-insensitive by default, case-sensitive if query has uppercase
- Boundary bonus: +8 (after `/`, `-`, `_`, `.`, space)
- CamelCase bonus: +7 (lowercase→uppercase transition)
- Consecutive bonus: +4 (adjacent matches)
- First char bonus: +2 (match at position 0)
- Gap penalty: -3 first gap, -1 subsequent gaps

**Extended search syntax** (fzf-compatible):
- `foo`     — fuzzy match
- `'exact`  — exact substring match
- `^prefix` — prefix match
- `suffix$` — suffix match
- `!negate` — exclude matches
- `a | b`   — OR (any term matches)
- `a b`     — AND (all space-separated terms must match)

### fzf.ss — Interactive TUI

Full-screen terminal UI rendered via ANSI escape codes:
- Bottom-up layout (like fzf): prompt at bottom, candidates above
- Input line with live filtering
- Highlighted matches in candidate list
- Scrollable candidate window
- Status line: match count / total

**Keybindings:**
- `Enter` / `Ctrl-J` — accept selection
- `Ctrl-C` / `Esc` — cancel
- `Up` / `Ctrl-P` — previous candidate
- `Down` / `Ctrl-N` — next candidate
- `Ctrl-U` — clear query
- Typing updates filter in real-time

### history.ss — Enhanced History

**New file format:** `EPOCH\tCWD\tCOMMAND`
- Tab-separated: Unix epoch, working directory, command text
- Backward-compatible: old plain-text entries loaded as (0, "", command)
- New entries written with timestamp and CWD

**New APIs:**
- `history-entry-timestamp` — get epoch for an entry
- `history-entry-cwd` — get working directory for an entry
- `history-search-fuzzy` — fuzzy search using fuzzy.ss

### lineedit.ss — Keybinding Integration

| Key    | Action                  | Replaces         |
|--------|-------------------------|------------------|
| Ctrl-R | Fuzzy history search    | Substring search |
| Ctrl-T | Recursive file finder   | transpose-chars  |
| Alt-C  | Fuzzy directory jump    | capitalize-word  |

## Implementation Status

- [x] fuzzy.ss — matching engine
- [x] fzf.ss — interactive TUI
- [x] history.ss — timestamps + CWD
- [x] lineedit.ss — keybinding wiring
- [x] build.ss — compilation order
