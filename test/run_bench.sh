#!/bin/sh
# Compare bench.ss performance across runtimes
#
# Usage: ./test/run_bench.sh [path-to-gsh]
#
# Runs bench.ss under:
#   1. gxi — interpreted (no compilation)
#   2. gsh ,use — compiles to .o1 via embedded compile-file, then runs native
#   3. static gsh ,use — compiles to .o1 but loads .scm (no dlopen)
#
# Parses wall-clock times from stderr and prints a side-by-side comparison.

set -eu

STATIC_GSH=static/gsh
BENCH="$(cd "$(dirname "$0")/.." && pwd)/bench.ss"

# Find dynamic gsh: explicit arg, .gerbil/bin, or PATH
if [ -n "${1:-}" ]; then
  GSH="$1"
elif [ -x .gerbil/bin/gsh ]; then
  GSH=.gerbil/bin/gsh
elif command -v gsh >/dev/null 2>&1; then
  GSH=$(command -v gsh)
else
  GSH=""
fi

# Auto-detect Gerbil home for static binary's -:~~ flag
GERBIL_HOME_PATH="${GERBIL_HOME:-$(gxi -e '(display (path-expand "~~"))' 2>/dev/null || echo "")}"

if [ -z "$GSH" ] && [ ! -x "$STATIC_GSH" ]; then
  echo "error: no gsh found (run 'make build' or 'make static')" >&2
  exit 1
fi
if [ ! -f "$BENCH" ]; then
  echo "error: bench.ss not found at $BENCH" >&2
  exit 1
fi

TMPDIR=$(mktemp -d /tmp/bench-compare.XXXXXX)
trap "rm -rf $TMPDIR" EXIT

# --- Run gxi (interpreted) ---
echo "Running gxi (interpreted)..." >&2
gxi -e '(import :std/sugar :std/format :std/srfi/1)' \
    -e "(load \"$BENCH\")" \
    </dev/null 2>"$TMPDIR/gxi.txt" || true

# --- Run gsh ,use (native .o1) ---
HAS_DYNAMIC=false
if [ -n "$GSH" ] && [ -x "$GSH" ]; then
  HAS_DYNAMIC=true
  echo "Running gsh ,use (native .o1)..." >&2
  printf ',use %s\n' "$BENCH" | "$GSH" 2>"$TMPDIR/gsh.txt" || true
else
  echo "Skipping dynamic gsh (not found)" >&2
fi

# --- Run static gsh ,use (.scm interpreted) ---
HAS_STATIC=false
if [ -x "$STATIC_GSH" ] && [ -n "$GERBIL_HOME_PATH" ]; then
  HAS_STATIC=true
  echo "Running static gsh ,use (.scm eval)..." >&2
  printf ',use %s\n' "$BENCH" \
    | "$STATIC_GSH" "-:~~=$GERBIL_HOME_PATH" 2>"$TMPDIR/static.txt" || true
else
  if [ ! -x "$STATIC_GSH" ]; then
    echo "Skipping static gsh (not found at $STATIC_GSH)" >&2
  else
    echo "Skipping static gsh (cannot detect GERBIL_HOME for -:~~ flag)" >&2
  fi
fi

# --- Parse and compare ---
# Extract "label  Nms wall" lines, pull out label and wall ms
parse_times() {
  sed -n 's/^  \(.\{1,\}\)  \([0-9][0-9]*\)ms wall.*/\1|\2/p' "$1" \
    | sed 's/  *|/|/'
}

parse_times "$TMPDIR/gxi.txt" > "$TMPDIR/gxi.parsed"
if $HAS_DYNAMIC; then
  parse_times "$TMPDIR/gsh.txt" > "$TMPDIR/gsh.parsed"
fi
if $HAS_STATIC; then
  parse_times "$TMPDIR/static.txt" > "$TMPDIR/static.parsed"
fi

# Helper: compute speedup string (base_ms / target_ms)
calc_speedup() {
  _base="$1" _target="$2"
  if [ -z "$_target" ] || [ "$_target" = "?" ]; then echo "?"; return; fi
  if [ "$_target" -eq 0 ]; then echo "inf"; return; fi
  _ratio=$((_base / _target))
  _rem=$(( (_base % _target) * 10 / _target ))
  echo "${_ratio}.${_rem}x"
}

# Print table header based on available runners
printf '\n'
if $HAS_DYNAMIC && $HAS_STATIC; then
  printf '%-16s %8s %8s %8s %9s %9s\n' \
    "Benchmark" "gxi" "gsh .o1" "static" "gxi/gsh" "gxi/static"
  printf '%-16s %8s %8s %8s %9s %9s\n' \
    "---------" "---" "-------" "------" "-------" "----------"
elif $HAS_DYNAMIC; then
  printf '%-16s %8s %8s %9s\n' "Benchmark" "gxi" "gsh .o1" "Speedup"
  printf '%-16s %8s %8s %9s\n' "---------" "---" "-------" "-------"
elif $HAS_STATIC; then
  printf '%-16s %8s %8s %9s\n' "Benchmark" "gxi" "static" "gxi/static"
  printf '%-16s %8s %8s %9s\n' "---------" "---" "------" "----------"
fi

while IFS='|' read -r label gxi_ms; do
  if $HAS_DYNAMIC; then
    gsh_ms=$(grep "^${label}|" "$TMPDIR/gsh.parsed" 2>/dev/null | head -1 | cut -d'|' -f2)
    : "${gsh_ms:=?}"
    gsh_speedup=$(calc_speedup "$gxi_ms" "$gsh_ms")
  fi
  if $HAS_STATIC; then
    static_ms=$(grep "^${label}|" "$TMPDIR/static.parsed" 2>/dev/null | head -1 | cut -d'|' -f2)
    : "${static_ms:=?}"
    static_speedup=$(calc_speedup "$gxi_ms" "$static_ms")
  fi

  if $HAS_DYNAMIC && $HAS_STATIC; then
    printf '%-16s %6sms %6sms %6sms %9s %9s\n' \
      "$label" "$gxi_ms" "$gsh_ms" "$static_ms" "$gsh_speedup" "$static_speedup"
  elif $HAS_DYNAMIC; then
    printf '%-16s %6sms %6sms %9s\n' "$label" "$gxi_ms" "$gsh_ms" "$gsh_speedup"
  elif $HAS_STATIC; then
    printf '%-16s %6sms %6sms %9s\n' "$label" "$gxi_ms" "$static_ms" "$static_speedup"
  fi
done < "$TMPDIR/gxi.parsed"

printf '\n'
