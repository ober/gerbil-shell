#!/bin/sh
# Compare bench.ss performance: gxi (interpreted) vs gsh (native .o1)
#
# Usage: ./test/run_bench.sh [path-to-gsh]
#
# Runs bench.ss under:
#   1. gxi — interpreted (no compilation)
#   2. gsh ,use — compiles to .o1 via embedded compile-file, then runs native
#
# Parses wall-clock times from stderr and prints a side-by-side comparison.

set -eu

GSH="${1:-.gerbil/bin/gsh}"
BENCH="$(cd "$(dirname "$0")/.." && pwd)/bench.ss"

if [ ! -x "$GSH" ]; then
  echo "error: gsh not found at $GSH (run 'make build' first)" >&2
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
echo "Running gsh ,use (native .o1)..." >&2
printf ',use %s\n' "$BENCH" | "$GSH" 2>"$TMPDIR/gsh.txt" || true

# --- Parse and compare ---
# Extract "label  Nms wall" lines, pull out label and wall ms
parse_times() {
  # Match: "  <label>  <N>ms wall ..."
  # Use non-greedy trick: match label up to 2+ spaces followed by digits+ms
  sed -n 's/^  \(.\{1,\}\)  \([0-9][0-9]*\)ms wall.*/\1|\2/p' "$1" \
    | sed 's/  *|/|/'
}

parse_times "$TMPDIR/gxi.txt" > "$TMPDIR/gxi.parsed"
parse_times "$TMPDIR/gsh.txt" > "$TMPDIR/gsh.parsed"

# Print table
printf '\n'
printf '%-18s %10s %10s %10s\n' "Benchmark" "gxi" "gsh .o1" "Speedup"
printf '%-18s %10s %10s %10s\n' "---------" "---" "-------" "-------"

while IFS='|' read -r label gxi_ms; do
  # Find matching line in gsh output
  gsh_ms=$(grep "^${label}|" "$TMPDIR/gsh.parsed" 2>/dev/null | head -1 | cut -d'|' -f2)
  if [ -z "$gsh_ms" ]; then
    gsh_ms="?"
    speedup="?"
  elif [ "$gsh_ms" -eq 0 ]; then
    speedup="inf"
  else
    # Integer division for speedup ratio
    ratio=$((gxi_ms / gsh_ms))
    remainder=$(( (gxi_ms % gsh_ms) * 10 / gsh_ms ))
    speedup="${ratio}.${remainder}x"
  fi
  printf '%-18s %8sms %8sms %10s\n' "$label" "$gxi_ms" "$gsh_ms" "$speedup"
done < "$TMPDIR/gxi.parsed"

printf '\n'
