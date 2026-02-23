#!/bin/bash
# Test script for ,use with no gsc/gxc available.
# Usage: rename /opt/gerbil out of the way, then run:
#   ./test-nogsc.sh [path-to-gsh]
#
# Tests that gsh can compile .ss → .scm/.ssi and load interpreted.

set -e

GSH="${1:-./static/gsh}"
TMPDIR=$(mktemp -d /tmp/test-nogsc.XXXXXX)
trap "rm -rf $TMPDIR" EXIT

# Verify gsc is NOT reachable
if command -v gsc &>/dev/null; then
  echo "FAIL: gsc is still on PATH — rename /opt/gerbil first"
  exit 1
fi
if [ -f /opt/gerbil/bin/gsc ]; then
  echo "FAIL: /opt/gerbil/bin/gsc still exists — rename /opt/gerbil first"
  exit 1
fi

echo "--- Using gsh: $GSH"
echo "--- Temp dir:  $TMPDIR"

# Write test module
cat > "$TMPDIR/testmod.ss" <<'SCHEME'
(export greet add-nums make-greeting)
(def (greet name) (string-append "Hello, " name "!"))
(def (add-nums a b) (+ a b))
(def (make-greeting prefix name)
  (string-append prefix ", " name "!"))
SCHEME

# Test 1: ,use compiles and loads, qualified names work
echo ""
echo "=== Test 1: ,use + qualified name calls ==="
OUTPUT=$(printf ',use %s/testmod.ss\n,(testmod#greet "World")\n,(testmod#add-nums 17 25)\n,(testmod#make-greeting "Hey" "gsh")\n' "$TMPDIR" | "$GSH" 2>&1)
echo "$OUTPUT"

echo "$OUTPUT" | grep -q 'compiled:' || { echo "FAIL: no compiled message"; exit 1; }
echo "$OUTPUT" | grep -q 'loaded:' || { echo "FAIL: no loaded message"; exit 1; }
echo "$OUTPUT" | grep -q '"Hello, World!"' || { echo "FAIL: greet returned wrong value"; exit 1; }
echo "$OUTPUT" | grep -q '42' || { echo "FAIL: add-nums returned wrong value"; exit 1; }
echo "$OUTPUT" | grep -q '"Hey, gsh!"' || { echo "FAIL: make-greeting returned wrong value"; exit 1; }
echo "PASS"

# Test 2: Verify .scm files were generated (no .o1)
echo ""
echo "=== Test 2: .scm generated, no .o1 ==="
if ls "$TMPDIR"/testmod*.scm &>/dev/null; then
  echo "  .scm files: present"
else
  echo "FAIL: no .scm files generated"; exit 1
fi
if ls "$TMPDIR"/testmod*.o1 &>/dev/null; then
  echo "  .o1 files: present (gsc was found somehow!)"
  echo "WARN: native compilation worked — gsc may be embedded"
else
  echo "  .o1 files: absent (expected without gsc)"
fi
echo "PASS"

# Test 3: ,load on already-compiled module
echo ""
echo "=== Test 3: ,load already-compiled module ==="
OUTPUT=$(printf ',load testmod %s\n,(testmod#greet "Again")\n' "$TMPDIR" | "$GSH" 2>&1)
echo "$OUTPUT"
echo "$OUTPUT" | grep -q 'loaded:' || { echo "FAIL: no loaded message"; exit 1; }
echo "$OUTPUT" | grep -q '"Hello, Again!"' || { echo "FAIL: greet returned wrong value"; exit 1; }
echo "PASS"

# Test 4: ,use with a module that has multiple definitions
echo ""
echo "=== Test 4: larger module ==="
cat > "$TMPDIR/mathmod.ss" <<'SCHEME'
(export square cube factorial)
(def (square x) (* x x))
(def (cube x) (* x x x))
(def (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))
SCHEME

OUTPUT=$(printf ',use %s/mathmod.ss\n,(mathmod#square 7)\n,(mathmod#cube 3)\n,(mathmod#factorial 10)\n' "$TMPDIR" | "$GSH" 2>&1)
echo "$OUTPUT"
echo "$OUTPUT" | grep -q '49' || { echo "FAIL: square 7 != 49"; exit 1; }
echo "$OUTPUT" | grep -q '27' || { echo "FAIL: cube 3 != 27"; exit 1; }
echo "$OUTPUT" | grep -q '3628800' || { echo "FAIL: factorial 10 != 3628800"; exit 1; }
echo "PASS"

echo ""
echo "=== All tests passed ==="
