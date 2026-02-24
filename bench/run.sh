#!/usr/bin/env bash
# bench/run.sh — Compare bash+traditional-tools vs gsh+native-scheme
#
# The thesis: microbenchmarks favor bash (tight C interpreter), but real-world
# I/O workloads tell a different story. gsh's embedded Scheme lets you replace
# multi-process pipelines with a single-process solution.
#
# Requirements: hyperfine (https://github.com/sharkdp/hyperfine)
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
DATA="$ROOT/bench/data"
GSH="$ROOT/.gerbil/bin/gsh"
WARMUP=2
RUNS=5

if ! command -v hyperfine &>/dev/null; then
  echo "ERROR: hyperfine not found. Install from https://github.com/sharkdp/hyperfine" >&2
  exit 1
fi
if [ ! -x "$GSH" ]; then
  echo "ERROR: gsh not found at $GSH — run 'make build' first" >&2
  exit 1
fi

# Generate data if missing
if [ ! -f "$DATA/sales.csv" ]; then
  echo "==> Generating test data..."
  bash "$ROOT/bench/generate-data.sh"
fi

echo "========================================"
echo " gsh vs bash — Real-World I/O Benchmarks"
echo "========================================"
echo ""
echo "Data: $(wc -l < "$DATA/sales.csv") rows CSV, $(wc -l < "$DATA/access.log") lines log, $(du -h "$DATA/words.txt" | cut -f1) text"
echo "gsh:  $GSH"
echo "bash: $(which bash)"
echo ""

# --- Helper: create temp scripts to avoid quoting hell ---
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

write_script() {
  local f="$TMP/$1"
  cat > "$f"
  chmod +x "$f"
  echo "$f"
}

# ============================================================
# Benchmark 1: Startup Time
# ============================================================
echo "### 1. Startup Time ###"
echo "  Measures shell startup + exit overhead"
echo ""

hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  -n 'bash' 'bash -c true' \
  -n 'gsh'  "$GSH -c true"

# ============================================================
# Benchmark 2: CSV Column Average
#   bash: awk processes file (single external process)
#   gsh:  native Scheme reads file directly (zero external processes)
# ============================================================
echo ""
echo "### 2. CSV Column Average (1M rows) ###"
echo "  bash: awk -F, '{s+=\$3}...' (one awk process)"
echo "  gsh:  ,(read-line loop + string->number) (zero external processes)"
echo ""

BASH_CSV_AVG=$(write_script bash-csv-avg.sh <<'BASH'
#!/usr/bin/env bash
awk -F, 'NR>1 {s+=$3; n++} END {printf "%.6f\n", s/n}' bench/data/sales.csv
BASH
)

# gsh scheme: define field extractor, skip header, accumulate
# NOTE: no Scheme quote (') allowed inside single-quoted shell string
GSH_CSV_AVG=$(write_script gsh-csv-avg.sh <<GEOF
#!/usr/bin/env bash
exec $GSH -c ',(let () (define (fld s n) (let lp ((i 0)(c 0)(st 0)) (cond ((= i (string-length s)) (if (= c n) (substring s st i) "0")) ((char=? (string-ref s i) (integer->char 44)) (if (= c n) (substring s st i) (lp (+ i 1)(+ c 1)(+ i 1)))) (else (lp (+ i 1) c st))))) (let ((p (open-input-file "bench/data/sales.csv"))) (read-line p) (let lp ((s 0.0)(n 0)(l (read-line p))) (if (eof-object? l) (begin (close-input-port p)(display (exact->inexact (/ s n)))(newline)) (lp (+ s (string->number (fld l 2)))(+ n 1)(read-line p))))))'
GEOF
)

hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  -n 'bash+awk' "$BASH_CSV_AVG" \
  -n 'gsh+scheme' "$GSH_CSV_AVG"

# ============================================================
# Benchmark 3: CSV Group-By Aggregation
#   bash: awk to group + sort + head (3 processes piped)
#   gsh:  native hash table + inline sort (zero external processes)
# ============================================================
echo ""
echo "### 3. CSV Group-By Sum — Top Categories (1M rows) ###"
echo "  bash: awk '{a[\$2]+=\$3}...' | sort | head (3-process pipeline)"
echo "  gsh:  ,(hash-table accumulation + sort) (zero external processes)"
echo ""

BASH_CSV_GRP=$(write_script bash-csv-grp.sh <<'BASH'
#!/usr/bin/env bash
awk -F, 'NR>1 {a[$2]+=$3} END {for(k in a) printf "%s %.2f\n",k,a[k]}' bench/data/sales.csv | sort -t' ' -k2 -rn | head -10
BASH
)

# gsh: hash table accumulate, convert to list, insertion-sort (20 items), print top 10
GSH_CSV_GRP=$(write_script gsh-csv-grp.sh <<GEOF
#!/usr/bin/env bash
exec $GSH -c ',(let () (define (fld s n) (let lp ((i 0)(c 0)(st 0)) (cond ((= i (string-length s)) (if (= c n) (substring s st i) "")) ((char=? (string-ref s i) (integer->char 44)) (if (= c n) (substring s st i) (lp (+ i 1)(+ c 1)(+ i 1)))) (else (lp (+ i 1) c st))))) (define (isort lst) (if (null? lst) lst (let ins ((x (car lst))(rest (isort (cdr lst)))) (if (null? rest) (list x) (if (> (cdr x)(cdr (car rest))) (cons x rest) (cons (car rest)(ins x (cdr rest)))))))) (let ((ht (make-table))(p (open-input-file "bench/data/sales.csv"))) (read-line p) (let lp ((l (read-line p))) (if (eof-object? l) (begin (close-input-port p) (let show ((items (isort (table->list ht)))(n 0)) (when (and (pair? items)(< n 10)) (display (caar items))(display " ")(display (exact->inexact (cdar items)))(newline)(show (cdr items)(+ n 1))))) (let ((cat (fld l 1))(amt (string->number (fld l 2)))) (table-set! ht cat (+ (table-ref ht cat 0.0) amt)) (lp (read-line p)))))))'
GEOF
)

hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  -n 'bash+awk+sort+head' "$BASH_CSV_GRP" \
  -n 'gsh+scheme' "$GSH_CSV_GRP"

# ============================================================
# Benchmark 4: Word Frequency — Top 20
#   bash: tr | sort | uniq -c | sort | head (5-process pipeline on ~100MB)
#   gsh:  single-pass hash table (zero external processes)
# ============================================================
echo ""
echo "### 4. Word Frequency — Top 20 ($(du -h "$DATA/words.txt" | cut -f1) text) ###"
echo "  bash: tr | sort | uniq -c | sort | head (5-process pipeline)"
echo "  gsh:  ,(single-pass hash-table counting) (zero external processes)"
echo ""

BASH_WORDFREQ=$(write_script bash-wordfreq.sh <<'BASH'
#!/usr/bin/env bash
tr -cs 'A-Za-z' '\n' < bench/data/words.txt | tr 'A-Z' 'a-z' | sort | uniq -c | sort -rn | head -20
BASH
)

# gsh: read char by char, accumulate words in hash table, sort by count
GSH_WORDFREQ=$(write_script gsh-wordfreq.sh <<GEOF
#!/usr/bin/env bash
exec $GSH -c ',(let () (define (alpha? c) (or (and (char>=? c (integer->char 65))(char<=? c (integer->char 90))) (and (char>=? c (integer->char 97))(char<=? c (integer->char 122))))) (define (downcase s) (let ((n (string-length s))) (let ((r (make-string n))) (let lp ((i 0)) (if (= i n) r (begin (string-set! r i (char-downcase (string-ref s i)))(lp (+ i 1)))))))) (define (isort lst) (if (null? lst) lst (let ins ((x (car lst))(rest (isort (cdr lst)))) (if (null? rest) (list x) (if (> (cdr x)(cdr (car rest))) (cons x rest) (cons (car rest)(ins x (cdr rest)))))))) (let ((ht (make-table))(p (open-input-file "bench/data/words.txt"))) (let lp ((c (read-char p))(buf (open-output-string))) (cond ((eof-object? c) (close-input-port p) (let ((w (downcase (get-output-string buf)))) (when (> (string-length w) 0) (table-set! ht w (+ (table-ref ht w 0) 1)))) (let show ((items (isort (table->list ht)))(n 0)) (when (and (pair? items)(< n 20)) (display (cdar items))(display " ")(display (caar items))(newline)(show (cdr items)(+ n 1))))) ((alpha? c) (write-char c buf)(lp (read-char p) buf)) (else (let ((w (downcase (get-output-string buf)))) (when (> (string-length w) 0) (table-set! ht w (+ (table-ref ht w 0) 1)))) (lp (read-char p)(open-output-string)))))))'
GEOF
)

hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  -n 'bash+tr+sort+uniq' "$BASH_WORDFREQ" \
  -n 'gsh+scheme' "$GSH_WORDFREQ"

# ============================================================
# Benchmark 5: Log Analysis — Single Pass vs Multi-Pipeline
#   bash: 3 separate awk|sort|uniq pipelines (9+ processes)
#   gsh:  ONE pass computing all 3 stats (zero external processes)
#
#   This is the killer benchmark: bash must read the file 3 times
#   with 3 separate pipelines. gsh reads it once.
# ============================================================
echo ""
echo "### 5. Log Analysis — 3 Stats in One Pass (1M lines) ###"
echo "  bash: 3x (awk|sort|uniq|head) = 9+ processes, 3 file reads"
echo "  gsh:  ,(single pass: unique IPs + top URLs + status codes) = 1 file read"
echo ""

BASH_LOG=$(write_script bash-log.sh <<'BASH'
#!/usr/bin/env bash
FILE=bench/data/access.log
echo "=== Unique IPs ==="
awk '{print $1}' "$FILE" | sort -u | wc -l
echo "=== Top 10 URLs ==="
awk '{print $7}' "$FILE" | sort | uniq -c | sort -rn | head -10
echo "=== Status Codes ==="
awk '{print $9}' "$FILE" | sort | uniq -c | sort -rn
BASH
)

# gsh: single pass — 3 hash tables built simultaneously
GSH_LOG=$(write_script gsh-log.sh <<GEOF
#!/usr/bin/env bash
exec $GSH -c ',(let () (define (word-at s n) (let lp ((i 0)(c 0)(st 0)) (cond ((= i (string-length s)) (if (= c n) (substring s st i) "")) ((char=? (string-ref s i) (integer->char 32)) (if (= c n) (substring s st i) (lp (+ i 1)(+ c 1)(+ i 1)))) (else (lp (+ i 1) c st))))) (define (isort lst) (if (null? lst) lst (let ins ((x (car lst))(rest (isort (cdr lst)))) (if (null? rest) (list x) (if (> (cdr x)(cdr (car rest))) (cons x rest) (cons (car rest)(ins x (cdr rest)))))))) (let ((ips (make-table))(urls (make-table))(codes (make-table))(p (open-input-file "bench/data/access.log"))) (let lp ((l (read-line p))) (if (eof-object? l) (begin (close-input-port p) (display "=== Unique IPs ===")(newline)(display (length (table->list ips)))(newline) (display "=== Top 10 URLs ===")(newline) (let show ((items (isort (table->list urls)))(n 0)) (when (and (pair? items)(< n 10)) (display (cdar items))(display " ")(display (caar items))(newline)(show (cdr items)(+ n 1)))) (display "=== Status Codes ===")(newline) (for-each (lambda (p) (display (cdr p))(display " ")(display (car p))(newline)) (isort (table->list codes)))) (let ((ip (word-at l 0))(url (word-at l 6))(code (word-at l 8))) (table-set! ips ip 1) (table-set! urls url (+ (table-ref urls url 0) 1)) (table-set! codes code (+ (table-ref codes code 0) 1)) (lp (read-line p)))))))'
GEOF
)

hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  -n 'bash+awk+sort+uniq (3 passes)' "$BASH_LOG" \
  -n 'gsh+scheme (1 pass)' "$GSH_LOG"

# ============================================================
# Benchmark 6: Pipeline Throughput (same command in both shells)
#   Measures pure pipe setup + fork/exec overhead.
#   Both shells run identical external commands.
# ============================================================
echo ""
echo "### 6. Pipeline Throughput (identical pipeline, different shell) ###"
echo "  Both: grep | sort | uniq -c | sort -rn | head"
echo ""

BASH_PIPE=$(write_script bash-pipe.sh <<'BASH'
#!/usr/bin/env bash
grep 'GET' bench/data/access.log | awk '{print $7}' | sort | uniq -c | sort -rn | head -10
BASH
)

GSH_PIPE=$(write_script gsh-pipe.sh <<GEOF
#!/usr/bin/env bash
exec $GSH -c 'grep GET bench/data/access.log | awk "{print \$7}" | sort | uniq -c | sort -rn | head -10'
GEOF
)

hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  -n 'bash' "$BASH_PIPE" \
  -n 'gsh'  "$GSH_PIPE"

# ============================================================
# Benchmark 7: Sequential Process Launch
#   How fast can each shell fork+exec 500 external commands?
# ============================================================
echo ""
echo "### 7. Sequential Process Launch (500x /bin/true) ###"
echo ""

hyperfine --warmup "$WARMUP" --runs "$RUNS" \
  -n 'bash' 'bash -c "for i in \$(seq 1 500); do /bin/true; done"' \
  -n 'gsh'  "$GSH -c 'for i in \$(seq 1 500); do /bin/true; done'"

echo ""
echo "========================================"
echo " Done. All benchmarks complete."
echo "========================================"
