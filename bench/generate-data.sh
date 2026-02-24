#!/usr/bin/env bash
# Generate test data for benchmarks
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
DATA="$DIR/data"
ROWS=${1:-1000000}

mkdir -p "$DATA"

echo "==> Generating sales.csv ($ROWS rows)..."
awk -v rows="$ROWS" 'BEGIN {
  srand(42)
  cats[1]="Electronics"; cats[2]="Clothing"; cats[3]="Food"; cats[4]="Books"
  cats[5]="Toys"; cats[6]="Sports"; cats[7]="Music"; cats[8]="Movies"
  cats[9]="Games"; cats[10]="Health"; cats[11]="Beauty"; cats[12]="Home"
  cats[13]="Garden"; cats[14]="Tools"; cats[15]="Auto"; cats[16]="Office"
  cats[17]="Pet"; cats[18]="Travel"; cats[19]="Fitness"; cats[20]="Baby"
  print "id,category,amount,quantity,date"
  for (i=1; i<=rows; i++) {
    printf "%d,%s,%.2f,%d,2024-%02d-%02d\n", \
      i, cats[int(rand()*20)+1], rand()*10000, int(rand()*100)+1, \
      int(rand()*12)+1, int(rand()*28)+1
  }
}' > "$DATA/sales.csv"

echo "==> Generating access.log ($ROWS lines)..."
awk -v rows="$ROWS" 'BEGIN {
  srand(42)
  for (i=1; i<=50; i++)
    ips[i]=sprintf("%d.%d.%d.%d", int(rand()*223)+1, int(rand()*256), int(rand()*256), int(rand()*256))
  urls[1]="/"; urls[2]="/index.html"; urls[3]="/api/users"; urls[4]="/api/products"
  urls[5]="/api/orders"; urls[6]="/static/app.js"; urls[7]="/static/style.css"
  urls[8]="/login"; urls[9]="/logout"; urls[10]="/dashboard"
  urls[11]="/api/search"; urls[12]="/api/cart"; urls[13]="/checkout"
  urls[14]="/about"; urls[15]="/contact"; nurl=15
  for (i=1; i<=70; i++) st[i]=200
  for (i=71; i<=80; i++) st[i]=301
  for (i=81; i<=90; i++) st[i]=304
  for (i=91; i<=95; i++) st[i]=404
  for (i=96; i<=98; i++) st[i]=500
  for (i=99; i<=100; i++) st[i]=503; nst=100
  methods[1]="GET"; methods[2]="GET"; methods[3]="GET"
  methods[4]="POST"; methods[5]="PUT"; nm=5
  for (i=1; i<=rows; i++) {
    printf "%s - - [23/Feb/2026:10:%02d:%02d +0000] \"%s %s HTTP/1.1\" %d %d\n", \
      ips[int(rand()*50)+1], int(rand()*60), int(rand()*60), \
      methods[int(rand()*nm)+1], urls[int(rand()*nurl)+1], \
      st[int(rand()*nst)+1], int(rand()*50000)+100
  }
}' > "$DATA/access.log"

echo "==> Generating words.txt..."
if [ -f /usr/share/dict/words ]; then
  # Repeat dictionary ~100 times for ~100MB
  for _ in $(seq 1 100); do cat /usr/share/dict/words; done > "$DATA/words.txt"
else
  awk 'BEGIN {
    srand(42)
    w[1]="the"; w[2]="quick"; w[3]="brown"; w[4]="fox"; w[5]="jumps"
    w[6]="over"; w[7]="lazy"; w[8]="dog"; w[9]="and"; w[10]="runs"
    w[11]="through"; w[12]="forest"; w[13]="while"; w[14]="birds"; w[15]="sing"
    w[16]="songs"; w[17]="above"; w[18]="green"; w[19]="trees"; w[20]="below"
    for (i=1; i<=10000000; i++) {
      printf "%s", w[int(rand()*20)+1]
      if (i%10==0) printf "\n"; else printf " "
    }
  }' > "$DATA/words.txt"
fi

echo "==> Generated:"
ls -lh "$DATA/"
