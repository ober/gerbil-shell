#!/usr/bin/env gsh
# Demo: Mixing Shell and Scheme in gsh
# Lines starting with comma (,) are evaluated as Gerbil Scheme

echo "=== Basic Arithmetic ==="
,(+ 1 2 3 4 5)
,(*  6 7)
,(expt 2 10)

echo ""
echo "=== String Operations ==="
,(string-append "Hello" " " "from" " " "Gerbil!")
,(string-upcase "gerbil scheme")
,(string-length "How long is this?")

echo ""
echo "=== List Processing ==="
,(map (lambda (x) (* x x)) '(1 2 3 4 5))
,(filter even? '(1 2 3 4 5 6 7 8 9 10))
,(foldl + 0 '(1 2 3 4 5))

echo ""
echo "=== Environment Access ==="
echo "Shell HOME: $HOME"
,(getenv "HOME")
,(getenv "USER")

echo ""
echo "=== File System ==="
pwd
,(current-directory)
,(file-exists? "/tmp")
,(if (file-exists? "/etc/passwd") "Password file exists" "No password file")

echo ""
echo "=== Definitions and State ==="
,(define greeting "Hello, World!")
,greeting
,(define (factorial n) (if (<= n 1) 1 (* n (factorial (- n 1)))))
,(factorial 5)
,(factorial 10)

echo ""
echo "=== Mixing Shell and Scheme ==="
export SHELL_VAR="From Shell"
echo "Shell: $SHELL_VAR"
,(getenv "SHELL_VAR")
,(begin (setenv "SCHEME_VAR" "From Scheme") (void))
echo "Shell sees Scheme var: $SCHEME_VAR"

echo ""
echo "=== Error Handling ==="
ls /nonexistent 2>&1 | head -1
,(/ 1 0)
echo "Shell continues after Scheme error"

echo ""
echo "Demo complete!"
