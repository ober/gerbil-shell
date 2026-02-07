;;; arithmetic.ss — Shell arithmetic evaluation for gsh
;;; Implements $(( )) arithmetic with full operator precedence

(export #t)
(import :std/sugar
        :std/format)

;;; --- Public interface ---

;; Evaluate a shell arithmetic expression string
;; env-get-fn: (lambda (name) value-string-or-#f)
;; env-set-fn: (lambda (name value) void)
;; Returns: integer result
(def (arith-eval expr env-get-fn env-set-fn)
  (let* ((tokens (arith-tokenize expr))
         (state (new-arith-state tokens env-get-fn env-set-fn)))
    (if (null? tokens)
      0
      (let ((result (parse-comma-expr state)))
        result))))

;;; --- Tokenizer ---

(defstruct arith-state (tokens pos env-get env-set) transparent: #t)

(def (new-arith-state tokens env-get env-set)
  (make-arith-state tokens 0 env-get env-set))

;; Arith token types: 'number 'name 'op
(defstruct arith-token (type value) transparent: #t)

(def (arith-tokenize expr)
  (let loop ((i 0) (tokens []))
    (cond
      ((>= i (string-length expr))
       (reverse tokens))
      ;; Skip whitespace
      ((char-whitespace? (string-ref expr i))
       (loop (+ i 1) tokens))
      ;; Numbers: decimal, hex (0x), octal (0), binary (0b)
      ((char-numeric? (string-ref expr i))
       (let-values (((num end) (read-number expr i)))
         (loop end (cons (make-arith-token 'number num) tokens))))
      ;; Names (variable references)
      ((or (char-alphabetic? (string-ref expr i))
           (char=? (string-ref expr i) #\_))
       (let-values (((name end) (read-name expr i)))
         (loop end (cons (make-arith-token 'name name) tokens))))
      ;; Multi-char operators
      (else
       (let-values (((op end) (read-operator expr i)))
         (loop end (cons (make-arith-token 'op op) tokens)))))))

(def (read-number expr i)
  (let ((len (string-length expr)))
    (cond
      ;; Hex: 0x or 0X
      ((and (< (+ i 1) len)
            (char=? (string-ref expr i) #\0)
            (or (char=? (string-ref expr (+ i 1)) #\x)
                (char=? (string-ref expr (+ i 1)) #\X)))
       (let loop ((j (+ i 2)))
         (if (and (< j len) (hex-digit? (string-ref expr j)))
           (loop (+ j 1))
           (values (string->number (substring expr i j) 16) j))))
      ;; Binary: 0b or 0B
      ((and (< (+ i 1) len)
            (char=? (string-ref expr i) #\0)
            (or (char=? (string-ref expr (+ i 1)) #\b)
                (char=? (string-ref expr (+ i 1)) #\B)))
       (let loop ((j (+ i 2)))
         (if (and (< j len) (or (char=? (string-ref expr j) #\0)
                                 (char=? (string-ref expr j) #\1)))
           (loop (+ j 1))
           (values (string->number (substring expr (+ i 2) j) 2) j))))
      ;; Octal: starts with 0
      ((and (char=? (string-ref expr i) #\0)
            (< (+ i 1) len)
            (char-numeric? (string-ref expr (+ i 1))))
       (let loop ((j (+ i 1)))
         (if (and (< j len) (octal-digit? (string-ref expr j)))
           (loop (+ j 1))
           (values (string->number (substring expr i j) 8) j))))
      ;; Decimal
      (else
       (let loop ((j i))
         (if (and (< j len) (char-numeric? (string-ref expr j)))
           (loop (+ j 1))
           (values (string->number (substring expr i j)) j)))))))

(def (read-name expr i)
  (let ((len (string-length expr)))
    (let loop ((j i))
      (if (and (< j len)
               (let ((ch (string-ref expr j)))
                 (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))))
        (loop (+ j 1))
        (values (substring expr i j) j)))))

(def (read-operator expr i)
  (let ((len (string-length expr))
        (ch (string-ref expr i)))
    (cond
      ;; Three-char operators
      ((and (< (+ i 2) len)
            (string=? (substring expr i (+ i 3)) "<<="))
       (values "<<=" (+ i 3)))
      ((and (< (+ i 2) len)
            (string=? (substring expr i (+ i 3)) ">>="))
       (values ">>=" (+ i 3)))
      ;; Two-char operators
      ((< (+ i 1) len)
       (let ((two (substring expr i (+ i 2))))
         (cond
           ((member two '("==" "!=" "<=" ">=" "&&" "||" "<<" ">>"
                          "+=" "-=" "*=" "/=" "%=" "&=" "^=" "|="
                          "++" "--" "**"))
            (values two (+ i 2)))
           (else
            (values (string ch) (+ i 1))))))
      (else
       (values (string ch) (+ i 1))))))

(def (hex-digit? ch)
  (or (char-numeric? ch)
      (and (char>=? ch #\a) (char<=? ch #\f))
      (and (char>=? ch #\A) (char<=? ch #\F))))

(def (octal-digit? ch)
  (and (char>=? ch #\0) (char<=? ch #\7)))

;;; --- Recursive descent parser (operator precedence) ---

(def (arith-peek state)
  (if (>= (arith-state-pos state) (length (arith-state-tokens state)))
    #f
    (list-ref (arith-state-tokens state) (arith-state-pos state))))

(def (arith-advance! state)
  (set! (arith-state-pos state) (+ 1 (arith-state-pos state))))

(def (arith-expect-op! state expected)
  (let ((tok (arith-peek state)))
    (if (and tok (eq? (arith-token-type tok) 'op)
             (string=? (arith-token-value tok) expected))
      (begin (arith-advance! state) #t)
      (error (format "arithmetic: expected ~a" expected)))))

(def (arith-match-op? state op)
  (let ((tok (arith-peek state)))
    (and tok (eq? (arith-token-type tok) 'op)
         (string=? (arith-token-value tok) op))))

(def (arith-consume-op! state op)
  (if (arith-match-op? state op)
    (begin (arith-advance! state) #t)
    #f))

;; Get variable value as integer
(def (arith-get-var state name)
  (let ((val ((arith-state-env-get state) name)))
    (if val
      (or (string->number val) 0)
      0)))

;; Set variable value
(def (arith-set-var! state name value)
  ((arith-state-env-set state) name (number->string value))
  value)

;;; --- Precedence levels (lowest to highest) ---

;; Level 1: Comma (sequence)
(def (parse-comma-expr state)
  (let loop ((result (parse-assignment-expr state)))
    (if (arith-consume-op! state ",")
      (loop (parse-assignment-expr state))
      result)))

;; Level 2: Assignment
(def (parse-assignment-expr state)
  (let ((tok (arith-peek state)))
    (if (and tok (eq? (arith-token-type tok) 'name))
      ;; Peek ahead for assignment operator
      (let* ((name (arith-token-value tok))
             (saved-pos (arith-state-pos state)))
        (arith-advance! state)
        (let ((op-tok (arith-peek state)))
          (if (and op-tok (eq? (arith-token-type op-tok) 'op)
                   (member (arith-token-value op-tok)
                           '("=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&=" "^=" "|=")))
            (let ((op (arith-token-value op-tok)))
              (arith-advance! state)
              (let ((rhs (parse-assignment-expr state)))
                (cond
                  ((string=? op "=") (arith-set-var! state name rhs))
                  ((string=? op "+=") (arith-set-var! state name (+ (arith-get-var state name) rhs)))
                  ((string=? op "-=") (arith-set-var! state name (- (arith-get-var state name) rhs)))
                  ((string=? op "*=") (arith-set-var! state name (* (arith-get-var state name) rhs)))
                  ((string=? op "/=") (arith-set-var! state name (quotient (arith-get-var state name) rhs)))
                  ((string=? op "%=") (arith-set-var! state name (modulo (arith-get-var state name) rhs)))
                  ((string=? op "<<=") (arith-set-var! state name (arithmetic-shift (arith-get-var state name) rhs)))
                  ((string=? op ">>=") (arith-set-var! state name (arithmetic-shift (arith-get-var state name) (- rhs))))
                  ((string=? op "&=") (arith-set-var! state name (bitwise-and (arith-get-var state name) rhs)))
                  ((string=? op "^=") (arith-set-var! state name (bitwise-xor (arith-get-var state name) rhs)))
                  ((string=? op "|=") (arith-set-var! state name (bitwise-ior (arith-get-var state name) rhs)))
                  (else (error "unknown assignment op" op)))))
            ;; Not an assignment, backtrack
            (begin
              (set! (arith-state-pos state) saved-pos)
              (parse-ternary-expr state)))))
      (parse-ternary-expr state))))

;; Level 3: Ternary ?:
(def (parse-ternary-expr state)
  (let ((cond-val (parse-logical-or state)))
    (if (arith-consume-op! state "?")
      (let ((then-val (parse-comma-expr state)))
        (arith-expect-op! state ":")
        (let ((else-val (parse-ternary-expr state)))
          (if (not (= cond-val 0)) then-val else-val)))
      cond-val)))

;; Level 4: Logical OR ||
(def (parse-logical-or state)
  (let loop ((result (parse-logical-and state)))
    (if (arith-consume-op! state "||")
      (let ((rhs (parse-logical-and state)))
        (loop (if (or (not (= result 0)) (not (= rhs 0))) 1 0)))
      result)))

;; Level 5: Logical AND &&
(def (parse-logical-and state)
  (let loop ((result (parse-bitwise-or state)))
    (if (arith-consume-op! state "&&")
      (let ((rhs (parse-bitwise-or state)))
        (loop (if (and (not (= result 0)) (not (= rhs 0))) 1 0)))
      result)))

;; Level 6: Bitwise OR |
(def (parse-bitwise-or state)
  (let loop ((result (parse-bitwise-xor state)))
    (if (arith-consume-op! state "|")
      (loop (bitwise-ior result (parse-bitwise-xor state)))
      result)))

;; Level 7: Bitwise XOR ^
(def (parse-bitwise-xor state)
  (let loop ((result (parse-bitwise-and state)))
    (if (arith-consume-op! state "^")
      (loop (bitwise-xor result (parse-bitwise-and state)))
      result)))

;; Level 8: Bitwise AND &
(def (parse-bitwise-and state)
  (let loop ((result (parse-equality state)))
    (if (arith-consume-op! state "&")
      (loop (bitwise-and result (parse-equality state)))
      result)))

;; Level 9: Equality == !=
(def (parse-equality state)
  (let loop ((result (parse-comparison state)))
    (cond
      ((arith-consume-op! state "==")
       (loop (if (= result (parse-comparison state)) 1 0)))
      ((arith-consume-op! state "!=")
       (loop (if (not (= result (parse-comparison state))) 1 0)))
      (else result))))

;; Level 10: Comparison < <= > >=
(def (parse-comparison state)
  (let loop ((result (parse-shift state)))
    (cond
      ((arith-consume-op! state "<=")
       (loop (if (<= result (parse-shift state)) 1 0)))
      ((arith-consume-op! state ">=")
       (loop (if (>= result (parse-shift state)) 1 0)))
      ((arith-consume-op! state "<")
       (loop (if (< result (parse-shift state)) 1 0)))
      ((arith-consume-op! state ">")
       (loop (if (> result (parse-shift state)) 1 0)))
      (else result))))

;; Level 11: Bit shift << >>
(def (parse-shift state)
  (let loop ((result (parse-additive state)))
    (cond
      ((arith-consume-op! state "<<")
       (loop (arithmetic-shift result (parse-additive state))))
      ((arith-consume-op! state ">>")
       (loop (arithmetic-shift result (- (parse-additive state)))))
      (else result))))

;; Level 12: Addition + -
(def (parse-additive state)
  (let loop ((result (parse-multiplicative state)))
    (cond
      ((arith-consume-op! state "+")
       (loop (+ result (parse-multiplicative state))))
      ((arith-consume-op! state "-")
       (loop (- result (parse-multiplicative state))))
      (else result))))

;; Level 13: Multiplication * / %
(def (parse-multiplicative state)
  (let loop ((result (parse-exponent state)))
    (cond
      ((arith-consume-op! state "*")
       (loop (* result (parse-exponent state))))
      ((arith-consume-op! state "/")
       (let ((divisor (parse-exponent state)))
         (when (= divisor 0) (error "arithmetic: division by zero"))
         (loop (quotient result divisor))))
      ((arith-consume-op! state "%")
       (let ((divisor (parse-exponent state)))
         (when (= divisor 0) (error "arithmetic: division by zero"))
         (loop (modulo result divisor))))
      (else result))))

;; Level 14: Exponentiation ** (right-associative)
(def (parse-exponent state)
  (let ((base (parse-unary state)))
    (if (arith-consume-op! state "**")
      (let ((exp (parse-exponent state)))  ;; right-associative
        (expt base exp))
      base)))

;; Level 15: Unary ! ~ + - (prefix)
(def (parse-unary state)
  (cond
    ((arith-consume-op! state "!")
     (if (= (parse-unary state) 0) 1 0))
    ((arith-consume-op! state "~")
     (bitwise-not (parse-unary state)))
    ((arith-consume-op! state "-")
     (- (parse-unary state)))
    ((arith-consume-op! state "+")
     (parse-unary state))
    ;; Pre-increment/decrement
    ((arith-consume-op! state "++")
     (let ((tok (arith-peek state)))
       (if (and tok (eq? (arith-token-type tok) 'name))
         (let ((name (arith-token-value tok)))
           (arith-advance! state)
           (arith-set-var! state name (+ (arith-get-var state name) 1)))
         (error "arithmetic: ++ requires variable"))))
    ((arith-consume-op! state "--")
     (let ((tok (arith-peek state)))
       (if (and tok (eq? (arith-token-type tok) 'name))
         (let ((name (arith-token-value tok)))
           (arith-advance! state)
           (arith-set-var! state name (- (arith-get-var state name) 1)))
         (error "arithmetic: -- requires variable"))))
    (else (parse-postfix state))))

;; Level 16: Postfix ++ --
(def (parse-postfix state)
  (let ((val (parse-primary state)))
    ;; Check for postfix ++ or -- (only valid after a name)
    val))

;; Primary: number, variable, or (expr)
(def (parse-primary state)
  (let ((tok (arith-peek state)))
    (cond
      ((not tok) (error "arithmetic: unexpected end of expression"))
      ((eq? (arith-token-type tok) 'number)
       (arith-advance! state)
       (arith-token-value tok))
      ((eq? (arith-token-type tok) 'name)
       (arith-advance! state)
       (let ((name (arith-token-value tok)))
         ;; Check for postfix ++ --
         (cond
           ((arith-consume-op! state "++")
            (let ((val (arith-get-var state name)))
              (arith-set-var! state name (+ val 1))
              val))  ;; return old value
           ((arith-consume-op! state "--")
            (let ((val (arith-get-var state name)))
              (arith-set-var! state name (- val 1))
              val))  ;; return old value
           (else (arith-get-var state name)))))
      ((and (eq? (arith-token-type tok) 'op)
            (string=? (arith-token-value tok) "("))
       (arith-advance! state)
       (let ((result (parse-comma-expr state)))
         (arith-expect-op! state ")")
         result))
      (else
       (error (format "arithmetic: unexpected token ~a" (arith-token-value tok)))))))
