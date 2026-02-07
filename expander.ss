;;; expander.ss — Word expansion for gsh
;;; Order: brace -> tilde -> parameter -> command-sub -> arithmetic
;;;        -> process-sub -> word-split -> pathname -> quote-removal

(export #t)
(import :std/sugar
        :std/format
        :std/pregexp
        :gsh/ast
        :gsh/util
        :gsh/environment
        :gsh/ffi
        :gsh/glob
        :gsh/arithmetic)

;;; --- Public interface ---

;; Expand a word (string with shell syntax) to a list of strings
;; Performs full expansion pipeline including word splitting and globbing
(def (expand-word word env)
  (if (string? word)
    (let* ((expanded (expand-string word env))
           ;; Word splitting — inhibited if original word was quoted
           (quoted? (word-has-quotes? word))
           (split (if quoted? [expanded] (word-split expanded env)))
           ;; Pathname expansion (globbing) — inhibited if quoted
           (globbed (if quoted?
                     split
                     (append-map (lambda (w)
                                   (if (and (glob-pattern? w)
                                            (not (env-option? env "noglob")))
                                     (glob-expand w)
                                     [w]))
                                 split))))
      (if (null? globbed) [""] globbed))
    [word]))

;; Expand a word without word splitting or globbing
;; Used for assignments, here-docs, etc.
(def (expand-word-nosplit word env)
  (if (string? word)
    (expand-string word env)
    word))

;; Expand a list of words, returning a flat list of strings
(def (expand-words words env)
  (append-map (lambda (w) (expand-word w env)) words))

;;; --- String expansion ---

;; Main expansion: processes $, `, ~, and quoting in a string
(def (expand-string str env)
  (let ((len (string-length str))
        (out (open-output-string)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref str i)))
          (cond
            ;; Tilde at start
            ((and (= i 0) (char=? ch #\~))
             (let-values (((expanded end) (expand-tilde-in str i env)))
               (display expanded out)
               (loop end)))
            ;; Dollar expansion
            ((char=? ch #\$)
             (let-values (((expanded end) (expand-dollar str i env)))
               (display expanded out)
               (loop end)))
            ;; Backtick command substitution
            ((char=? ch #\`)
             (let-values (((expanded end) (expand-backtick str i env)))
               (display expanded out)
               (loop end)))
            ;; Backslash escape
            ((char=? ch #\\)
             (if (< (+ i 1) len)
               (begin
                 (display (string-ref str (+ i 1)) out)
                 (loop (+ i 2)))
               (begin
                 (display "\\" out)
                 (loop (+ i 1)))))
            ;; Single quote — literal
            ((char=? ch #\')
             (let-values (((content end) (read-single-quote str (+ i 1))))
               (display content out)
               (loop end)))
            ;; Double quote — partial expansion
            ((char=? ch #\")
             (let-values (((content end) (expand-double-quote str (+ i 1) env)))
               (display content out)
               (loop end)))
            ;; Regular character
            (else
             (display ch out)
             (loop (+ i 1)))))))))

;;; --- Tilde expansion ---

(def (expand-tilde-in str i env)
  ;; Find end of tilde prefix (up to first / or end)
  (let* ((len (string-length str))
         (end (let loop ((j (+ i 1)))
                (if (or (>= j len) (char=? (string-ref str j) #\/))
                  j
                  (loop (+ j 1)))))
         (prefix (substring str (+ i 1) end)))
    (cond
      ;; ~ alone or ~/...
      ((= (string-length prefix) 0)
       (values (or (env-get env "HOME") "~") end))
      ;; ~+ = $PWD
      ((string=? prefix "+")
       (values (or (env-get env "PWD") "+") end))
      ;; ~- = $OLDPWD
      ((string=? prefix "-")
       (values (or (env-get env "OLDPWD") "-") end))
      ;; ~user
      (else
       (with-catch
        (lambda (e) (values (substring str i end) end))
        (lambda ()
          (values (user-info-home (user-info prefix)) end)))))))

;;; --- Dollar expansion ---

(def (expand-dollar str i env)
  (let ((len (string-length str)))
    (if (>= (+ i 1) len)
      (values "$" (+ i 1))
      (let ((next (string-ref str (+ i 1))))
        (cond
          ;; $(( ... )) — arithmetic
          ((and (char=? next #\()
                (< (+ i 2) len)
                (char=? (string-ref str (+ i 2)) #\())
           (expand-arith-sub str i env))
          ;; $( ... ) — command substitution
          ((char=? next #\()
           (expand-command-sub str i env))
          ;; ${ ... } — parameter expansion
          ((char=? next #\{)
           (expand-parameter-braced str i env))
          ;; $name — simple variable
          ((or (char-alphabetic? next) (char=? next #\_))
           (expand-simple-var str i env))
          ;; $? $$ $! $# $* $@ $- $0-$9
          ((memq next '(#\? #\$ #\! #\# #\* #\@ #\- #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
           (let ((val (env-get env (string next))))
             (values (or val "") (+ i 2))))
          (else
           (values "$" (+ i 1))))))))

;; Simple variable: $NAME
(def (expand-simple-var str i env)
  (let* ((len (string-length str))
         (end (let loop ((j (+ i 1)))
                (if (and (< j len)
                         (let ((ch (string-ref str j)))
                           (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_))))
                  (loop (+ j 1))
                  j)))
         (name (substring str (+ i 1) end))
         (val (env-get env name)))
    (values (or val "") end)))

;; Braced parameter: ${name} ${name:-default} ${#name} etc.
(def (expand-parameter-braced str i env)
  (let* ((len (string-length str))
         (close (find-matching-brace str (+ i 2)))
         (content (if close
                    (substring str (+ i 2) close)
                    (error "bad substitution: unclosed ${"))))
    (let ((result (expand-parameter-content content env)))
      (values result (+ close 1)))))

;; Expand parameter content (inside ${...})
(def (expand-parameter-content content env)
  (let ((len (string-length content)))
    (cond
      ;; ${#name} — length
      ((and (> len 1) (char=? (string-ref content 0) #\#))
       (let* ((name (substring content 1 len))
              (val (or (env-get env name) "")))
         (number->string (string-length val))))
      ;; ${!name} — indirect expansion: dereference variable
      ((and (> len 1) (char=? (string-ref content 0) #\!))
       (let* ((name (substring content 1 len))
              (ref-name (env-get env name)))
         (if ref-name
           (or (env-get env ref-name) "")
           "")))
      (else
       ;; Find modifier
       (let-values (((name modifier arg) (parse-parameter-modifier content)))
         (let ((val (env-get env name)))
           (apply-parameter-modifier val name modifier arg env)))))))

;; Parse NAME and modifier from parameter content
(def (parse-parameter-modifier content)
  (let ((len (string-length content)))
    ;; Find the modifier operator
    (let loop ((i 0))
      (cond
        ((>= i len) (values content #f ""))
        ((and (< (+ i 1) len)
              (memq (string-ref content i) '(#\: #\% #\# #\/ #\^ #\,)))
         (let ((ch (string-ref content i)))
           (case ch
             ((#\:)
              (if (< (+ i 1) len)
                (let ((mod-ch (string-ref content (+ i 1))))
                  (case mod-ch
                    ((#\- #\+ #\= #\?)
                     (values (substring content 0 i)
                             (string->symbol (string #\: mod-ch))
                             (substring content (+ i 2) len)))
                    (else
                     ;; ${name:offset} or ${name:offset:length}
                     (values (substring content 0 i) ':
                             (substring content (+ i 1) len)))))
                (values content #f "")))
             ((#\%)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\%))
                (values (substring content 0 i) '%%
                        (substring content (+ i 2) len))
                (values (substring content 0 i) '%
                        (substring content (+ i 1) len))))
             ((#\#)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\#))
                (values (substring content 0 i) 'prefix-long
                        (substring content (+ i 2) len))
                (values (substring content 0 i) 'prefix-short
                        (substring content (+ i 1) len))))
             ((#\/)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\/))
                ;; ${name//pattern/replacement}
                (let ((sep (string-find-char-from content #\/ (+ i 2))))
                  (if sep
                    (values (substring content 0 i) '//
                            (cons (substring content (+ i 2) sep)
                                  (substring content (+ sep 1) len)))
                    (values (substring content 0 i) '//
                            (cons (substring content (+ i 2) len) ""))))
                ;; ${name/pattern/replacement}
                (let ((sep (string-find-char-from content #\/ (+ i 1))))
                  (if sep
                    (values (substring content 0 i) '/
                            (cons (substring content (+ i 1) sep)
                                  (substring content (+ sep 1) len)))
                    (values (substring content 0 i) '/
                            (cons (substring content (+ i 1) len) ""))))))
             ((#\^)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\^))
                (values (substring content 0 i) '^^
                        (substring content (+ i 2) len))
                (values (substring content 0 i) '^
                        (substring content (+ i 1) len))))
             ((#\,)
              (if (and (< (+ i 1) len) (char=? (string-ref content (+ i 1)) #\,))
                (values (substring content 0 i) 'lc-all
                        (substring content (+ i 2) len))
                (values (substring content 0 i) 'lc-first
                        (substring content (+ i 1) len))))
             (else (loop (+ i 1))))))
        ;; Check for uncolon modifiers: - + = ?
        ((and (memq (string-ref content i) '(#\- #\+ #\= #\?))
              ;; Only if this is the first non-name char
              (let loop2 ((j 0))
                (or (= j i)
                    (and (let ((ch (string-ref content j)))
                           (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))
                         (loop2 (+ j 1))))))
         (values (substring content 0 i)
                 (string->symbol (string (string-ref content i)))
                 (substring content (+ i 1) len)))
        (else (loop (+ i 1)))))))

;; Apply parameter modifier
(def (apply-parameter-modifier val name modifier arg env)
  (case modifier
    ;; ${name:-word} — default if unset or null
    ((:-) (if (or (not val) (string=? val ""))
            (expand-string arg env)
            val))
    ;; ${name-word} — default if unset
    ((-) (if (not val)
           (expand-string arg env)
           val))
    ;; ${name:=word} — assign default if unset or null
    ((:=) (if (or (not val) (string=? val ""))
            (let ((default (expand-string arg env)))
              (env-set! env name default)
              default)
            val))
    ;; ${name=word} — assign default if unset
    ((=) (if (not val)
           (let ((default (expand-string arg env)))
             (env-set! env name default)
             default)
           val))
    ;; ${name:?word} — error if unset or null
    ((:?) (if (or (not val) (string=? val ""))
            (error (format "~a: ~a" name (if (string=? arg "") "parameter null or not set" arg)))
            val))
    ;; ${name?word} — error if unset
    ((?) (if (not val)
           (error (format "~a: ~a" name (if (string=? arg "") "parameter not set" arg)))
           val))
    ;; ${name:+word} — alternate if set and non-null
    ((:+) (if (and val (not (string=? val "")))
            (expand-string arg env)
            ""))
    ;; ${name+word} — alternate if set
    ((+) (if val (expand-string arg env) ""))
    ;; ${name%pattern} — remove shortest suffix
    ((%) (if val (remove-suffix val (expand-string arg env) #f) ""))
    ;; ${name%%pattern} — remove longest suffix
    ((%%) (if val (remove-suffix val (expand-string arg env) #t) ""))
    ;; ${name#pattern} — remove shortest prefix
    ((prefix-short) (if val (remove-prefix val (expand-string arg env) #f) ""))
    ;; ${name##pattern} — remove longest prefix
    ((prefix-long) (if val (remove-prefix val (expand-string arg env) #t) ""))
    ;; ${name^^} — uppercase all
    ((^^) (if val (string-upcase val) ""))
    ;; ${name^} — uppercase first
    ((^) (if (and val (> (string-length val) 0))
           (string-append (string (char-upcase (string-ref val 0)))
                          (substring val 1 (string-length val)))
           (or val "")))
    ;; ${name,,} — lowercase all
    ((lc-all) (if val (string-downcase val) ""))
    ;; ${name,} — lowercase first
    ((lc-first) (if (and val (> (string-length val) 0))
           (string-append (string (char-downcase (string-ref val 0)))
                          (substring val 1 (string-length val)))
           (or val "")))
    ;; ${name:offset} or ${name:offset:length} — substring expansion
    ((:) (if val
           (let* ((str val)
                  (slen (string-length str))
                  ;; arg is "offset" or "offset:length"
                  (colon-pos (string-find-char-from arg #\: 0))
                  (offset-str (if colon-pos (substring arg 0 colon-pos)
                                  (string-trim-whitespace-str arg)))
                  (length-str (if colon-pos
                                (string-trim-whitespace-str
                                 (substring arg (+ colon-pos 1) (string-length arg)))
                                #f))
                  (offset (or (string->number (string-trim-whitespace-str offset-str)) 0))
                  ;; Handle negative offset (from end) — bash requires space before minus
                  (start (if (< offset 0) (max 0 (+ slen offset)) (min offset slen))))
             (if length-str
               (let ((length (or (string->number length-str) 0)))
                 (if (< length 0)
                   ;; Negative length: trim from end
                   (let ((end (max start (+ slen length))))
                     (substring str start end))
                   ;; Positive length
                   (substring str start (min slen (+ start length)))))
               ;; No length — from offset to end
               (substring str start slen)))
           ""))
    ;; No modifier
    ((#f) (or val ""))
    (else (or val ""))))

;;; --- Command substitution ---

(def (expand-command-sub str i env)
  ;; $( ... ) — find matching paren
  (let* ((close (find-matching-paren str (+ i 2)))
         (cmd (substring str (+ i 2) close))
         (output (command-substitute cmd env)))
    (values output (+ close 1))))

(def (expand-backtick str i env)
  ;; ` ... ` — find matching backtick
  (let* ((len (string-length str))
         (end (let loop ((j (+ i 1)))
                (cond
                  ((>= j len) j)
                  ((char=? (string-ref str j) #\\)
                   (loop (+ j 2)))
                  ((char=? (string-ref str j) #\`)
                   j)
                  (else (loop (+ j 1))))))
         (cmd (substring str (+ i 1) end))
         (output (command-substitute cmd env)))
    (values output (+ end 1))))

;; Execute a command and capture its stdout
;; Strips trailing newlines (per POSIX)
(def (command-substitute cmd env)
  (let ((exec-fn (*execute-input*)))
    (if exec-fn
      ;; Use gsh's own executor: redirect stdout to a pipe, run command, read output
      (with-catch
       (lambda (e) "")
       (lambda ()
         (let-values (((read-fd write-fd) (ffi-pipe-raw)))
           ;; Save real stdout fd and Gambit port
           (let ((saved-fd (ffi-dup 1))
                 (saved-port (current-output-port)))
             ;; Redirect real fd 1 to pipe write end
             (ffi-dup2 write-fd 1)
             (ffi-close-fd write-fd)
             ;; Create a Gambit character port via /dev/fd for builtins
             (let ((pipe-port (open-output-file
                               (string-append "/dev/fd/" (number->string 1)))))
               (current-output-port pipe-port)
               ;; Execute the command
               (with-catch
                (lambda (e) #!void)
                (lambda () (exec-fn cmd env)))
               ;; Flush and close the pipe port
               (force-output pipe-port)
               (close-port pipe-port))
             ;; Restore real fd 1 and Gambit port
             (ffi-dup2 saved-fd 1)
             (ffi-close-fd saved-fd)
             (current-output-port saved-port)
             ;; Read the output from the pipe read end
             (let* ((read-port (open-input-file
                                (string-append "/dev/fd/" (number->string read-fd))))
                    (output (read-all-string read-port)))
               (close-port read-port)
               (ffi-close-fd read-fd)
               (string-trim-trailing-newlines output))))))
      ;; Fallback: use /bin/sh if executor not initialized
      (with-catch
       (lambda (e) "")
       (lambda ()
         (let* ((port (open-input-process
                       [path: "/bin/sh"
                        arguments: ["-c" cmd]
                        environment: (env-exported-alist env)]))
                (output (read-all-string port)))
           (close-port port)
           (string-trim-trailing-newlines output)))))))

;;; --- Arithmetic substitution ---

(def (expand-arith-sub str i env)
  ;; $(( ... )) — find matching ))
  (let* ((close (find-arith-close str (+ i 3)))
         (expr (substring str (+ i 3) close))
         (result (arith-eval expr
                            (lambda (name) (env-get env name))
                            (lambda (name val) (env-set! env name val)))))
    (values (number->string result) (+ close 2))))

;;; --- Word splitting ---

(def (word-split str env)
  (let ((ifs (or (env-get env "IFS") " \t\n")))
    (if (string=? ifs "")
      [str]  ;; empty IFS = no splitting
      (let ((len (string-length str)))
        (let loop ((i 0) (start 0) (words []))
          (cond
            ((>= i len)
             (reverse (if (> i start)
                        (cons (substring str start i) words)
                        words)))
            ((ifs-char? (string-ref str i) ifs)
             (let ((new-words (if (> i start)
                                (cons (substring str start i) words)
                                words)))
               ;; Skip IFS whitespace
               (let skip ((j (+ i 1)))
                 (if (and (< j len) (ifs-whitespace? (string-ref str j) ifs))
                   (skip (+ j 1))
                   (loop j j new-words)))))
            (else
             (loop (+ i 1) start words))))))))

(def (ifs-char? ch ifs)
  (let loop ((i 0))
    (and (< i (string-length ifs))
         (or (char=? ch (string-ref ifs i))
             (loop (+ i 1))))))

(def (ifs-whitespace? ch ifs)
  (and (ifs-char? ch ifs)
       (or (char=? ch #\space)
           (char=? ch #\tab)
           (char=? ch #\newline))))

;;; --- Quoting helpers ---

;; Check if a raw word contains quote characters (single or double quotes)
;; Used to determine if word splitting should be inhibited
(def (word-has-quotes? word)
  (let ((len (string-length word)))
    (let loop ((i 0))
      (if (>= i len) #f
          (let ((ch (string-ref word i)))
            (or (char=? ch #\') (char=? ch #\")
                (loop (+ i 1))))))))

(def (read-single-quote str i)
  (let ((len (string-length str)))
    (let loop ((j i) (buf (open-output-string)))
      (cond
        ((>= j len) (values (get-output-string buf) j))
        ((char=? (string-ref str j) #\')
         (values (get-output-string buf) (+ j 1)))
        (else
         (display (string-ref str j) buf)
         (loop (+ j 1) buf))))))

(def (expand-double-quote str i env)
  (let ((len (string-length str))
        (buf (open-output-string)))
    (let loop ((j i))
      (cond
        ((>= j len) (values (get-output-string buf) j))
        ((char=? (string-ref str j) #\")
         (values (get-output-string buf) (+ j 1)))
        ((char=? (string-ref str j) #\\)
         (if (< (+ j 1) len)
           (let ((next (string-ref str (+ j 1))))
             (if (memq next '(#\$ #\` #\" #\\ #\newline))
               (begin (display next buf) (loop (+ j 2)))
               (begin (display "\\" buf) (display next buf) (loop (+ j 2)))))
           (begin (display "\\" buf) (loop (+ j 1)))))
        ((char=? (string-ref str j) #\$)
         (let-values (((expanded end) (expand-dollar str j env)))
           (display expanded buf)
           (loop end)))
        ((char=? (string-ref str j) #\`)
         (let-values (((expanded end) (expand-backtick str j env)))
           (display expanded buf)
           (loop end)))
        (else
         (display (string-ref str j) buf)
         (loop (+ j 1)))))))

;;; --- Bracket/brace matching ---

(def (find-matching-brace str start)
  (let ((len (string-length str)))
    (let loop ((i start) (depth 1))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref str i) #\{) (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref str i) #\})
         (if (= depth 1) i (loop (+ i 1) (- depth 1))))
        ((char=? (string-ref str i) #\\) (loop (+ i 2) depth))
        (else (loop (+ i 1) depth))))))

(def (find-matching-paren str start)
  (let ((len (string-length str)))
    (let loop ((i start) (depth 1))
      (cond
        ((>= i len) (- len 1))
        ((char=? (string-ref str i) #\() (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref str i) #\))
         (if (= depth 1) i (loop (+ i 1) (- depth 1))))
        ((char=? (string-ref str i) #\\) (loop (+ i 2) depth))
        ((char=? (string-ref str i) #\') ;; skip single-quoted
         (let sq ((j (+ i 1)))
           (cond ((>= j len) (loop j depth))
                 ((char=? (string-ref str j) #\') (loop (+ j 1) depth))
                 (else (sq (+ j 1))))))
        (else (loop (+ i 1) depth))))))

(def (find-arith-close str start)
  (let ((len (string-length str)))
    (let loop ((i start) (depth 1))
      (cond
        ((>= i len) (- len 2))
        ((and (char=? (string-ref str i) #\))
              (< (+ i 1) len)
              (char=? (string-ref str (+ i 1)) #\)))
         (if (= depth 1) i (loop (+ i 2) (- depth 1))))
        ((and (char=? (string-ref str i) #\()
              (< (+ i 1) len)
              (char=? (string-ref str (+ i 1)) #\())
         (loop (+ i 2) (+ depth 1)))
        (else (loop (+ i 1) depth))))))

;;; --- Pattern matching helpers ---

(def (remove-suffix val pattern longest?)
  ;; Remove suffix matching pattern
  (if longest?
    ;; Longest: try from start
    (let loop ((i 0))
      (if (> i (string-length val))
        val
        (if (glob-match? pattern (substring val i (string-length val)))
          (substring val 0 i)
          (loop (+ i 1)))))
    ;; Shortest: try from end
    (let loop ((i (string-length val)))
      (if (< i 0)
        val
        (if (glob-match? pattern (substring val i (string-length val)))
          (substring val 0 i)
          (loop (- i 1)))))))

(def (remove-prefix val pattern longest?)
  (if longest?
    (let loop ((i (string-length val)))
      (if (< i 0)
        val
        (if (glob-match? pattern (substring val 0 i))
          (substring val i (string-length val))
          (loop (- i 1)))))
    (let loop ((i 0))
      (if (> i (string-length val))
        val
        (if (glob-match? pattern (substring val 0 i))
          (substring val i (string-length val))
          (loop (+ i 1)))))))

;;; --- String helpers ---

(def (string-trim-trailing-newlines str)
  (let loop ((i (- (string-length str) 1)))
    (if (and (>= i 0) (char=? (string-ref str i) #\newline))
      (loop (- i 1))
      (substring str 0 (+ i 1)))))

(def (read-all-string port)
  (let ((buf (open-output-string)))
    (let loop ()
      (let ((ch (read-char port)))
        (if (eof-object? ch)
          (get-output-string buf)
          (begin (display ch buf) (loop)))))))

(def (append-map f lst)
  (apply append (map f lst)))

(def (string-find-char-from str ch start)
  (let loop ((i start))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

(def (string-trim-whitespace-str str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))
