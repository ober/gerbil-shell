;;; expander.ss — Word expansion for gsh
;;; Order: brace -> tilde -> parameter -> command-sub -> arithmetic
;;;        -> process-sub -> word-split -> pathname -> quote-removal

(export #t)
(import :std/sugar
        :std/format
        :std/pregexp
        :std/srfi/1
        :gsh/ast
        :gsh/util
        :gsh/environment
        :gsh/ffi
        :gsh/functions
        :gsh/glob
        :gsh/arithmetic)

;;; --- Nounset (set -u) error ---
(def (nounset-error! name env)
  (fprintf (current-error-port) "gsh: ~a: unbound variable~n" name)
  (raise (make-nounset-exception 1)))

;;; --- Double-quote context tracking ---
;; When #t, backslash inside expand-string uses double-quote rules:
;; only $, `, ", \, newline are special; others preserve the backslash.
(def *in-dquote-context* (make-parameter #f))

;;; --- Process substitution state ---
;; Accumulates cleanup thunks for process substitutions created during expansion.
;; Caller should bind this and call the cleanups after the command finishes.
(def *procsub-cleanups* (make-parameter []))
(def *procsub-counter* 0)

;; Create a temporary FIFO for process substitution
(def (make-procsub-fifo!)
  (set! *procsub-counter* (+ *procsub-counter* 1))
  (let ((path (string-append "/tmp/gsh-procsub-"
                             (number->string (ffi-getpid)) "-"
                             (number->string *procsub-counter*))))
    (let ((rc (ffi-mkfifo path #o600)))
      (when (< rc 0)
        (error "mkfifo failed" path))
      path)))

;; Run process substitution cleanup: reap processes and remove FIFOs
(def (run-procsub-cleanups!)
  (for-each (lambda (thunk) (thunk)) (*procsub-cleanups*))
  (*procsub-cleanups* []))

;; Expand a process substitution word-process-sub into a FIFO path.
;; Spawns the child process and registers cleanup.
(def (expand-process-sub psub env)
  (let* ((dir (word-process-sub-direction psub))
         (cmd-text (word-process-sub-command psub))
         (path (make-procsub-fifo!))
         ;; For <(cmd): cmd stdout goes to FIFO
         ;; For >(cmd): cmd stdin comes from FIFO
         (redir (if (eq? dir 'in)
                  (string-append "exec >" path "; ")
                  (string-append "exec <" path "; ")))
         ;; Don't redirect stdin/stdout/stderr — child inherits current fds.
         ;; The exec redirect inside the shell command handles the FIFO connection.
         (p (open-process
             [path: "/bin/sh"
              arguments: ["-c" (string-append redir cmd-text)]
              environment: (env-exported-alist env)
              directory: (current-directory)
              stdin-redirection: #f
              stdout-redirection: #f
              stderr-redirection: #f])))
    ;; Register cleanup
    (*procsub-cleanups*
     (cons (lambda ()
             (with-catch void (lambda () (process-status p)))
             (with-catch void (lambda () (ffi-unlink path))))
           (*procsub-cleanups*)))
    path))

;;; --- Public interface ---

;; Expand a word (string with shell syntax) to a list of strings
;; Performs full expansion pipeline including word splitting and globbing
(def (expand-word word env)
  (cond
    ((string? word)
     ;; Step 1: Brace expansion (produces multiple words)
     (let ((brace-words (if (env-option? env "braceexpand")
                          (brace-expand word)
                          [word])))
       ;; Step 2: For each brace-expanded word, do remaining expansions
       (let ((result
              (append-map
               (lambda (w)
                 ;; Special handling for "$@" — expands to multiple words
                 (if (word-has-quoted-at? w)
                   (expand-word-with-at w env)
                   ;; Use segment-aware expansion for proper mixed quote handling
                   (let* ((segments (expand-string-segments w env))
                          (all-quoted? (every (lambda (seg) (eq? (cdr seg) 'quoted)) segments))
                          (any-quoted? (any (lambda (seg) (eq? (cdr seg) 'quoted)) segments))
                          ;; Split segments: unquoted parts undergo IFS splitting,
                          ;; quoted parts are preserved
                          (split (split-expanded-segments segments env))
                          ;; Glob expansion: only on words from unquoted context
                          ;; split returns (text . has-unquoted?) pairs
                          (globbed (let ((raw (with-catch
                                               (lambda (e)
                                                 (if (and (pair? e) (eq? (car e) 'failglob))
                                                   (begin
                                                     (fprintf (current-error-port) "gsh: ~a: no match~n" (cdr e))
                                                     (raise e))
                                                   (raise e)))
                                               (lambda ()
                                                 (append-map
                                                  (lambda (item)
                                                    (let ((s (car item))
                                                          (can-glob? (cdr item)))
                                                      (if (and can-glob?
                                                               (glob-pattern? s (env-shopt? env "extglob"))
                                                               (not (env-option? env "noglob")))
                                                        (glob-expand s
                                                          dotglob?: (env-shopt? env "dotglob")
                                                          nullglob?: (env-shopt? env "nullglob")
                                                          failglob?: (env-shopt? env "failglob")
                                                          nocase?: (env-shopt? env "nocaseglob")
                                                          extglob?: (env-shopt? env "extglob"))
                                                        [s])))
                                                  split)))))
                                    ;; Apply GLOBIGNORE filtering
                                    (let ((gi (env-get env "GLOBIGNORE")))
                                      (if (and gi (not (string=? gi "")))
                                        (glob-ignore-filter raw gi)
                                        raw)))))
                     ;; For quoted words, preserve empty string [""]
                     ;; For unquoted words that expand to empty/nothing, remove them
                     (cond
                       ((null? globbed)
                        (if any-quoted? [""] []))
                       ;; Unquoted word that expanded to single empty string → remove
                       ((and (not any-quoted?)
                             (= (length globbed) 1)
                             (string=? (car globbed) ""))
                        [])
                       (else globbed)))))
               brace-words)))
         result)))
    ;; Process substitution: expand to FIFO path
    ((word-process-sub? word)
     [(expand-process-sub word env)])
    (else [word])))

;;; --- Segment-aware expansion ---
;;; Segments are (text . type) where type is:
;;;   'quoted   — from "..." or '...' — no IFS splitting, no globbing
;;;   'literal  — literal unquoted text, tilde, backslash-escape — no IFS splitting, yes globbing
;;;   'expanded — from unquoted $var, $(cmd), $((arith)) — yes IFS splitting, yes globbing

;; Check if a segment type allows IFS splitting
(def (seg-splittable? type) (eq? type 'expanded))
;; Check if a segment type allows glob expansion
(def (seg-globbable? type) (not (eq? type 'quoted)))
(def (expand-string-segments word env)
  (let ((len (string-length word)))
    (let loop ((i 0) (segments []))
      (if (>= i len)
        (reverse segments)
        (let ((ch (string-ref word i)))
          (cond
            ;; Tilde at start — literal (no split, yes glob)
            ((and (= i 0) (char=? ch #\~))
             (let-values (((expanded end) (expand-tilde-in word i env)))
               (loop end (cons (cons expanded 'literal) segments))))
            ;; Dollar expansion — unquoted, subject to splitting
            ((char=? ch #\$)
             (let-values (((expanded end) (expand-dollar word i env)))
               (loop end (cons (cons expanded 'expanded) segments))))
            ;; Backtick command substitution — unquoted
            ((char=? ch #\`)
             (let-values (((expanded end) (expand-backtick word i env)))
               (loop end (cons (cons expanded 'expanded) segments))))
            ;; Backslash escape — literal (quoted)
            ((char=? ch #\\)
             (if (< (+ i 1) len)
               (loop (+ i 2) (cons (cons (string (string-ref word (+ i 1))) 'quoted) segments))
               (loop (+ i 1) (cons (cons "\\" 'literal) segments))))
            ;; Single quote — literal (quoted)
            ((char=? ch #\')
             (let-values (((content end) (read-single-quote word (+ i 1))))
               (loop end (cons (cons content 'quoted) segments))))
            ;; Double quote — contents are quoted (no splitting)
            ((char=? ch #\")
             (let-values (((content end) (expand-double-quote word (+ i 1) env)))
               (loop end (cons (cons content 'quoted) segments))))
            ;; Regular character — literal (quoted, not split)
            (else
             ;; Accumulate consecutive literal chars
             (let lit-loop ((j (+ i 1)))
               (if (and (< j len)
                        (let ((c (string-ref word j)))
                          (not (or (char=? c #\$) (char=? c #\`) (char=? c #\\)
                                   (char=? c #\') (char=? c #\") (char=? c #\~)))))
                 (lit-loop (+ j 1))
                 (loop j (cons (cons (substring word i j) 'literal) segments)))))))))))

;; Split expanded segments according to IFS rules.
;; Input: list of (text . quoted?) pairs
;; Output: list of (text . has-unquoted?) pairs (after IFS splitting)
;; Unquoted segments are split on IFS; quoted segments are not.
;; Adjacent segments join into the same word (no split boundary between them).
(def (split-expanded-segments segments env)
  (if (null? segments)
    [(cons "" #f)]
    (let ((ifs (or (env-get env "IFS") " \t\n")))
      (if (string=? ifs "")
        ;; Empty IFS: no splitting at all, just concatenate
        (let ((text (apply string-append (map car segments)))
              (can-glob? (any (lambda (seg) (seg-globbable? (cdr seg))) segments)))
          [(cons text can-glob?)])
        ;; Normal IFS splitting
        (let ((words [])      ;; completed words (reversed)
              (current (open-output-string))  ;; current word being built
              (cur-can-glob? #f))  ;; does current word have unquoted content?
          ;; Process each segment
          (let seg-loop ((segs segments)
                         (words [])
                         (current (open-output-string))
                         (cur-can-glob? #f)
                         (word-started? #f))
            (if (null? segs)
              ;; Done: emit final word if anything was accumulated
              (let ((final (get-output-string current)))
                (if (or word-started? (> (string-length final) 0))
                  (reverse (cons (cons final cur-can-glob?) words))
                  (reverse words)))
              (let* ((seg (car segs))
                     (text (car seg))
                     (type (cdr seg)))
                (if (not (seg-splittable? type))
                  ;; Quoted segment: append directly to current word, never split
                  (begin
                    (display text current)
                    (seg-loop (cdr segs) words current (or cur-can-glob? (seg-globbable? type))
                              (or word-started? #t)))
                  ;; Unquoted segment: apply IFS splitting
                  (let* ((split-words (word-split text env))
                         (n (length split-words)))
                    (cond
                      ;; Empty expansion: nothing to add, but mark as having unquoted content
                      ((= n 0)
                       (seg-loop (cdr segs) words current (or cur-can-glob? #t) word-started?))
                      ;; Single word: append to current (joining with adjacent)
                      ((= n 1)
                       (display (car split-words) current)
                       (seg-loop (cdr segs) words current (or cur-can-glob? #t)
                                 (or word-started? (> (string-length (car split-words)) 0))))
                      ;; Multiple words: first joins with current, rest are separate
                      (else
                       (display (car split-words) current)
                       ;; Emit current word
                       (let ((w (get-output-string current)))
                         (let inner-loop ((rest (cdr split-words))
                                          (words (cons (cons w #t) words)))
                           (if (null? (cdr rest))
                             ;; Last split word: start a new current
                             (let ((new-current (open-output-string)))
                               (display (car rest) new-current)
                               (seg-loop (cdr segs) words new-current #t #t))
                             ;; Middle split words: complete words
                             (inner-loop (cdr rest)
                                         (cons (cons (car rest) #t) words)))))))))))))))))

;; Expand a word without word splitting or globbing
;; Used for assignments, here-docs, etc.
(def (expand-word-nosplit word env)
  (if (string? word)
    (expand-string word env)
    word))

;; Expand heredoc body: $, `, \ escaping, but NO quote processing (" and ' are literal)
(def (expand-heredoc-body body env)
  (let ((len (string-length body))
        (out (open-output-string)))
    (let loop ((i 0))
      (if (>= i len)
        (get-output-string out)
        (let ((ch (string-ref body i)))
          (cond
            ;; Dollar expansion
            ((char=? ch #\$)
             (let-values (((expanded end) (expand-dollar body i env)))
               (display expanded out)
               (loop end)))
            ;; Backtick command substitution
            ((char=? ch #\`)
             (let-values (((expanded end) (expand-backtick body i env)))
               (display expanded out)
               (loop end)))
            ;; Backslash: only escape $, `, \, and newline in heredocs
            ((char=? ch #\\)
             (if (< (+ i 1) len)
               (let ((next (string-ref body (+ i 1))))
                 (if (memv next '(#\$ #\` #\\ #\newline))
                   (begin
                     (display next out)
                     (loop (+ i 2)))
                   (begin
                     (display #\\ out)
                     (display next out)
                     (loop (+ i 2)))))
               (begin
                 (display #\\ out)
                 (loop (+ i 1)))))
            ;; Everything else is literal (including " and ')
            (else
             (display ch out)
             (loop (+ i 1)))))))))

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
               (let ((next (string-ref str (+ i 1))))
                 (if (and (*in-dquote-context*)
                          (not (memq next '(#\$ #\` #\" #\\ #\newline))))
                   ;; In double-quote context, preserve backslash for non-special chars
                   (begin (display "\\" out) (display next out) (loop (+ i 2)))
                   ;; Outside double quotes or special char: consume backslash
                   (begin (display next out) (loop (+ i 2)))))
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
    (when (and (not val) (env-option? env "nounset"))
      (nounset-error! name env))
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
      ;; ${#name[@]} or ${#name[*]} — array length
      ((and (> len 1) (char=? (string-ref content 0) #\#))
       (let* ((rest (substring content 1 len))
              (bracket-pos (string-find-char-from rest #\[ 0)))
         (if (and bracket-pos
                  (let ((subscript (substring rest (+ bracket-pos 1)
                                             (- (string-length rest) 1))))
                    (or (string=? subscript "@") (string=? subscript "*"))))
           ;; ${#name[@]} — number of elements
           (let ((name (substring rest 0 bracket-pos)))
             (number->string (env-array-length env name)))
           ;; ${#name} — string length (or ${#name[idx]} — element length)
           (if bracket-pos
             ;; ${#name[idx]} — length of specific element
             (let* ((name (substring rest 0 bracket-pos))
                    (close (string-find-char-from rest #\] (+ bracket-pos 1)))
                    (idx (substring rest (+ bracket-pos 1) (or close (string-length rest))))
                    (val (env-array-get env name (expand-word-nosplit idx env))))
               (number->string (string-length val)))
             ;; ${#name} — string length
             (let ((val (or (env-get env rest) "")))
               (number->string (string-length val)))))))
      ;; ${!name[@]} or ${!name[*]} — array keys/indices
      ;; Also handle ${!name} — indirect expansion
      ((and (> len 1)
            (or (char=? (string-ref content 0) #\!)
                (and (> len 2) (char=? (string-ref content 0) #\\)
                     (char=? (string-ref content 1) #\!))))
       (let* ((name-start (if (char=? (string-ref content 0) #\!) 1 2))
              (rest (substring content name-start len))
              (bracket-pos (string-find-char-from rest #\[ 0)))
         (if (and bracket-pos
                  (let ((close (string-find-char-from rest #\] (+ bracket-pos 1))))
                    (and close
                         (let ((subscript (substring rest (+ bracket-pos 1) close)))
                           (or (string=? subscript "@") (string=? subscript "*"))))))
           ;; ${!name[@]} — array keys
           (let* ((name (substring rest 0 bracket-pos))
                  (keys (env-array-keys env name)))
             (string-join-with " " keys))
           ;; ${!name} — indirect expansion, possibly with modifiers
           (let-values (((iname modifier arg) (parse-parameter-modifier rest)))
             (let* ((ref-name (env-get env iname))
                    (val (if ref-name (env-get env ref-name) #f)))
               (if modifier
                 (apply-parameter-modifier val (or ref-name iname) modifier arg env)
                 (or val "")))))))
      (else
       ;; Check for array subscript: name[idx] or name[@] or name[*]
       (let ((bracket-pos (find-array-subscript-start content)))
         (if bracket-pos
           (expand-array-parameter content bracket-pos env)
           ;; Regular parameter — find modifier
           (let-values (((name modifier arg) (parse-parameter-modifier content)))
             (let ((val (env-get env name)))
               ;; nounset check: only for bare ${name} or modifiers that don't provide defaults
               (when (and (not val) (env-option? env "nounset")
                          (not (memq modifier '(:- - := = :+ + :? ?))))
                 (nounset-error! name env))
               (apply-parameter-modifier val name modifier arg env)))))))))

;; Find the start of an array subscript [idx] in parameter content.
;; Returns position of [ or #f. Must be preceded by a valid name.
(def (find-array-subscript-start content)
  (let ((len (string-length content)))
    (let loop ((i 0))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref content i) #\[)
         ;; Check that the part before [ is a valid name
         (if (and (> i 0)
                  (let loop2 ((j 0))
                    (or (= j i)
                        (and (let ((ch (string-ref content j)))
                               (or (char-alphabetic? ch) (char-numeric? ch) (char=? ch #\_)))
                             (loop2 (+ j 1))))))
           i
           #f))
        (else (loop (+ i 1)))))))

;; Expand an array parameter like name[idx], name[@], name[*]
;; bracket-pos is the position of [ in content.
;; Handles modifiers after the ] too (e.g. ${arr[@]:-default}).
(def (expand-array-parameter content bracket-pos env)
  (let* ((name (substring content 0 bracket-pos))
         (len (string-length content))
         (close (string-find-char-from content #\] (+ bracket-pos 1))))
    (if (not close)
      ;; No closing ] — treat as regular parameter
      (let-values (((pname modifier arg) (parse-parameter-modifier content)))
        (let ((val (env-get env pname)))
          (apply-parameter-modifier val pname modifier arg env)))
      ;; Extract subscript and any trailing modifier
      (let* ((subscript (substring content (+ bracket-pos 1) close))
             (after-bracket (substring content (+ close 1) len)))
        (cond
          ;; ${name[@]} — all elements (separate words in some contexts)
          ((and (string=? subscript "@") (string=? after-bracket ""))
           (let ((vals (env-array-values env name)))
             (string-join-with " " vals)))
          ;; ${name[*]} — all elements (single word)
          ((and (string=? subscript "*") (string=? after-bracket ""))
           (let* ((vals (env-array-values env name))
                  (ifs (or (env-get env "IFS") " \t\n"))
                  (sep (if (> (string-length ifs) 0)
                         (string (string-ref ifs 0))
                         "")))
             (string-join-with sep vals)))
          ;; ${name[@]modifier} or ${name[*]modifier} — with modifier
          ((and (or (string=? subscript "@") (string=? subscript "*"))
                (> (string-length after-bracket) 0))
           ;; Parse the modifier from after-bracket
           (let* ((vals (env-array-values env name)))
             (let-values (((mname modifier arg)
                           (parse-parameter-modifier
                            (string-append "x" after-bracket))))
               ;; For @, apply modifier per-element
               (if (string=? subscript "@")
                 (let ((modified-vals
                        (map (lambda (v)
                               (apply-parameter-modifier v name modifier arg env))
                             vals)))
                   (string-join-with " " modified-vals))
                 ;; For *, join first then apply
                 (let* ((ifs (or (env-get env "IFS") " \t\n"))
                        (sep (if (> (string-length ifs) 0) (string (string-ref ifs 0)) ""))
                        (joined (string-join-with sep vals)))
                   (apply-parameter-modifier (if (null? vals) #f joined)
                                            name modifier arg env))))))
          ;; ${name[idx]} — single element access
          (else
           (let ((expanded-idx (expand-word-nosplit subscript env)))
             (if (string=? after-bracket "")
               ;; Simple element access
               (env-array-get env name expanded-idx)
               ;; Element access with modifier: ${arr[0]:-default}
               (let* ((val (env-array-get env name expanded-idx)))
                 (let-values (((mname modifier arg)
                               (parse-parameter-modifier
                                (string-append "x" after-bracket))))
                   (apply-parameter-modifier (if (string=? val "") #f val)
                                            name modifier arg env)))))))))))

;; Parse NAME and modifier from parameter content
(def (special-param-char? ch)
  (or (char=? ch #\@) (char=? ch #\*)
      (char=? ch #\?) (char=? ch #\!)
      (char=? ch #\$) (char=? ch #\-)))

;; Parse modifier from a rest string starting after the name.
;; Returns (values modifier arg) or #f if no modifier found.
(def (parse-modifier-from-rest rest)
  (let ((rlen (string-length rest)))
    (if (= rlen 0) (values #f "")
      (let ((mod-ch (string-ref rest 0)))
        (cond
          ((char=? mod-ch #\:)
           (if (> rlen 1)
             (let ((mod2 (string-ref rest 1)))
               (case mod2
                 ((#\- #\+ #\= #\?)
                  (values (string->symbol (string #\: mod2))
                          (substring rest 2 rlen)))
                 (else
                  (values ': (substring rest 1 rlen)))))
             (values #f "")))
          ((memq mod-ch '(#\- #\+ #\= #\?))
           (values (string->symbol (string mod-ch))
                   (substring rest 1 rlen)))
          ((char=? mod-ch #\%)
           (if (and (> rlen 1) (char=? (string-ref rest 1) #\%))
             (values '%% (substring rest 2 rlen))
             (values '% (substring rest 1 rlen))))
          ((char=? mod-ch #\#)
           (if (and (> rlen 1) (char=? (string-ref rest 1) #\#))
             (values 'prefix-long (substring rest 2 rlen))
             (values 'prefix-short (substring rest 1 rlen))))
          (else #f))))))

(def (parse-parameter-modifier content)
  (let ((len (string-length content)))
    ;; Handle special parameters: @, *, ?, !, $, -, 0-9
    ;; These are single-char names — modifier starts at position 1
    (if (and (> len 0)
             (let ((ch (string-ref content 0)))
               (or (special-param-char? ch)
                   (and (char-numeric? ch)
                        (or (= len 1)
                            (not (char-numeric? (string-ref content 1))))))))
      (let* ((name (string (string-ref content 0)))
             (rest (substring content 1 len)))
        (let-values (((modifier arg) (parse-modifier-from-rest rest)))
          (if modifier
            (values name modifier arg)
            (values name #f ""))))
    ;; Find the modifier operator
    (let loop ((i 0))
      (cond
        ((>= i len) (values content #f ""))
        ((memq (string-ref content i) '(#\: #\% #\# #\/ #\^ #\,))
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
        (else (loop (+ i 1))))))))

;; Pattern substitution helpers for ${var/pattern/repl} and ${var//pattern/repl}
;; Returns string with first match of glob pattern replaced
(def (pattern-substitute-first val pattern replacement (extglob? #f))
  (let ((vlen (string-length val)))
    (cond
      ;; ${var/#pattern/repl} — anchor to start (prefix match)
      ((and (> (string-length pattern) 0) (char=? (string-ref pattern 0) #\#))
       (let ((pat (substring pattern 1 (string-length pattern))))
         ;; Try longest match from start
         (let loop ((i vlen))
           (if (< i 0)
             val
             (if (glob-match? pat (substring val 0 i) #f extglob?)
               (string-append replacement (substring val i vlen))
               (loop (- i 1)))))))
      ;; ${var/%pattern/repl} — anchor to end (suffix match)
      ((and (> (string-length pattern) 0) (char=? (string-ref pattern 0) #\%))
       (let ((pat (substring pattern 1 (string-length pattern))))
         ;; Try longest match from end
         (let loop ((i 0))
           (if (> i vlen)
             val
             (if (glob-match? pat (substring val i vlen) #f extglob?)
               (string-append (substring val 0 i) replacement)
               (loop (+ i 1)))))))
      ;; Unanchored — find first match anywhere
      (else
       (let loop ((start 0))
         (if (> start vlen)
           val
           ;; Try all end positions from longest to shortest
           (let inner ((end vlen))
             (cond
               ((< end start)
                ;; No match at this start position, try next
                (loop (+ start 1)))
               ((glob-match? pattern (substring val start end) #f extglob?)
                ;; Found a match
                (string-append (substring val 0 start)
                               replacement
                               (substring val end vlen)))
               (else (inner (- end 1)))))))))))

;; Returns string with all matches of glob pattern replaced
(def (pattern-substitute-all val pattern replacement (extglob? #f))
  (let ((vlen (string-length val)))
    (if (string=? pattern "")
      val  ;; Empty pattern — no replacement
      (let ((buf (open-output-string)))
        (let loop ((start 0))
          (if (> start vlen)
            (get-output-string buf)
            ;; Try all end positions from longest to shortest
            (let inner ((end vlen))
              (cond
                ((< end (+ start 1))
                 ;; No match at this start, emit char and move on
                 (if (< start vlen)
                   (begin
                     (display (string-ref val start) buf)
                     (loop (+ start 1)))
                   (get-output-string buf)))
                ((glob-match? pattern (substring val start end) #f extglob?)
                 ;; Found match — emit replacement and skip past match
                 (display replacement buf)
                 (if (= end start)
                   ;; Zero-length match — advance by one to avoid infinite loop
                   (begin
                     (when (< start vlen)
                       (display (string-ref val start) buf))
                     (loop (+ start 1)))
                   (loop end)))
                (else (inner (- end 1)))))))))))

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
    ((%) (if val (remove-suffix val (expand-string arg env) #f (env-shopt? env "extglob")) ""))
    ;; ${name%%pattern} — remove longest suffix
    ((%%) (if val (remove-suffix val (expand-string arg env) #t (env-shopt? env "extglob")) ""))
    ;; ${name#pattern} — remove shortest prefix
    ((prefix-short) (if val (remove-prefix val (expand-string arg env) #f (env-shopt? env "extglob")) ""))
    ;; ${name##pattern} — remove longest prefix
    ((prefix-long) (if val (remove-prefix val (expand-string arg env) #t (env-shopt? env "extglob")) ""))
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
    ;; ${name/pattern/replacement} — replace first match
    ((/) (if val
           (let ((pattern (expand-string (car arg) env))
                 (replacement (expand-string (cdr arg) env)))
             (pattern-substitute-first val pattern replacement
                                       (env-shopt? env "extglob")))
           ""))
    ;; ${name//pattern/replacement} — replace all matches
    ((//) (if val
            (let ((pattern (expand-string (car arg) env))
                  (replacement (expand-string (cdr arg) env)))
              (pattern-substitute-all val pattern replacement
                                      (env-shopt? env "extglob")))
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

(def (expand-backtick str i env (in-dquote? #f))
  ;; ` ... ` — find matching backtick
  ;; Process backtick-specific escaping:
  ;; \$ → $, \` → `, \\ → \  (always, per POSIX)
  ;; \" → "  (only when backtick is inside double quotes)
  ;; \x for other x → \x  (backslash preserved)
  (let* ((len (string-length str))
         (end (let loop ((j (+ i 1)))
                (cond
                  ((>= j len) j)
                  ((char=? (string-ref str j) #\\)
                   (loop (+ j 2)))
                  ((char=? (string-ref str j) #\`)
                   j)
                  (else (loop (+ j 1))))))
         (raw (substring str (+ i 1) end))
         (cmd (let ((rlen (string-length raw))
                    (buf (open-output-string))
                    (special (if in-dquote?
                               '(#\$ #\` #\\ #\" #\newline)
                               '(#\$ #\` #\\ #\newline))))
                (let loop ((k 0))
                  (cond
                    ((>= k rlen) (get-output-string buf))
                    ((char=? (string-ref raw k) #\\)
                     (if (< (+ k 1) rlen)
                       (let ((next (string-ref raw (+ k 1))))
                         (if (memq next special)
                           (begin (display next buf) (loop (+ k 2)))
                           (begin (display #\\ buf) (display next buf) (loop (+ k 2)))))
                       (begin (display #\\ buf) (loop (+ k 1)))))
                    (else (display (string-ref raw k) buf) (loop (+ k 1)))))))
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
               ;; Execute in subshell context so exit doesn't terminate parent
               (with-catch
                (lambda (e)
                  (when (subshell-exit-exception? e)
                    (env-set-last-status! env (subshell-exit-exception-status e))))
                (lambda ()
                  (parameterize ((*in-subshell* #t))
                    (exec-fn cmd env))))
               ;; Flush and close the pipe port
               (force-output pipe-port)
               (close-port pipe-port))
             ;; Restore real fd 1 and Gambit port
             (ffi-dup2 saved-fd 1)
             (ffi-close-fd saved-fd)
             (current-output-port saved-port)
             ;; Read the output from the pipe read end using raw read()
             ;; (open-input-file "/dev/fd/N" blocks on empty pipes in Gambit)
             (let ((output (ffi-read-all-from-fd read-fd)))
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
  ;; Pre-expand $var, ${var}, $(cmd) inside the expression before arith-eval
  (let* ((close (find-arith-close str (+ i 3)))
         (raw-expr (substring str (+ i 3) close))
         (expr (expand-arith-expr raw-expr env))
         (result (arith-eval expr
                            (arith-env-getter env)
                            (arith-env-setter env)
                            (env-option? env "nounset"))))
    (values (number->string result) (+ close 2))))

;; Expand $var, ${var}, $(cmd) inside an arithmetic expression
;; but leave bare variable names alone (they're resolved by arith-eval)
(def (expand-arith-expr expr env)
  (let ((len (string-length expr))
        (buf (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len) (get-output-string buf))
        ((and (char=? (string-ref expr i) #\$)
              (< (+ i 1) len))
         (let ((next (string-ref expr (+ i 1))))
           (cond
             ;; $(( ... )) — nested arithmetic substitution
             ((and (char=? next #\()
                   (< (+ i 2) len)
                   (char=? (string-ref expr (+ i 2)) #\())
              (let-values (((val end) (expand-arith-sub expr i env)))
                (display val buf)
                (loop end)))
             ;; $( ... ) — command substitution
             ((char=? next #\()
              (let-values (((val end) (expand-command-sub expr i env)))
                (display val buf)
                (loop end)))
             ;; ${ ... } — braced parameter expansion
             ((char=? next #\{)
              (let-values (((val end) (expand-parameter-braced expr i env)))
                (display val buf)
                (loop end)))
             ;; $name — simple variable
             ((or (char-alphabetic? next) (char=? next #\_))
              (let-values (((val end) (expand-simple-var expr i env)))
                (display val buf)
                (loop end)))
             ;; $N — positional parameter
             ((char-numeric? next)
              (let-values (((val end) (expand-simple-var expr i env)))
                (display val buf)
                (loop end)))
             ;; $? $! $# etc.
             ((memv next '(#\? #\! #\# #\- #\$ #\0))
              (let-values (((val end) (expand-simple-var expr i env)))
                (display val buf)
                (loop end)))
             (else
              (display #\$ buf)
              (loop (+ i 1))))))
        ;; Backquote substitution
        ((char=? (string-ref expr i) #\`)
         (let-values (((val end) (expand-backtick expr i env)))
           (display val buf)
           (loop end)))
        ;; Quoted strings: pass through removing quotes for arith purposes
        ((char=? (string-ref expr i) #\')
         ;; Single-quoted string in arithmetic — the content is a literal
         (let qloop ((j (+ i 1)))
           (cond
             ((>= j len) (display (substring expr i len) buf) (loop len))
             ((char=? (string-ref expr j) #\')
              (display (substring expr (+ i 1) j) buf)
              (loop (+ j 1)))
             (else (qloop (+ j 1))))))
        ((char=? (string-ref expr i) #\")
         ;; Double-quoted string in arithmetic — expand contents
         (let qloop ((j (+ i 1)))
           (cond
             ((>= j len) (display (substring expr i len) buf) (loop len))
             ((char=? (string-ref expr j) #\")
              (let ((inner (substring expr (+ i 1) j)))
                (display (expand-arith-expr inner env) buf)
                (loop (+ j 1))))
             ((char=? (string-ref expr j) #\\)
              (qloop (min (+ j 2) len)))
             (else (qloop (+ j 1))))))
        (else
         (display (string-ref expr i) buf)
         (loop (+ i 1)))))))

;;; --- Word splitting ---

;; POSIX IFS field splitting algorithm:
;; - IFS whitespace chars (space, tab, newline in IFS): collapse consecutive, trim leading/trailing
;; - IFS non-whitespace chars: each is a delimiter, adjacent ones produce empty fields
;; - Mixed: IFS whitespace around non-whitespace delimiter is trimmed (doesn't create extra fields)
;; - Leading non-whitespace IFS char produces leading empty field
;; - Trailing non-whitespace IFS char does NOT produce trailing empty field
(def (word-split str env)
  (let ((ifs (or (env-get env "IFS") " \t\n")))
    (if (string=? ifs "")
      [str]  ;; empty IFS = no splitting
      (let ((len (string-length str)))
        (if (= len 0) [str]
          ;; Classify IFS characters
          (let ((ifs-ws? (lambda (ch) (and (ifs-char? ch ifs)
                                           (or (char=? ch #\space)
                                               (char=? ch #\tab)
                                               (char=? ch #\newline)))))
                (ifs-nws? (lambda (ch) (and (ifs-char? ch ifs)
                                            (not (or (char=? ch #\space)
                                                     (char=? ch #\tab)
                                                     (char=? ch #\newline)))))))
            ;; Algorithm: scan left to right.
            ;; State: accumulating non-IFS chars into a field, or at a delimiter boundary.
            (let loop ((i 0) (start 0) (words []))
              (cond
                ((>= i len)
                 ;; End of string — trailing IFS delimiters do NOT produce empty fields
                 (let ((words (if (> i start)
                                (cons (substring str start i) words)
                                words)))
                   (reverse words)))
                ;; Non-whitespace IFS delimiter
                ((ifs-nws? (string-ref str i))
                 ;; Add the field before this delimiter (may be empty for leading/adjacent)
                 (let ((words (cons (substring str start i) words)))
                   ;; Skip any IFS whitespace after the delimiter
                   (let skip-ws ((j (+ i 1)))
                     (if (and (< j len) (ifs-ws? (string-ref str j)))
                       (skip-ws (+ j 1))
                       (loop j j words)))))
                ;; IFS whitespace
                ((ifs-ws? (string-ref str i))
                 ;; Skip all consecutive IFS whitespace
                 (let skip-ws ((j (+ i 1)))
                   (cond
                     ;; Whitespace followed by non-whitespace IFS delimiter:
                     ;; the nws delimiter is the actual field separator
                     ((and (< j len) (ifs-nws? (string-ref str j)))
                      ;; Add field before whitespace if there's content
                      (let ((words (if (> i start)
                                     (cons (substring str start i) words)
                                     words)))
                        ;; Skip past the nws delimiter and any trailing ws
                        (let skip-ws2 ((k (+ j 1)))
                          (if (and (< k len) (ifs-ws? (string-ref str k)))
                            (skip-ws2 (+ k 1))
                            (loop k k words)))))
                     ;; Plain whitespace delimiter (no adjacent nws)
                     ((and (< j len) (ifs-ws? (string-ref str j)))
                      (skip-ws (+ j 1)))
                     ;; End of whitespace run - it's a plain ws delimiter
                     (else
                      (let ((words (if (> i start)
                                     (cons (substring str start i) words)
                                     words)))
                        (loop j j words))))))
                ;; Regular character
                (else
                 (loop (+ i 1) start words))))))))))

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
;; Check if a word has top-level quotes (not inside $() or ``)
;; Quotes inside command substitutions don't suppress word splitting
(def (word-has-quotes? word)
  (let ((len (string-length word)))
    (let loop ((i 0) (depth 0))
      (if (>= i len) #f
          (let ((ch (string-ref word i)))
            (cond
              ;; Top-level ' or " → word is quoted
              ((and (= depth 0) (or (char=? ch #\') (char=? ch #\")))
               #t)
              ;; $( opens a nesting level
              ((and (char=? ch #\$) (< (+ i 1) len)
                    (char=? (string-ref word (+ i 1)) #\())
               (loop (+ i 2) (+ depth 1)))
              ;; $((  opens a nesting level (arithmetic)
              ((and (char=? ch #\$) (< (+ i 2) len)
                    (char=? (string-ref word (+ i 1)) #\()
                    (char=? (string-ref word (+ i 2)) #\())
               (loop (+ i 3) (+ depth 1)))
              ;; ( inside $() increments depth
              ((and (> depth 0) (char=? ch #\())
               (loop (+ i 1) (+ depth 1)))
              ;; ) decrements depth
              ((and (> depth 0) (char=? ch #\)))
               (loop (+ i 1) (- depth 1)))
              ;; Backtick at top level counts as nesting
              ((and (= depth 0) (char=? ch #\`))
               (let skip-bt ((j (+ i 1)))
                 (cond
                   ((>= j len) #f)
                   ((char=? (string-ref word j) #\`)
                    (loop (+ j 1) 0))
                   ((char=? (string-ref word j) #\\)
                    (skip-bt (+ j 2)))
                   (else (skip-bt (+ j 1))))))
              ;; Backslash at top level: skip next char
              ((and (= depth 0) (char=? ch #\\))
               (loop (+ i 2) 0))
              (else (loop (+ i 1) depth))))))))

;; Check if a word contains $@ or ${@} or ${name[@]} inside double quotes (not inside $() or ``)
;; Used to detect the need for "$@" / "${arr[@]}" multi-word expansion
(def (word-has-quoted-at? word)
  (let ((len (string-length word)))
    (let loop ((i 0) (in-dq #f) (depth 0))
      (if (>= i len) #f
          (let ((ch (string-ref word i)))
            (cond
              ;; Backslash: skip next char
              ((char=? ch #\\) (loop (min (+ i 2) len) in-dq depth))
              ;; Double quote: only toggle at depth 0
              ((and (char=? ch #\") (= depth 0))
               (loop (+ i 1) (not in-dq) depth))
              ;; Single quote outside double-quotes at depth 0: skip to closing
              ((and (= depth 0) (not in-dq) (char=? ch #\'))
               (let skip ((j (+ i 1)))
                 (if (or (>= j len) (char=? (string-ref word j) #\'))
                   (loop (+ j 1) in-dq depth)
                   (skip (+ j 1)))))
              ;; $( — increase depth (handles both $( and $(()
              ((and (char=? ch #\$) (< (+ i 1) len)
                    (char=? (string-ref word (+ i 1)) #\())
               (loop (+ i 2) in-dq (+ depth 1)))
              ;; ( inside command sub — increase depth
              ((and (> depth 0) (char=? ch #\())
               (loop (+ i 1) in-dq (+ depth 1)))
              ;; ) — decrease depth
              ((and (> depth 0) (char=? ch #\)))
               (loop (+ i 1) in-dq (- depth 1)))
              ;; Backtick at depth 0: skip to matching backtick
              ((and (= depth 0) (char=? ch #\`))
               (let skip ((j (+ i 1)))
                 (cond
                   ((>= j len) (loop j in-dq depth))
                   ((char=? (string-ref word j) #\\) (skip (+ j 2)))
                   ((char=? (string-ref word j) #\`) (loop (+ j 1) in-dq depth))
                   (else (skip (+ j 1))))))
              ;; $@ at depth 0 inside double quotes — MATCH
              ((and in-dq (= depth 0) (char=? ch #\$) (< (+ i 1) len)
                    (char=? (string-ref word (+ i 1)) #\@))
               #t)
              ;; ${@} at depth 0 inside double quotes — MATCH
              ((and in-dq (= depth 0) (char=? ch #\$) (< (+ i 3) len)
                    (char=? (string-ref word (+ i 1)) #\{)
                    (char=? (string-ref word (+ i 2)) #\@)
                    (char=? (string-ref word (+ i 3)) #\}))
               #t)
              ;; ${name[@]} at depth 0 inside double quotes — MATCH
              ((and in-dq (= depth 0) (char=? ch #\$) (< (+ i 1) len)
                    (char=? (string-ref word (+ i 1)) #\{))
               ;; Scan for [@]} pattern
               (let scan ((j (+ i 2)))
                 (cond
                   ((>= j len) (loop (+ i 1) in-dq depth))
                   ;; Found [@]} — match!
                   ((and (char=? (string-ref word j) #\[)
                         (< (+ j 2) len)
                         (char=? (string-ref word (+ j 1)) #\@)
                         (char=? (string-ref word (+ j 2)) #\]))
                    ;; Check for closing }
                    (let ((k (+ j 3)))
                      (if (and (< k len) (char=? (string-ref word k) #\}))
                        #t
                        (loop (+ i 1) in-dq depth))))
                   ;; Valid identifier chars — keep scanning
                   ((or (char-alphabetic? (string-ref word j))
                        (char-numeric? (string-ref word j))
                        (char=? (string-ref word j) #\_))
                    (scan (+ j 1)))
                   ;; Not an array[@] pattern — fall through
                   (else (loop (+ i 1) in-dq depth)))))
              (else (loop (+ i 1) in-dq depth))))))))

;; Find $@ or ${@} or ${name[@]} inside double quotes and split into prefix/suffix
;; Returns (values prefix-str suffix-str array-name-or-false) where quotes are balanced
;; array-name is #f for $@/${@}, or the array name string for ${name[@]}
(def (split-word-at-quoted-at word)
  (let ((len (string-length word)))
    (let loop ((i 0) (in-dq #f) (depth 0))
      (cond
        ((>= i len)
         (values word "" #f))
        ((char=? (string-ref word i) #\\)
         (loop (min (+ i 2) len) in-dq depth))
        ((and (char=? (string-ref word i) #\") (= depth 0))
         (loop (+ i 1) (not in-dq) depth))
        ((and (= depth 0) (not in-dq) (char=? (string-ref word i) #\'))
         (let skip ((j (+ i 1)))
           (if (or (>= j len) (char=? (string-ref word j) #\'))
             (loop (+ j 1) in-dq depth)
             (skip (+ j 1)))))
        ((and (char=? (string-ref word i) #\$) (< (+ i 1) len)
              (char=? (string-ref word (+ i 1)) #\())
         (loop (+ i 2) in-dq (+ depth 1)))
        ((and (> depth 0) (char=? (string-ref word i) #\())
         (loop (+ i 1) in-dq (+ depth 1)))
        ((and (> depth 0) (char=? (string-ref word i) #\)))
         (loop (+ i 1) in-dq (- depth 1)))
        ((and (= depth 0) (char=? (string-ref word i) #\`))
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) (loop j in-dq depth))
             ((char=? (string-ref word j) #\\) (skip (+ j 2)))
             ((char=? (string-ref word j) #\`) (loop (+ j 1) in-dq depth))
             (else (skip (+ j 1))))))
        ;; $@ inside double quotes at depth 0
        ((and in-dq (= depth 0) (char=? (string-ref word i) #\$) (< (+ i 1) len)
              (char=? (string-ref word (+ i 1)) #\@))
         ;; Close double quote for prefix, reopen for suffix
         (values (string-append (substring word 0 i) "\"")
                 (string-append "\"" (substring word (+ i 2) len))
                 #f))
        ;; ${@} inside double quotes at depth 0
        ((and in-dq (= depth 0) (char=? (string-ref word i) #\$) (< (+ i 3) len)
              (char=? (string-ref word (+ i 1)) #\{)
              (char=? (string-ref word (+ i 2)) #\@)
              (char=? (string-ref word (+ i 3)) #\}))
         (values (string-append (substring word 0 i) "\"")
                 (string-append "\"" (substring word (+ i 4) len))
                 #f))
        ;; ${name[@]} inside double quotes at depth 0
        ((and in-dq (= depth 0) (char=? (string-ref word i) #\$) (< (+ i 1) len)
              (char=? (string-ref word (+ i 1)) #\{))
         ;; Try to match name[@]}
         (let scan ((j (+ i 2)))
           (cond
             ((>= j len) (loop (+ i 1) in-dq depth))
             ((and (char=? (string-ref word j) #\[)
                   (< (+ j 3) len)
                   (char=? (string-ref word (+ j 1)) #\@)
                   (char=? (string-ref word (+ j 2)) #\])
                   (char=? (string-ref word (+ j 3)) #\}))
              (let ((arr-name (substring word (+ i 2) j))
                    (end-pos (+ j 4)))
                (values (string-append (substring word 0 i) "\"")
                        (string-append "\"" (substring word end-pos len))
                        arr-name)))
             ((or (char-alphabetic? (string-ref word j))
                  (char-numeric? (string-ref word j))
                  (char=? (string-ref word j) #\_))
              (scan (+ j 1)))
             (else (loop (+ i 1) in-dq depth)))))
        (else
         (loop (+ i 1) in-dq depth))))))

;; Expand a word containing "$@" or "${arr[@]}" in double quotes to multiple words
(def (expand-word-with-at word env)
  (let-values (((prefix-raw suffix-raw array-name) (split-word-at-quoted-at word)))
    (let ((elements (if array-name
                      (env-array-values env array-name)
                      (env-positional-list env)))
          (expanded-prefix (expand-string prefix-raw env))
          (expanded-suffix (expand-string suffix-raw env)))
      (if (null? elements)
        ;; No elements: "$@"/"${arr[@]}" produces nothing, but prefix/suffix may remain
        (let ((combined (string-append expanded-prefix expanded-suffix)))
          (if (string=? combined "") [] [combined]))
        ;; Has elements
        (let ((n (length elements)))
          (if (= n 1)
            [(string-append expanded-prefix (car elements) expanded-suffix)]
            (let* ((first (string-append expanded-prefix (car elements)))
                   (last-val (string-append (last elements) expanded-suffix))
                   (middle (let mid-loop ((ps (cdr elements)) (acc []))
                             (if (null? (cdr ps))
                               (reverse acc)
                               (mid-loop (cdr ps) (cons (car ps) acc))))))
              (append [first] middle [last-val]))))))))

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
         (let-values (((expanded end)
                       (parameterize ((*in-dquote-context* #t))
                         (expand-dollar str j env))))
           (display expanded buf)
           (loop end)))
        ((char=? (string-ref str j) #\`)
         (let-values (((expanded end) (expand-backtick str j env #t)))
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
  ;; Tracks case/esac nesting so ) in case patterns doesn't close early
  (let ((len (string-length str)))
    (define (update-case-depth w cd)
      (cond ((string=? w "case") (+ cd 1))
            ((string=? w "esac") (max 0 (- cd 1)))
            (else cd)))
    (let loop ((i start) (depth 1) (case-depth 0) (word ""))
      (cond
        ((>= i len) (- len 1))
        ((char=? (string-ref str i) #\()
         (let ((cd (update-case-depth word case-depth)))
           (loop (+ i 1) (+ depth 1) cd "")))
        ((char=? (string-ref str i) #\))
         (let ((cd (update-case-depth word case-depth)))
           (if (and (= depth 1) (> cd 0))
             ;; Inside case: ) is pattern delimiter, not closing
             (loop (+ i 1) depth cd "")
             ;; Normal close
             (if (= depth 1) i (loop (+ i 1) (- depth 1) cd "")))))
        ((char=? (string-ref str i) #\\)
         (let ((cd (update-case-depth word case-depth)))
           (loop (+ i 2) depth cd "")))
        ((char=? (string-ref str i) #\') ;; skip single-quoted
         (let ((cd (update-case-depth word case-depth)))
           (let sq ((j (+ i 1)))
             (cond ((>= j len) (loop j depth cd ""))
                   ((char=? (string-ref str j) #\') (loop (+ j 1) depth cd ""))
                   (else (sq (+ j 1)))))))
        ((char=? (string-ref str i) #\") ;; skip double-quoted
         (let ((cd (update-case-depth word case-depth)))
           (let dq ((j (+ i 1)))
             (cond ((>= j len) (loop j depth cd ""))
                   ((char=? (string-ref str j) #\\) (dq (+ j 2)))
                   ((char=? (string-ref str j) #\") (loop (+ j 1) depth cd ""))
                   (else (dq (+ j 1)))))))
        ;; Word characters: accumulate for keyword detection
        ((or (char-alphabetic? (string-ref str i))
             (char=? (string-ref str i) #\_))
         (loop (+ i 1) depth case-depth
               (string-append word (string (string-ref str i)))))
        ;; Non-word: flush keyword
        (else
         (let ((cd (update-case-depth word case-depth)))
           (loop (+ i 1) depth cd "")))))))

(def (find-arith-close str start)
  ;; Find matching )) for $(( ... ))
  ;; Must track nested (( )) pairs AND single ( ) for expressions like ~(1|2)
  (let ((len (string-length str)))
    (let loop ((i start) (paren-depth 0))
      (cond
        ((>= i len) (- len 2))
        ;; Check for )) — only close if no open single parens
        ((and (= paren-depth 0)
              (char=? (string-ref str i) #\))
              (< (+ i 1) len)
              (char=? (string-ref str (+ i 1)) #\)))
         i)
        ;; Nested $(( — skip
        ((and (< (+ i 2) len)
              (char=? (string-ref str i) #\$)
              (char=? (string-ref str (+ i 1)) #\()
              (char=? (string-ref str (+ i 2)) #\())
         ;; Find matching )) for nested arith, then continue
         (let ((inner-close (find-arith-close str (+ i 3))))
           (loop (+ inner-close 2) paren-depth)))
        ;; $( — command substitution, skip to matching )
        ((and (< (+ i 1) len)
              (char=? (string-ref str i) #\$)
              (char=? (string-ref str (+ i 1)) #\())
         (let ((close (find-matching-paren str (+ i 2))))
           (loop (+ close 1) paren-depth)))
        ;; Single ( increases depth
        ((char=? (string-ref str i) #\()
         (loop (+ i 1) (+ paren-depth 1)))
        ;; Single ) decreases depth (must be inside parens)
        ((and (char=? (string-ref str i) #\))
              (> paren-depth 0))
         (loop (+ i 1) (- paren-depth 1)))
        (else (loop (+ i 1) paren-depth))))))

;;; --- Pattern matching helpers ---

(def (remove-suffix val pattern longest? (extglob? #f))
  ;; Remove suffix matching pattern
  (if longest?
    ;; Longest: try from start
    (let loop ((i 0))
      (if (> i (string-length val))
        val
        (if (glob-match? pattern (substring val i (string-length val)) #f extglob?)
          (substring val 0 i)
          (loop (+ i 1)))))
    ;; Shortest: try from end
    (let loop ((i (string-length val)))
      (if (< i 0)
        val
        (if (glob-match? pattern (substring val i (string-length val)) #f extglob?)
          (substring val 0 i)
          (loop (- i 1)))))))

(def (remove-prefix val pattern longest? (extglob? #f))
  (if longest?
    (let loop ((i (string-length val)))
      (if (< i 0)
        val
        (if (glob-match? pattern (substring val 0 i) #f extglob?)
          (substring val i (string-length val))
          (loop (- i 1)))))
    (let loop ((i 0))
      (if (> i (string-length val))
        val
        (if (glob-match? pattern (substring val 0 i) #f extglob?)
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

;;; --- Brace expansion ---

;; Expand brace expressions in a word. Returns a list of words.
;; {a,b,c} → (a b c), {1..5} → (1 2 3 4 5)
;; Preamble/postscript: pre{a,b}post → (preapost prebpost)
;; Nested: {a,b{1,2}} → (a b1 b2)
;; Does NOT expand inside quotes or ${...}
(def (brace-expand word)
  (let ((result (brace-expand-once word)))
    (if (and (= (length result) 1) (string=? (car result) word))
      result  ;; no expansion happened
      ;; Recursively expand each result (for nested braces)
      (append-map brace-expand result))))

;; Find and expand the first (leftmost) valid brace expression in word.
;; Returns a list of expanded words, or [word] if no valid brace found.
(def (brace-expand-once word)
  (let ((len (string-length word)))
    ;; Find the first valid { that isn't quoted or part of ${
    (let find-open ((i 0))
      (cond
        ((>= i len) [word])  ;; no brace found
        ;; Skip single-quoted regions
        ((char=? (string-ref word i) #\')
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) [word])
             ((char=? (string-ref word j) #\') (find-open (+ j 1)))
             (else (skip (+ j 1))))))
        ;; Skip double-quoted regions
        ((char=? (string-ref word i) #\")
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) [word])
             ((char=? (string-ref word j) #\") (find-open (+ j 1)))
             ((char=? (string-ref word j) #\\) (skip (+ j 2)))
             (else (skip (+ j 1))))))
        ;; Skip ${ parameter expansion
        ((and (char=? (string-ref word i) #\$)
              (< (+ i 1) len)
              (char=? (string-ref word (+ i 1)) #\{))
         (let skip ((j (+ i 2)) (depth 1))
           (cond
             ((>= j len) [word])
             ((char=? (string-ref word j) #\})
              (if (= depth 1) (find-open (+ j 1)) (skip (+ j 1) (- depth 1))))
             ((char=? (string-ref word j) #\{) (skip (+ j 1) (+ depth 1)))
             ((char=? (string-ref word j) #\\) (skip (+ j 2) depth))
             (else (skip (+ j 1) depth)))))
        ;; Skip backslash-escaped chars
        ((char=? (string-ref word i) #\\)
         (find-open (+ i 2)))
        ;; Found { — try to find matching }
        ((char=? (string-ref word i) #\{)
         (let ((close (find-brace-close word (+ i 1))))
           (if close
             (let ((content (substring word (+ i 1) close))
                   (preamble (substring word 0 i))
                   (postscript (substring word (+ close 1) len)))
               ;; Check if this is a valid brace expression
               (cond
                 ;; Comma-separated: {a,b,c}
                 ((brace-has-comma? content)
                  (let ((parts (brace-split-commas content)))
                    (map (lambda (part)
                           (string-append preamble part postscript))
                         parts)))
                 ;; Sequence: {start..end} or {start..end..step}
                 ((brace-sequence? content)
                  (let ((seq (brace-expand-sequence content)))
                    (if seq
                      (map (lambda (item)
                             (string-append preamble item postscript))
                           seq)
                      [word])))
                 ;; Not valid — literal brace
                 (else [word])))
             ;; No matching } — literal
             [word])))
        (else (find-open (+ i 1)))))))

;; Find matching } for brace expansion, respecting nesting and quoting
(def (find-brace-close str start)
  (let ((len (string-length str)))
    (let loop ((i start) (depth 1))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref str i) #\\) (loop (+ i 2) depth))
        ((char=? (string-ref str i) #\')
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) #f)
             ((char=? (string-ref str j) #\') (loop (+ j 1) depth))
             (else (skip (+ j 1))))))
        ((char=? (string-ref str i) #\")
         (let skip ((j (+ i 1)))
           (cond
             ((>= j len) #f)
             ((char=? (string-ref str j) #\") (loop (+ j 1) depth))
             ((char=? (string-ref str j) #\\) (skip (+ j 2)))
             (else (skip (+ j 1))))))
        ((char=? (string-ref str i) #\{) (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref str i) #\})
         (if (= depth 1) i (loop (+ i 1) (- depth 1))))
        (else (loop (+ i 1) depth))))))

;; Check if brace content has an unquoted, unnested comma
(def (brace-has-comma? content)
  (let ((len (string-length content)))
    (let loop ((i 0) (depth 0))
      (cond
        ((>= i len) #f)
        ((char=? (string-ref content i) #\\) (loop (+ i 2) depth))
        ((char=? (string-ref content i) #\{) (loop (+ i 1) (+ depth 1)))
        ((char=? (string-ref content i) #\}) (loop (+ i 1) (- depth 1)))
        ((and (char=? (string-ref content i) #\,) (= depth 0)) #t)
        (else (loop (+ i 1) depth))))))

;; Split brace content by top-level commas, respecting nesting
(def (brace-split-commas content)
  (let ((len (string-length content)))
    (let loop ((i 0) (start 0) (depth 0) (parts []))
      (cond
        ((>= i len)
         (reverse (cons (substring content start i) parts)))
        ((char=? (string-ref content i) #\\) (loop (+ i 2) start depth parts))
        ((char=? (string-ref content i) #\{) (loop (+ i 1) start (+ depth 1) parts))
        ((char=? (string-ref content i) #\}) (loop (+ i 1) start (- depth 1) parts))
        ((and (char=? (string-ref content i) #\,) (= depth 0))
         (loop (+ i 1) (+ i 1) depth
               (cons (substring content start i) parts)))
        (else (loop (+ i 1) start depth parts))))))

;; Check if content looks like a sequence: start..end or start..end..step
(def (brace-sequence? content)
  (and (string-contains? content "..")
       (not (brace-has-comma? content))))

;; Expand a brace sequence: "1..5" → ("1" "2" "3" "4" "5")
;; Supports numeric and alphabetic ranges, with optional step
(def (brace-expand-sequence content)
  (let* ((parts (string-split-dot-dot content))
         (nparts (length parts)))
    (cond
      ;; start..end
      ((= nparts 2)
       (brace-range (car parts) (cadr parts) #f))
      ;; start..end..step
      ((= nparts 3)
       (brace-range (car parts) (cadr parts) (caddr parts)))
      (else #f))))

;; Split by ".." — returns list of 2 or 3 parts
(def (string-split-dot-dot str)
  (let ((len (string-length str)))
    (let loop ((i 0) (start 0) (parts []))
      (cond
        ((>= i len)
         (reverse (cons (substring str start i) parts)))
        ((and (< (+ i 1) len)
              (char=? (string-ref str i) #\.)
              (char=? (string-ref str (+ i 1)) #\.))
         (loop (+ i 2) (+ i 2)
               (cons (substring str start i) parts)))
        (else (loop (+ i 1) start parts))))))

;; Generate a range from start to end with optional step
(def (brace-range start-str end-str step-str)
  (let ((start-num (string->number start-str))
        (end-num (string->number end-str))
        (step-num (and step-str (string->number step-str))))
    (cond
      ;; Numeric range
      ((and start-num end-num)
       (let* ((step (or step-num 1))
              (step (if (< end-num start-num) (- (abs step)) (abs step)))
              ;; Zero-padding: if either has leading zeros, pad to max width
              (pad-width (max (string-length start-str) (string-length end-str)))
              (needs-pad? (or (and (> (string-length start-str) 1)
                                   (char=? (string-ref start-str 0) #\0))
                              (and (> (string-length end-str) 1)
                                   (char=? (string-ref end-str 0) #\0)))))
         (if (= step 0) #f
           (let loop ((i start-num) (result []))
             (if (if (> step 0) (> i end-num) (< i end-num))
               (reverse result)
               (loop (+ i step)
                     (cons (if needs-pad?
                             (pad-number i pad-width)
                             (number->string i))
                           result)))))))
      ;; Single-char alphabetic range
      ((and (= (string-length start-str) 1) (= (string-length end-str) 1)
            (char-alphabetic? (string-ref start-str 0))
            (char-alphabetic? (string-ref end-str 0)))
       (let* ((start-ch (char->integer (string-ref start-str 0)))
              (end-ch (char->integer (string-ref end-str 0)))
              (step (or (and step-num (inexact->exact step-num))
                        (if (<= start-ch end-ch) 1 -1)))
              (step (if (< end-ch start-ch) (- (abs step)) (abs step))))
         (if (= step 0) #f
           (let loop ((i start-ch) (result []))
             (if (if (> step 0) (> i end-ch) (< i end-ch))
               (reverse result)
               (loop (+ i step)
                     (cons (string (integer->char i)) result)))))))
      (else #f))))

;; Pad an integer to a given width with leading zeros
(def (pad-number n width)
  (let* ((s (number->string (abs n)))
         (padding (max 0 (- width (string-length s) (if (< n 0) 1 0)))))
    (string-append (if (< n 0) "-" "")
                   (make-string padding #\0)
                   s)))
