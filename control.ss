;;; control.ss — Compound commands (if/for/while/until/case/select) for gsh

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/environment
        :gsh/expander
        :gsh/glob
        :gsh/functions)

;;; --- Public interface ---
;;; All functions take an execute-fn callback to avoid circular dependency.
;;; execute-fn: (lambda (ast env) -> exit-status)

;; Execute an if-command
(def (execute-if cmd env execute-fn)
  (let loop ((clauses (if-command-clauses cmd)))
    (if (null? clauses)
      ;; No clause matched — try else
      (if (if-command-else-part cmd)
        (execute-fn (if-command-else-part cmd) env)
        0)
      (let* ((clause (car clauses))
             (condition (car clause))
             (body (cdr clause))
             (test-status (parameterize ((*in-condition-context* #t))
                            (execute-fn condition env))))
        (if (= test-status 0)
          (execute-fn body env)
          (loop (cdr clauses)))))))

;; Execute a for-command
(def (execute-for cmd env execute-fn)
  (let* ((var-name (for-command-var cmd))
         (word-list (for-command-words cmd))
         ;; If words is #f, use positional params ("$@")
         (items (if word-list
                  (expand-words word-list env)
                  (env-at env))))
    (let loop ((items items) (status 0))
      (if (null? items)
        status
        (begin
          (env-set! env var-name (car items))
          (with-catch
           (lambda (e)
             (cond
               ((break-exception? e)
                (if (> (break-exception-levels e) 1)
                  (raise (make-break-exception (- (break-exception-levels e) 1)))
                  status))
               ((continue-exception? e)
                (if (> (continue-exception-levels e) 1)
                  (raise (make-continue-exception (- (continue-exception-levels e) 1)))
                  (loop (cdr items) status)))
               (else (raise e))))
           (lambda ()
             (let ((new-status (execute-fn (for-command-body cmd) env)))
               (loop (cdr items) new-status)))))))))

;; Execute a while-command
(def (execute-while cmd env execute-fn)
  (let loop ((status 0))
    (let ((test-status (parameterize ((*in-condition-context* #t))
                          (execute-fn (while-command-test cmd) env))))
      (if (= test-status 0)
        (with-catch
         (lambda (e)
           (cond
             ((break-exception? e)
              (if (> (break-exception-levels e) 1)
                (raise (make-break-exception (- (break-exception-levels e) 1)))
                status))
             ((continue-exception? e)
              (if (> (continue-exception-levels e) 1)
                (raise (make-continue-exception (- (continue-exception-levels e) 1)))
                (loop status)))
             (else (raise e))))
         (lambda ()
           (let ((new-status (execute-fn (while-command-body cmd) env)))
             (loop new-status))))
        status))))

;; Execute an until-command
(def (execute-until cmd env execute-fn)
  (let loop ((status 0))
    (let ((test-status (parameterize ((*in-condition-context* #t))
                          (execute-fn (until-command-test cmd) env))))
      (if (not (= test-status 0))
        (with-catch
         (lambda (e)
           (cond
             ((break-exception? e)
              (if (> (break-exception-levels e) 1)
                (raise (make-break-exception (- (break-exception-levels e) 1)))
                status))
             ((continue-exception? e)
              (if (> (continue-exception-levels e) 1)
                (raise (make-continue-exception (- (continue-exception-levels e) 1)))
                (loop status)))
             (else (raise e))))
         (lambda ()
           (let ((new-status (execute-fn (until-command-body cmd) env)))
             (loop new-status))))
        status))))

;; Execute a case-command
(def (execute-case cmd env execute-fn)
  (let* ((word (expand-word-nosplit (case-command-word cmd) env))
         (clauses (case-command-clauses cmd)))
    (let loop ((clauses clauses))
      (if (null? clauses)
        0
        (let* ((clause (car clauses))
               (patterns (case-clause-patterns clause))
               (body (case-clause-body clause))
               (terminator (case-clause-terminator clause)))
          (if (any-pattern-matches? patterns word env)
            (let ((status (if body (execute-fn body env) 0)))
              (case terminator
                ;; ;; — break
                ((break) status)
                ;; ;& — fallthrough (execute next clause body unconditionally)
                ((fallthrough)
                 (if (null? (cdr clauses))
                   status
                   (let* ((next-clause (cadr clauses))
                          (next-body (case-clause-body next-clause)))
                     (if next-body
                       (execute-fn next-body env)
                       status))))
                ;; ;;& — test-next (continue checking patterns)
                ((test-next)
                 (let ((rest-status (loop (cdr clauses))))
                   (if (= rest-status 0) status rest-status)))
                (else status)))
            (loop (cdr clauses))))))))

;; Execute a select-command
(def (execute-select cmd env execute-fn)
  (let* ((var-name (select-command-var cmd))
         (word-list (select-command-words cmd))
         (items (expand-words word-list env))
         (ps3 (or (env-get env "PS3") "#? ")))
    ;; Print numbered menu
    (let menu-loop ((status 0))
      ;; Display choices
      (let ((i 1))
        (for-each
         (lambda (item)
           (fprintf (current-error-port) "~a) ~a~n" i item)
           (set! i (+ i 1)))
         items))
      ;; Read selection
      (display ps3 (current-error-port))
      (force-output (current-error-port))
      (let ((line (read-line)))
        (if (eof-object? line)
          status
          (let ((n (string->number (string-trim-ws line))))
            (if (and n (> n 0) (<= n (length items)))
              (begin
                (env-set! env var-name (list-ref items (- n 1)))
                (env-set! env "REPLY" line)
                (with-catch
                 (lambda (e)
                   (cond
                     ((break-exception? e) status)
                     (else (raise e))))
                 (lambda ()
                   (let ((new-status (execute-fn (select-command-body cmd) env)))
                     (menu-loop new-status)))))
              (begin
                (env-set! env var-name "")
                (env-set! env "REPLY" line)
                (let ((new-status (execute-fn (select-command-body cmd) env)))
                  (menu-loop new-status))))))))))

;;; --- Helpers ---

(def (any-pattern-matches? patterns word env)
  (let loop ((pats patterns))
    (if (null? pats)
      #f
      (let ((pat (expand-word-nosplit (car pats) env)))
        (if (or (string=? pat "*")
                (glob-match? pat word))
          #t
          (loop (cdr pats)))))))

(def (string-trim-ws str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))
