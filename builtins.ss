;;; builtins.ss — Built-in command registry and implementations for gsh

(export #t)
(import :std/sugar
        :std/format
        :std/iter
        :std/sort
        :std/pregexp
        :std/os/signal
        :gsh/ast
        :gsh/ffi
        :gsh/environment
        :gsh/expander
        :gsh/functions
        :gsh/jobs
        :gsh/signals
        :gsh/history
        :gsh/util)

;;; --- Built-in registry ---

(def *builtins* (make-hash-table))

;; Register a built-in command
(def (builtin-register! name handler)
  (hash-put! *builtins* name handler))

;; Look up a built-in by name
(def (builtin-lookup name)
  (hash-get *builtins* name))

;; List all built-in names
(def (builtin-list)
  (sort! (hash-keys *builtins*) string<?))

;; Check if a name is a built-in
(def (builtin? name)
  (and (builtin-lookup name) #t))

;;; --- Built-in implementations ---
;;; Each handler: (lambda (args env) -> exit-status)

;; : (colon) — no-op, always succeeds
(builtin-register! ":"
  (lambda (args env) 0))

;; true
(builtin-register! "true"
  (lambda (args env) 0))

;; false
(builtin-register! "false"
  (lambda (args env) 1))

;; echo [-neE] [args...]
(builtin-register! "echo"
  (lambda (args env)
    (let loop ((args args) (newline? #t) (escape? #f))
      (cond
        ;; Process flags
        ((and (pair? args) (string=? (car args) "-n"))
         (loop (cdr args) #f escape?))
        ((and (pair? args) (string=? (car args) "-e"))
         (loop (cdr args) newline? #t))
        ((and (pair? args) (string=? (car args) "-E"))
         (loop (cdr args) newline? #f))
        (else
         ;; Print arguments
         (let arg-loop ((rest args) (first? #t))
           (when (pair? rest)
             (unless first? (display " "))
             (if escape?
               (display (echo-expand-escapes (car rest)))
               (display (car rest)))
             (arg-loop (cdr rest) #f)))
         (when newline? (newline))
         (force-output)
         0)))))

;; printf format [args...]
(builtin-register! "printf"
  (lambda (args env)
    (if (null? args)
      (begin (fprintf (current-error-port) "printf: usage: printf format [arguments]~n") 1)
      (begin
        ;; Simplified printf — handles %s %d %% \n \t
        (let ((fmt (car args))
              (rest (cdr args)))
          (display (simple-printf fmt rest))
          (force-output)
          0)))))

;; cd [-L|-P] [dir]
(builtin-register! "cd"
  (lambda (args env)
    (let* ((dir (cond
                  ((null? args) (or (env-get env "HOME") (error "cd: HOME not set")))
                  ((string=? (car args) "-") (or (env-get env "OLDPWD") (error "cd: OLDPWD not set")))
                  ((string=? (car args) "-L") (if (pair? (cdr args)) (cadr args) (env-get env "HOME")))
                  ((string=? (car args) "-P") (if (pair? (cdr args)) (cadr args) (env-get env "HOME")))
                  (else (car args))))
           (expanded (expand-word-nosplit dir env)))
      (with-catch
       (lambda (e)
         (fprintf (current-error-port) "cd: ~a: No such file or directory~n" expanded)
         1)
       (lambda ()
         (let ((old-pwd (current-directory)))
           (current-directory expanded)
           (env-set! env "OLDPWD" old-pwd)
           (env-set! env "PWD" (current-directory))
           (when (string=? (car args) "-")
             (displayln (current-directory)))
           0))))))

;; pwd [-L|-P]
(builtin-register! "pwd"
  (lambda (args env)
    (displayln (current-directory))
    0))

;; export [name[=value] ...]
(builtin-register! "export"
  (lambda (args env)
    (if (null? args)
      ;; List all exports
      (begin
        (for-each
         (lambda (pair)
           (displayln (format "declare -x ~a=\"~a\"" (car pair) (cdr pair))))
         (env-exported-alist-pairs env))
        0)
      ;; Export variables
      (begin
        (for-each
         (lambda (arg)
           (let ((eq-pos (string-find-char* arg #\=)))
             (if eq-pos
               (let ((name (substring arg 0 eq-pos))
                     (value (substring arg (+ eq-pos 1) (string-length arg))))
                 (env-export! env name value))
               (env-export! env arg))))
         args)
        0))))

;; unset [-fv] name ...
(builtin-register! "unset"
  (lambda (args env)
    (let loop ((args args) (unset-func? #f))
      (cond
        ((null? args) 0)
        ((string=? (car args) "-f")
         (loop (cdr args) #t))
        ((string=? (car args) "-v")
         (loop (cdr args) #f))
        (else
         (for-each
          (lambda (name)
            (if unset-func?
              (function-unset! env name)
              (with-catch (lambda (e) #!void)
                (lambda () (env-unset! env name)))))
          args)
         0)))))

;; readonly [name[=value] ...]
(builtin-register! "readonly"
  (lambda (args env)
    (if (null? args)
      0  ;; TODO: list readonly vars
      (begin
        (for-each
         (lambda (arg)
           (let ((eq-pos (string-find-char* arg #\=)))
             (if eq-pos
               (let ((name (substring arg 0 eq-pos))
                     (value (substring arg (+ eq-pos 1) (string-length arg))))
                 (env-readonly! env name value))
               (env-readonly! env arg))))
         args)
        0))))

;; exit [n]
(builtin-register! "exit"
  (lambda (args env)
    (let ((code (if (pair? args) (or (string->number (car args)) 0) 0)))
      ;; In a subshell, raise exception instead of terminating
      (if (*in-subshell*)
        (raise (make-subshell-exit-exception code))
        (begin
          ;; Run EXIT trap if set
          (let ((exit-trap (trap-get "EXIT")))
            (when (and exit-trap (string? exit-trap))
              (let ((exec-fn (*execute-input*)))
                (when exec-fn
                  (exec-fn exit-trap env)))))
          (history-save!)
          (exit code))))))

;; return [n]
(builtin-register! "return"
  (lambda (args env)
    (let ((code (if (pair? args) (or (string->number (car args)) 0) 0)))
      (shell-return! code))))

;; break [n]
(builtin-register! "break"
  (lambda (args env)
    (let ((levels (if (pair? args) (or (string->number (car args)) 1) 1)))
      (shell-break! levels))))

;; continue [n]
(builtin-register! "continue"
  (lambda (args env)
    (let ((levels (if (pair? args) (or (string->number (car args)) 1) 1)))
      (shell-continue! levels))))

;; set [options] [-- args...]
(builtin-register! "set"
  (lambda (args env)
    (if (null? args)
      ;; No args: display all variables
      (begin
        ;; TODO: list all variables in posix format
        0)
      (let loop ((args args))
        (cond
          ((null? args) 0)
          ((string=? (car args) "--")
           ;; Set positional parameters
           (env-set-positional! env (cdr args))
           0)
          ((and (> (string-length (car args)) 1)
                (char=? (string-ref (car args) 0) #\-))
           (apply-set-options! env (car args) #t)
           (loop (cdr args)))
          ((and (> (string-length (car args)) 1)
                (char=? (string-ref (car args) 0) #\+))
           (apply-set-options! env (car args) #f)
           (loop (cdr args)))
          (else
           ;; Positional parameters
           (env-set-positional! env args)
           0))))))

;; shift [n]
(builtin-register! "shift"
  (lambda (args env)
    (let* ((n (if (pair? args) (or (string->number (car args)) 1) 1))
           (pos (env-positional-list env)))
      (if (> n (length pos))
        1
        (begin
          (env-set-positional! env (list-tail pos n))
          0)))))

;; eval [args...]
(builtin-register! "eval"
  (lambda (args env)
    (if (null? args)
      0
      (let ((input (string-join-sp args))
            (exec-fn (*execute-input*)))
        (if exec-fn
          (exec-fn input env)
          (begin
            (fprintf (current-error-port) "gsh: eval: executor not initialized~n")
            1))))))

;; test / [ — conditional expressions
(builtin-register! "test"
  (lambda (args env) (test-eval args)))

(builtin-register! "["
  (lambda (args env)
    (if (and (pair? args)
             (string=? (last-elem* args) "]"))
      (test-eval (butlast args))
      (begin
        (fprintf (current-error-port) "[: missing `]'~n")
        2))))

;; type [-afptP] name...
(builtin-register! "type"
  (lambda (args env)
    (let loop ((args args) (status 0))
      (if (null? args)
        status
        (let ((name (car args)))
          (cond
            ((builtin? name)
             (displayln (format "~a is a shell builtin" name))
             (loop (cdr args) status))
            ((function-lookup env name)
             (displayln (format "~a is a function" name))
             (loop (cdr args) status))
            ((which name)
             => (lambda (path)
                  (displayln (format "~a is ~a" name path))
                  (loop (cdr args) status)))
            (else
             (fprintf (current-error-port) "type: ~a: not found~n" name)
             (loop (cdr args) 1))))))))

;; command [-pvV] cmd [args...]
(builtin-register! "command"
  (lambda (args env)
    (cond
      ((null? args) 0)
      ((string=? (car args) "-v")
       ;; Print path
       (if (pair? (cdr args))
         (let ((path (which (cadr args))))
           (if path (begin (displayln path) 0)
               (begin (fprintf (current-error-port) "~a: not found~n" (cadr args)) 1)))
         1))
      ((string=? (car args) "-V")
       ;; Verbose type
       (if (pair? (cdr args))
         (let ((name (cadr args)))
           (cond
             ((which name) => (lambda (p) (displayln (format "~a is ~a" name p)) 0))
             (else (fprintf (current-error-port) "~a: not found~n" name) 1)))
         1))
      (else 0))))  ;; Actual execution handled by executor

;; alias [name[=value] ...]
(builtin-register! "alias"
  (lambda (args env)
    (if (null? args)
      ;; List all aliases
      (begin
        (for-each
         (lambda (pair)
           (displayln (format "alias ~a='~a'" (car pair) (cdr pair))))
         (alias-list env))
        0)
      (begin
        (for-each
         (lambda (arg)
           (let ((eq-pos (string-find-char* arg #\=)))
             (if eq-pos
               (alias-set! env
                          (substring arg 0 eq-pos)
                          (substring arg (+ eq-pos 1) (string-length arg)))
               ;; Show single alias
               (let ((val (alias-get env arg)))
                 (if val
                   (displayln (format "alias ~a='~a'" arg val))
                   (fprintf (current-error-port) "alias: ~a: not found~n" arg))))))
         args)
        0))))

;; unalias [-a] name...
(builtin-register! "unalias"
  (lambda (args env)
    (cond
      ((and (pair? args) (string=? (car args) "-a"))
       (alias-clear! env) 0)
      (else
       (for-each (lambda (name) (alias-unset! env name)) args)
       0))))

;; read [-r] [-p prompt] [-t timeout] [-d delim] [-n count] [-s] var...
(builtin-register! "read"
  (lambda (args env)
    (let loop ((args args) (raw? #f) (prompt "") (vars []))
      (cond
        ((null? args)
         ;; Do the read
         (when (> (string-length prompt) 0)
           (display prompt (current-error-port))
           (force-output (current-error-port)))
         (let ((line (read-line)))
           (if (eof-object? line)
             1
             (let* ((var-names (if (null? vars) ["REPLY"] (reverse vars)))
                    (ifs (or (env-get env "IFS") " \t\n"))
                    (fields (split-by-ifs line ifs (length var-names))))
               (let field-loop ((names var-names) (fields fields))
                 (cond
                   ((null? names) 0)
                   ((null? fields)
                    (env-set! env (car names) "")
                    (field-loop (cdr names) []))
                   ((null? (cdr names))
                    ;; Last var gets remainder
                    (env-set! env (car names)
                              (string-join-sp fields))
                    0)
                   (else
                    (env-set! env (car names) (car fields))
                    (field-loop (cdr names) (cdr fields)))))))))
        ((string=? (car args) "-r")
         (loop (cdr args) #t prompt vars))
        ((string=? (car args) "-p")
         (if (pair? (cdr args))
           (loop (cddr args) raw? (cadr args) vars)
           (loop (cdr args) raw? prompt vars)))
        ((string=? (car args) "-s")
         (loop (cdr args) raw? prompt vars))
        (else
         (loop (cdr args) raw? prompt (cons (car args) vars)))))))

;; trap ['command'] [signal ...]
(builtin-register! "trap"
  (lambda (args env)
    (cond
      ((null? args)
       ;; Print all traps
       (for-each
        (lambda (pair)
          (if (string? (cdr pair))
            (displayln (format "trap -- '~a' ~a" (cdr pair) (car pair)))
            (displayln (format "trap -- '' ~a" (car pair)))))
        (trap-list))
       0)
      ((string=? (car args) "-l")
       ;; List signal names
       (for-each displayln (signal-name-list))
       0)
      ((string=? (car args) "-p")
       ;; Print specific traps
       (for-each
        (lambda (sig)
          (let ((action (trap-get sig)))
            (when action
              (if (string? action)
                (displayln (format "trap -- '~a' ~a" action sig))
                (displayln (format "trap -- '' ~a" sig))))))
        (if (pair? (cdr args)) (cdr args) (map car (trap-list))))
       0)
      ((and (>= (length args) 2))
       ;; Set trap: trap 'command' SIGNAL...
       (let ((action (car args))
             (signals (cdr args)))
         (for-each
          (lambda (sig)
            (cond
              ((string=? action "-") (trap-set! sig 'default))
              ((string=? action "") (trap-set! sig 'ignore))
              (else (trap-set! sig action))))
          signals)
         0))
      (else 0))))

;; jobs [-lnprs]
(builtin-register! "jobs"
  (lambda (args env)
    (job-update-status!)
    (for-each
     (lambda (job)
       (displayln (format "[~a] ~a ~a"
                         (job-id job)
                         (case (job-status job)
                           ((running) "Running")
                           ((stopped) "Stopped")
                           ((done) "Done")
                           ((killed) "Killed")
                           (else "???"))
                         (job-command-text job))))
     (job-table-list))
    ;; Clean up completed jobs after listing
    (job-table-cleanup!)
    0))

;; fg [jobspec]
(builtin-register! "fg"
  (lambda (args env)
    (let ((spec (if (pair? args) (car args) "%%")))
      (let ((job (job-table-get spec)))
        (if job
          (begin
            (displayln (job-command-text job))
            (job-foreground! job))
          (begin
            (fprintf (current-error-port) "fg: ~a: no such job~n" spec)
            1))))))

;; bg [jobspec...]
(builtin-register! "bg"
  (lambda (args env)
    (let ((specs (if (null? args) ["%%"] args)))
      (for-each
       (lambda (spec)
         (let ((job (job-table-get spec)))
           (if job
             (job-background! job)
             (fprintf (current-error-port) "bg: ~a: no such job~n" spec))))
       specs)
      0)))

;; wait [id...]
(builtin-register! "wait"
  (lambda (args env)
    (let ((result
           (if (null? args)
             ;; Wait for all background jobs
             (let loop ((jobs (job-table-list)) (status 0))
               (if (null? jobs)
                 status
                 (loop (cdr jobs) (job-wait (car jobs)))))
             ;; Wait for specific jobs
             (let loop ((args args) (status 0))
               (if (null? args)
                 status
                 (let ((job (job-table-get (car args))))
                   (if job
                     (loop (cdr args) (job-wait job))
                     (loop (cdr args) 127))))))))
      ;; Clean up completed jobs
      (job-table-cleanup!)
      result)))

;; kill [-signal] pid|jobspec
(builtin-register! "kill"
  (lambda (args env)
    (if (null? args)
      (begin
        (fprintf (current-error-port) "kill: usage: kill [-signal] pid|jobspec~n")
        1)
      (let loop ((args args) (sig SIGTERM))
        (cond
          ((null? args) 0)
          ((and (> (string-length (car args)) 1)
                (char=? (string-ref (car args) 0) #\-))
           (let* ((sig-name (substring (car args) 1 (string-length (car args))))
                  (sig-num (or (signal-name->number sig-name)
                              (string->number sig-name)
                              SIGTERM)))
             (loop (cdr args) sig-num)))
          (else
           (for-each
            (lambda (target)
              (cond
                ((char=? (string-ref target 0) #\%)
                 (let ((job (job-table-get target)))
                   (when job
                     (for-each
                      (lambda (proc)
                        (kill (job-process-pid proc) sig))
                      (job-processes job)))))
                (else
                 (let ((pid (string->number target)))
                   (when pid (kill pid sig))))))
            args)
           0))))))

;; history [n]
(builtin-register! "history"
  (lambda (args env)
    (let* ((entries (history-list))
           (n (if (pair? args) (or (string->number (car args)) (length entries))
                  (length entries)))
           (to-show (if (< n (length entries))
                      (list-tail entries (- (length entries) n))
                      entries)))
      (let loop ((entries to-show) (num (- (length entries) (length to-show) -1)))
        (when (pair? entries)
          (displayln (format "  ~a  ~a" num (car entries)))
          (loop (cdr entries) (+ num 1))))
      0)))

;; local var[=value] ...
(builtin-register! "local"
  (lambda (args env)
    (for-each
     (lambda (arg)
       (let ((eq-pos (string-find-char* arg #\=)))
         (if eq-pos
           (let ((name (substring arg 0 eq-pos))
                 (value (substring arg (+ eq-pos 1) (string-length arg))))
             (hash-put! (shell-environment-vars env) name
                        (make-shell-var value #f #f #t #f #f #f #f)))
           (hash-put! (shell-environment-vars env) arg
                      (make-shell-var "" #f #f #t #f #f #f #f)))))
     args)
    0))

;; declare/typeset [-aAfFgilnrtux] [-p] [name[=value] ...]
(def (builtin-declare args env)
  (let ((flags (make-hash-table))
        (names [])
        (print? #f)
        (remove? #f))
    ;; Parse flags and name arguments
    (let loop ((args args))
      (when (pair? args)
        (let ((arg (car args)))
          (cond
            ;; Flag argument: -xyz or +xyz
            ((and (> (string-length arg) 1)
                  (or (char=? (string-ref arg 0) #\-)
                      (char=? (string-ref arg 0) #\+))
                  (char-alphabetic? (string-ref arg 1)))
             (let ((remove (char=? (string-ref arg 0) #\+)))
               (let floop ((i 1))
                 (when (< i (string-length arg))
                   (let ((ch (string-ref arg i)))
                     (case ch
                       ((#\p) (set! print? #t))
                       ((#\x) (hash-put! flags 'export (not remove)))
                       ((#\r) (hash-put! flags 'readonly (not remove)))
                       ((#\i) (hash-put! flags 'integer (not remove)))
                       ((#\l) (hash-put! flags 'lowercase (not remove)))
                       ((#\u) (hash-put! flags 'uppercase (not remove)))
                       ((#\n) (hash-put! flags 'nameref (not remove)))
                       ((#\a) (hash-put! flags 'array #t))
                       ((#\A) (hash-put! flags 'assoc #t))
                       ((#\g) (hash-put! flags 'global #t))
                       ;; -f: functions, -F: function names only
                       ((#\f #\F) #!void)
                       (else #!void)))
                   (floop (+ i 1))))
               (loop (cdr args))))
            ;; Name or name=value
            (else
             (set! names (cons arg names))
             (loop (cdr args)))))))
    ;; If -p with no names: print all variables with given attributes
    (when (and print? (null? names))
      (hash-for-each
       (lambda (name var)
         (display-declare-var name var))
       (shell-environment-vars env))
      (return-from-declare 0))
    ;; If no flags and no names: print all variables
    (when (and (= (hash-length flags) 0) (null? names) (not print?))
      (hash-for-each
       (lambda (name var)
         (display-declare-var name var))
       (shell-environment-vars env))
      (return-from-declare 0))
    ;; Process each name
    (for-each
     (lambda (arg)
       (let* ((eq-pos (string-find-char* arg #\=))
              (name (if eq-pos (substring arg 0 eq-pos) arg))
              (value (if eq-pos
                       (substring arg (+ eq-pos 1) (string-length arg))
                       #f)))
         (if print?
           ;; -p name: print declaration
           (let ((var (env-get-var env name)))
             (when var (display-declare-var name var)))
           ;; Apply attributes
           (let ((var (or (hash-get (shell-environment-vars env) name)
                          (let ((new-var (make-shell-var (or value "")
                                                        #f #f #f #f #f #f #f)))
                            (hash-put! (shell-environment-vars env) name new-var)
                            new-var))))
             ;; Set value if provided
             (when value
               (let ((final (apply-var-attrs var value env)))
                 (set! (shell-var-value var) final)
                 (when (shell-var-exported? var)
                   (setenv name final))))
             ;; Apply flags
             (when (hash-get flags 'export)
               (set! (shell-var-exported? var) #t)
               (setenv name (shell-var-value var)))
             (when (hash-key? flags 'export)
               (unless (hash-get flags 'export)
                 (set! (shell-var-exported? var) #f)))
             (when (hash-get flags 'readonly)
               (set! (shell-var-readonly? var) #t))
             (when (hash-get flags 'integer)
               (set! (shell-var-integer? var) #t)
               ;; Apply arithmetic evaluation to current value
               (set! (shell-var-value var)
                     (apply-var-attrs var (shell-var-value var) env)))
             (when (hash-key? flags 'integer)
               (unless (hash-get flags 'integer)
                 (set! (shell-var-integer? var) #f)))
             (when (hash-get flags 'uppercase)
               (set! (shell-var-uppercase? var) #t)
               (set! (shell-var-lowercase? var) #f)
               (set! (shell-var-value var) (string-upcase (shell-var-value var))))
             (when (hash-get flags 'lowercase)
               (set! (shell-var-lowercase? var) #t)
               (set! (shell-var-uppercase? var) #f)
               (set! (shell-var-value var) (string-downcase (shell-var-value var))))
             (when (hash-get flags 'nameref)
               (set! (shell-var-nameref? var) #t))))))
     (reverse names))
    0))

;; Helper: print a variable declaration
(def (display-declare-var name var)
  (let ((flags (string-append
                (if (shell-var-exported? var) "x" "")
                (if (shell-var-readonly? var) "r" "")
                (if (shell-var-integer? var) "i" "")
                (if (shell-var-uppercase? var) "u" "")
                (if (shell-var-lowercase? var) "l" "")
                (if (shell-var-nameref? var) "n" ""))))
    (let ((flag-str (if (string=? flags "") "--" (string-append "-" flags))))
      (displayln (format "declare ~a ~a=\"~a\"" flag-str name (shell-var-value var))))))

;; Continuation for early return from declare
(def return-from-declare (make-parameter #f))

(builtin-register! "declare"
  (lambda (args env)
    (let/cc return
      (parameterize ((return-from-declare return))
        (builtin-declare args env)))))

(builtin-register! "typeset"
  (lambda (args env)
    (let/cc return
      (parameterize ((return-from-declare return))
        (builtin-declare args env)))))

;; let expr...
(builtin-register! "let"
  (lambda (args env)
    (let loop ((args args) (result 0))
      (if (null? args)
        (if (= result 0) 1 0)  ;; let returns 1 if last expr is 0
        (let ((val (arith-eval-wrapper (car args) env)))
          (loop (cdr args) val))))))

;; shopt [-su] [optname...]
(builtin-register! "shopt"
  (lambda (args env)
    (cond
      ((null? args)
       ;; List all shopts
       0)
      ((string=? (car args) "-s")
       (for-each (lambda (name) (env-shopt-set! env name #t)) (cdr args))
       0)
      ((string=? (car args) "-u")
       (for-each (lambda (name) (env-shopt-set! env name #f)) (cdr args))
       0)
      (else 0))))

;; help [pattern]
(builtin-register! "help"
  (lambda (args env)
    (if (null? args)
      (begin
        (displayln "gsh - Gerbil Shell")
        (displayln "Built-in commands:")
        (for-each (lambda (name) (display "  ") (displayln name))
                  (builtin-list))
        0)
      0)))

;; umask [-S] [mode]
(builtin-register! "umask"
  (lambda (args env)
    (if (null? args)
      ;; Display current umask
      (let ((mask (ffi-umask 0)))
        (ffi-umask mask)  ;; restore
        (displayln (format "~4,'0o" mask))
        0)
      ;; Set umask
      (let ((mode (string->number (car args) 8)))
        (if mode
          (begin (ffi-umask mode) 0)
          (begin (fprintf (current-error-port) "umask: ~a: invalid octal number~n" (car args)) 1))))))

;; dirs [-clpv]
(builtin-register! "dirs"
  (lambda (args env)
    (for-each displayln (cons (current-directory) (shell-environment-dir-stack env)))
    0))

;; pushd [dir]
(builtin-register! "pushd"
  (lambda (args env)
    (let ((dir (if (pair? args) (car args)
                   ;; Swap top two
                   (if (pair? (shell-environment-dir-stack env))
                     (car (shell-environment-dir-stack env))
                     (begin (fprintf (current-error-port) "pushd: no other directory~n")
                            #f)))))
      (if dir
        (let ((old (current-directory)))
          (with-catch
           (lambda (e)
             (fprintf (current-error-port) "pushd: ~a: No such file or directory~n" dir)
             1)
           (lambda ()
             (current-directory (expand-word-nosplit dir env))
             (set! (shell-environment-dir-stack env)
               (cons old (shell-environment-dir-stack env)))
             (env-set! env "PWD" (current-directory))
             ;; Print stack
             (display (current-directory))
             (for-each (lambda (d) (display " ") (display d))
                       (shell-environment-dir-stack env))
             (newline)
             0)))
        1))))

;; popd [+N|-N]
(builtin-register! "popd"
  (lambda (args env)
    (let ((stack (shell-environment-dir-stack env)))
      (if (null? stack)
        (begin (fprintf (current-error-port) "popd: directory stack empty~n") 1)
        (let ((dir (car stack)))
          (set! (shell-environment-dir-stack env) (cdr stack))
          (current-directory dir)
          (env-set! env "OLDPWD" (env-get env "PWD"))
          (env-set! env "PWD" (current-directory))
          (display (current-directory))
          (for-each (lambda (d) (display " ") (display d))
                    (shell-environment-dir-stack env))
          (newline)
          0)))))

;;; --- test/[ implementation ---

(def (test-eval args)
  (cond
    ((null? args) 1)  ;; false
    ((= (length args) 1)
     ;; Single arg: true if non-empty string
     (if (> (string-length (car args)) 0) 0 1))
    ((= (length args) 2)
     ;; Unary: -flag arg
     (test-unary (car args) (cadr args)))
    ((= (length args) 3)
     ;; Binary or negation
     (if (string=? (car args) "!")
       (if (= (test-eval (cdr args)) 0) 1 0)
       (test-binary (car args) (cadr args) (caddr args))))
    ((and (>= (length args) 3) (string=? (car args) "!"))
     (if (= (test-eval (cdr args)) 0) 1 0))
    (else 1)))

(def (test-unary flag arg)
  (case (string->symbol flag)
    ((-z) (if (= (string-length arg) 0) 0 1))
    ((-n) (if (> (string-length arg) 0) 0 1))
    ((-e) (if (file-exists? arg) 0 1))
    ((-f) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'regular)) 0 1))
    ((-d) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'directory)) 0 1))
    ((-r) (if (file-exists? arg) 0 1))  ;; simplified
    ((-w) (if (file-exists? arg) 0 1))  ;; simplified
    ((-x) (if (and (file-exists? arg) (executable? arg)) 0 1))
    ((-s) (if (and (file-exists? arg)
                    (> (file-info-size (file-info arg)) 0)) 0 1))
    ((-L -h) (if (and (file-exists? arg)
                       (eq? (file-info-type (file-info arg #t)) 'symbolic-link)) 0 1))
    ((-t) (let ((fd (string->number arg)))
             (if (and fd (= (ffi-isatty fd) 1)) 0 1)))
    ((-p) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'fifo)) 0 1))
    (else 1)))

(def (test-binary left op right)
  (case (string->symbol op)
    ((= ==) (if (string=? left right) 0 1))
    ((!=) (if (not (string=? left right)) 0 1))
    ((<) (if (string<? left right) 0 1))
    ((>) (if (string>? left right) 0 1))
    ((-eq) (let ((l (string->number left)) (r (string->number right)))
             (if (and l r (= l r)) 0 1)))
    ((-ne) (let ((l (string->number left)) (r (string->number right)))
             (if (and l r (not (= l r))) 0 1)))
    ((-lt) (let ((l (string->number left)) (r (string->number right)))
             (if (and l r (< l r)) 0 1)))
    ((-le) (let ((l (string->number left)) (r (string->number right)))
             (if (and l r (<= l r)) 0 1)))
    ((-gt) (let ((l (string->number left)) (r (string->number right)))
             (if (and l r (> l r)) 0 1)))
    ((-ge) (let ((l (string->number left)) (r (string->number right)))
             (if (and l r (>= l r)) 0 1)))
    ((-nt) (if (and (file-exists? left) (file-exists? right)
                     (> (time->seconds (file-info-last-modification-time (file-info left)))
                        (time->seconds (file-info-last-modification-time (file-info right))))) 0 1))
    ((-ot) (if (and (file-exists? left) (file-exists? right)
                     (< (time->seconds (file-info-last-modification-time (file-info left)))
                        (time->seconds (file-info-last-modification-time (file-info right))))) 0 1))
    (else 1)))

;;; --- Helpers ---

(def (echo-expand-escapes str)
  (let ((len (string-length str))
        (buf (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len) (get-output-string buf))
        ((and (char=? (string-ref str i) #\\) (< (+ i 1) len))
         (let ((next (string-ref str (+ i 1))))
           (case next
             ((#\n) (display "\n" buf) (loop (+ i 2)))
             ((#\t) (display "\t" buf) (loop (+ i 2)))
             ((#\r) (display "\r" buf) (loop (+ i 2)))
             ((#\a) (display "\a" buf) (loop (+ i 2)))
             ((#\b) (display "\b" buf) (loop (+ i 2)))
             ((#\\) (display "\\" buf) (loop (+ i 2)))
             ((#\0) ;; Octal
              (let oloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 3)
                         (char>=? (string-ref str j) #\0)
                         (char<=? (string-ref str j) #\7))
                  (oloop (+ j 1)
                         (+ (* val 8) (- (char->integer (string-ref str j))
                                         (char->integer #\0)))
                         (+ count 1))
                  (begin
                    (display (string (integer->char val)) buf)
                    (loop j)))))
             ((#\c) (get-output-string buf))  ;; \c = stop output
             (else (display "\\" buf) (display (string next) buf) (loop (+ i 2))))))
        (else
         (display (string-ref str i) buf)
         (loop (+ i 1)))))))

(def (simple-printf fmt args)
  (let ((flen (string-length fmt))
        (buf (open-output-string)))
    (let loop ((i 0) (args args))
      (cond
        ((>= i flen) (get-output-string buf))
        ((and (char=? (string-ref fmt i) #\%) (< (+ i 1) flen))
         (let ((spec (string-ref fmt (+ i 1))))
           (case spec
             ((#\s) (display (if (pair? args) (car args) "") buf)
                    (loop (+ i 2) (if (pair? args) (cdr args) [])))
             ((#\d) (display (if (pair? args) (car args) "0") buf)
                    (loop (+ i 2) (if (pair? args) (cdr args) [])))
             ((#\%) (display "%" buf) (loop (+ i 2) args))
             (else (display "%" buf) (display (string spec) buf)
                   (loop (+ i 2) args)))))
        ((and (char=? (string-ref fmt i) #\\) (< (+ i 1) flen))
         (let ((esc (string-ref fmt (+ i 1))))
           (case esc
             ((#\n) (display "\n" buf))
             ((#\t) (display "\t" buf))
             ((#\r) (display "\r" buf))
             ((#\\) (display "\\" buf))
             (else (display "\\" buf) (display (string esc) buf)))
           (loop (+ i 2) args)))
        (else
         (display (string-ref fmt i) buf)
         (loop (+ i 1) args))))))

(def (apply-set-options! env flag-str enable?)
  (let ((len (string-length flag-str)))
    (let loop ((i 1))
      (when (< i len)
        (let ((ch (string-ref flag-str i)))
          (case ch
            ((#\e) (env-option-set! env "errexit" enable?))
            ((#\f) (env-option-set! env "noglob" enable?))
            ((#\h) (env-option-set! env "hashall" enable?))
            ((#\m) (env-option-set! env "monitor" enable?))
            ((#\C) (env-option-set! env "noclobber" enable?))
            ((#\u) (env-option-set! env "nounset" enable?))
            ((#\x) (env-option-set! env "xtrace" enable?))
            ((#\v) (env-option-set! env "verbose" enable?))
            ((#\n) (env-option-set! env "noexec" enable?))
            ((#\a) (env-option-set! env "allexport" enable?))
            (else #!void))
          (loop (+ i 1)))))))

(def (split-by-ifs str ifs max-fields)
  (let ((len (string-length str)))
    (let loop ((i 0) (start 0) (fields []) (count 1))
      (cond
        ((>= i len)
         (reverse (if (> i start)
                    (cons (substring str start i) fields)
                    fields)))
        ((and (< count max-fields) (ifs-member? (string-ref str i) ifs))
         (loop (+ i 1) (+ i 1)
               (if (> i start) (cons (substring str start i) fields) fields)
               (+ count 1)))
        (else (loop (+ i 1) start fields count))))))

(def (ifs-member? ch ifs)
  (let loop ((i 0))
    (and (< i (string-length ifs))
         (or (char=? ch (string-ref ifs i))
             (loop (+ i 1))))))

(def (string-join-sp lst)
  (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc
            (loop (cdr rest) (string-append acc " " (car rest)))))))

(def (string-find-char* str ch)
  (let loop ((i 0))
    (cond ((>= i (string-length str)) #f)
          ((char=? (string-ref str i) ch) i)
          (else (loop (+ i 1))))))

(def (last-elem* lst)
  (if (null? (cdr lst)) (car lst) (last-elem* (cdr lst))))

(def (butlast lst)
  (if (null? (cdr lst)) []
      (cons (car lst) (butlast (cdr lst)))))

(def (env-exported-alist-pairs env)
  (let ((result []))
    (hash-for-each
     (lambda (name var)
       (when (shell-var-exported? var)
         (set! result (cons (cons name (shell-var-value var)) result))))
     (shell-environment-vars env))
    result))

(def (arith-eval-wrapper expr env)
  (with-catch
   (lambda (e) 0)
   (lambda ()
     (arith-eval expr
                 (lambda (name) (env-get env name))
                 (lambda (name val) (env-set! env name val))))))

;; Import arith-eval from arithmetic module
(import :gsh/arithmetic)
