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

;; printf [-v var] format [args...]
(builtin-register! "printf"
  (lambda (args env)
    (if (null? args)
      (begin (fprintf (current-error-port) "printf: usage: printf [-v var] format [arguments]~n") 1)
      ;; Parse -v option
      (let-values (((var-name rest-args)
                    (if (and (>= (length args) 3)
                             (string=? (car args) "-v"))
                      (values (cadr args) (cddr args))
                      (values #f args))))
        (if (null? rest-args)
          (begin (fprintf (current-error-port) "printf: usage: printf [-v var] format [arguments]~n") 1)
          (let ((fmt (car rest-args))
                (rest (cdr rest-args)))
            ;; Format with argument recycling: repeat format until all args consumed
            (let ((output (shell-printf fmt rest)))
              (if var-name
                (begin (env-set! env var-name output) 0)
                (begin (display output) (force-output) 0)))))))))

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

;; unset [-fvn] name ...
(builtin-register! "unset"
  (lambda (args env)
    (let loop ((args args) (unset-func? #f) (unset-nameref? #f))
      (cond
        ((null? args) 0)
        ((string=? (car args) "-f")
         (loop (cdr args) #t #f))
        ((string=? (car args) "-v")
         (loop (cdr args) #f #f))
        ((string=? (car args) "-n")
         (loop (cdr args) #f #t))
        (else
         (for-each
          (lambda (name)
            (cond
              (unset-func?
               (function-unset! env name))
              (unset-nameref?
               ;; unset -n: unset the nameref itself, not the target
               (with-catch (lambda (e) #!void)
                 (lambda () (env-unset-nameref! env name))))
              (else
               (with-catch (lambda (e) #!void)
                 (lambda ()
                   ;; Check for array element syntax: name[idx]
                   (let ((bracket-pos (string-find-char* name #\[)))
                     (if (and bracket-pos
                              (> bracket-pos 0)
                              (let ((close (string-find-char* name #\])))
                                (and close (= close (- (string-length name) 1)))))
                       ;; Unset array element
                       (let ((var-name (substring name 0 bracket-pos))
                             (index (substring name (+ bracket-pos 1)
                                              (- (string-length name) 1))))
                         (env-array-unset-element! env var-name index))
                       ;; Unset whole variable (resolves namerefs)
                       (env-unset! env name))))))))
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
    (let ((code (if (pair? args)
                  (let ((n (string->number (car args))))
                    (cond
                      ((not n)
                       (fprintf (current-error-port) "gsh: exit: ~a: numeric argument required~n" (car args))
                       2)
                      ;; Reject values outside 32-bit signed integer range
                      ((or (> n 2147483647) (< n -2147483648))
                       (fprintf (current-error-port) "gsh: exit: ~a: expected a small integer~n" (car args))
                       1)
                      (else (bitwise-and n #xFF))))
                  (shell-environment-last-status env))))
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
    (let ((code (if (pair? args)
                  (let ((n (string->number (car args))))
                    (cond
                      ((not n)
                       (fprintf (current-error-port) "gsh: return: ~a: numeric argument required~n" (car args))
                       2)
                      ;; Reject values outside 32-bit signed integer range
                      ((or (> n 2147483647) (< n -2147483648))
                       (fprintf (current-error-port) "gsh: return: ~a: expected a small integer~n" (car args))
                       1)
                      (else (bitwise-and n #xFF))))
                  (shell-environment-last-status env))))
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
          ;; -o option-name / +o option-name
          ((and (pair? (cdr args))
                (or (string=? (car args) "-o")
                    (string=? (car args) "+o")))
           (let ((enable? (char=? (string-ref (car args) 0) #\-))
                 (opt-name (cadr args)))
             (env-option-set! env opt-name enable?)
             (loop (cddr args))))
          ;; -o without argument: print options
          ((or (string=? (car args) "-o") (string=? (car args) "+o"))
           ;; TODO: print option status
           (loop (cdr args)))
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
    ;; Skip leading -- (eval accepts/ignores it)
    (let ((args (if (and (pair? args) (string=? (car args) "--"))
                  (cdr args)
                  args)))
      (cond
        ((null? args) 0)
        ;; Check for invalid options (starts with - but isn't --)
        ((and (> (string-length (car args)) 1)
              (char=? (string-ref (car args) 0) #\-)
              (not (string=? (car args) "--")))
         (fprintf (current-error-port)
                  "gsh: eval: ~a: invalid option~n" (car args))
         2)
        (else
         (let ((input (string-join-sp args))
               (exec-fn (*execute-input*)))
           (if exec-fn
             (exec-fn input env)
             (begin
               (fprintf (current-error-port) "gsh: eval: executor not initialized~n")
               1))))))))

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
    (let loop ((args args) (raw? #f) (silent? #f) (prompt "")
               (nchars #f) (delim #f) (timeout #f) (fd #f) (vars []))
      (cond
        ((null? args)
         ;; Show prompt on stderr
         (when (> (string-length prompt) 0)
           (display prompt (current-error-port))
           (force-output (current-error-port)))
         ;; Set up input port
         (let* ((in-port (if fd
                           (open-input-file (string-append "/dev/fd/" (number->string fd)))
                           (current-input-port)))
                ;; Disable echo for -s (only on tty)
                (tty? (and silent? (not fd)
                           (with-catch (lambda (e) #f)
                             (lambda () (tty-mode-set! in-port #t #f #f #f 0) #t)))))
           (dynamic-wind
             (lambda () #!void)
             (lambda ()
               ;; Set timeout if requested
               (when timeout
                 (input-port-timeout-set! in-port timeout))
               (let ((line (cond
                             ;; -n nchars: read exactly N characters
                             (nchars
                              (let ((buf (open-output-string)))
                                (let rloop ((count 0))
                                  (if (>= count nchars)
                                    (get-output-string buf)
                                    (let ((ch (read-char in-port)))
                                      (if (eof-object? ch)
                                        (let ((s (get-output-string buf)))
                                          (if (string=? s "") ch s))
                                        (begin (display ch buf)
                                               (rloop (+ count 1)))))))))
                             ;; -d delim: read until delimiter
                             (delim
                              (let ((delim-ch (string-ref delim 0))
                                    (buf (open-output-string)))
                                (let rloop ()
                                  (let ((ch (read-char in-port)))
                                    (cond
                                      ((eof-object? ch)
                                       (let ((s (get-output-string buf)))
                                         (if (string=? s "") ch s)))
                                      ((char=? ch delim-ch)
                                       (get-output-string buf))
                                      (else
                                       (display ch buf)
                                       (rloop)))))))
                             ;; Default: read line, with backslash continuation if not -r
                             (else
                              (if raw?
                                (read-line in-port)
                                ;; Handle backslash-newline continuation
                                (let ((buf (open-output-string)))
                                  (let rloop ()
                                    (let ((l (read-line in-port)))
                                      (cond
                                        ((eof-object? l)
                                         (let ((s (get-output-string buf)))
                                           (if (string=? s "") l s)))
                                        ;; Line ends with backslash → continuation
                                        ((and (> (string-length l) 0)
                                              (char=? (string-ref l (- (string-length l) 1)) #\\))
                                         (display (substring l 0 (- (string-length l) 1)) buf)
                                         (rloop))
                                        (else
                                         (display l buf)
                                         (get-output-string buf)))))))))))
                 ;; Reset timeout
                 (when timeout
                   (input-port-timeout-set! in-port +inf.0))
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
             (lambda ()
               ;; Restore echo if we disabled it
               (when tty?
                 (with-catch void
                   (lambda () (tty-mode-set! in-port #t #t #f #f 0))))
               ;; Close fd port if we opened one
               (when fd (close-input-port in-port))))))
        ((string=? (car args) "-r")
         (loop (cdr args) #t silent? prompt nchars delim timeout fd vars))
        ((string=? (car args) "-s")
         (loop (cdr args) raw? #t prompt nchars delim timeout fd vars))
        ((string=? (car args) "-p")
         (if (pair? (cdr args))
           (loop (cddr args) raw? silent? (cadr args) nchars delim timeout fd vars)
           (loop (cdr args) raw? silent? prompt nchars delim timeout fd vars)))
        ((string=? (car args) "-n")
         (if (pair? (cdr args))
           (loop (cddr args) raw? silent? prompt
                 (or (string->number (cadr args)) 1) delim timeout fd vars)
           (loop (cdr args) raw? silent? prompt nchars delim timeout fd vars)))
        ((string=? (car args) "-d")
         (if (pair? (cdr args))
           (loop (cddr args) raw? silent? prompt nchars (cadr args) timeout fd vars)
           (loop (cdr args) raw? silent? prompt nchars delim timeout fd vars)))
        ((string=? (car args) "-t")
         (if (pair? (cdr args))
           (loop (cddr args) raw? silent? prompt nchars delim
                 (or (string->number (cadr args)) 0) fd vars)
           (loop (cdr args) raw? silent? prompt nchars delim timeout fd vars)))
        ((string=? (car args) "-u")
         (if (pair? (cdr args))
           (loop (cddr args) raw? silent? prompt nchars delim timeout
                 (or (string->number (cadr args)) 0) vars)
           (loop (cdr args) raw? silent? prompt nchars delim timeout fd vars)))
        (else
         (loop (cdr args) raw? silent? prompt nchars delim timeout fd
               (cons (car args) vars)))))))

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

;; local [-n] [-i] [-r] [-x] var[=value] ...
(builtin-register! "local"
  (lambda (args env)
    (let loop ((args args) (nameref? #f) (integer? #f) (readonly? #f) (export? #f))
      (cond
        ((null? args) 0)
        ;; Parse flag arguments
        ((and (> (string-length (car args)) 1)
              (char=? (string-ref (car args) 0) #\-)
              (char-alphabetic? (string-ref (car args) 1)))
         (let floop ((i 1) (nr? nameref?) (int? integer?) (ro? readonly?) (ex? export?))
           (if (>= i (string-length (car args)))
             (loop (cdr args) nr? int? ro? ex?)
             (let ((ch (string-ref (car args) i)))
               (case ch
                 ((#\n) (floop (+ i 1) #t int? ro? ex?))
                 ((#\i) (floop (+ i 1) nr? #t ro? ex?))
                 ((#\r) (floop (+ i 1) nr? int? #t ex?))
                 ((#\x) (floop (+ i 1) nr? int? ro? #t))
                 (else (floop (+ i 1) nr? int? ro? ex?)))))))
        (else
         ;; Process name or name=value arguments
         (for-each
          (lambda (arg)
            (let ((eq-pos (string-find-char* arg #\=)))
              (if eq-pos
                (let ((name (substring arg 0 eq-pos))
                      (value (substring arg (+ eq-pos 1) (string-length arg))))
                  (hash-put! (shell-environment-vars env) name
                             (make-shell-var value export? readonly? #t
                                            integer? #f #f nameref? #f #f)))
                (hash-put! (shell-environment-vars env) arg
                           (make-shell-var "" export? readonly? #t
                                          integer? #f #f nameref? #f #f)))))
          args)
         0)))))

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
                                                        #f #f #f #f #f #f #f #f #f)))
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
               (set! (shell-var-nameref? var) #t))
             (when (hash-get flags 'array)
               (unless (or (shell-var-array? var) (shell-var-assoc? var))
                 ;; Convert scalar to indexed array
                 (let ((old-val (shell-var-value var)))
                   (let ((tbl (make-hash-table)))
                     (when (and (string? old-val) (> (string-length old-val) 0))
                       (hash-put! tbl 0 old-val))
                     (set! (shell-var-value var) tbl)
                     (set! (shell-var-array? var) #t)))))
             (when (hash-get flags 'assoc)
               (unless (shell-var-assoc? var)
                 (set! (shell-var-value var) (make-hash-table))
                 (set! (shell-var-assoc? var) #t)
                 (set! (shell-var-array? var) #f)))))))
     (reverse names))
    0))

;; Helper: print a variable declaration
(def (display-declare-var name var)
  (let ((flags (string-append
                (if (shell-var-assoc? var) "A" "")
                (if (shell-var-array? var) "a" "")
                (if (shell-var-exported? var) "x" "")
                (if (shell-var-readonly? var) "r" "")
                (if (shell-var-integer? var) "i" "")
                (if (shell-var-uppercase? var) "u" "")
                (if (shell-var-lowercase? var) "l" "")
                (if (shell-var-nameref? var) "n" ""))))
    (let ((flag-str (if (string=? flags "") "--" (string-append "-" flags))))
      (cond
        ((shell-var-array? var)
         ;; Indexed array: declare -a arr=([0]="val0" [1]="val1" ...)
         (let ((tbl (shell-var-value var)))
           (display (format "declare ~a ~a=(" flag-str name))
           (let ((keys (sort! (hash-keys tbl) <)))
             (for-each
              (lambda (k)
                (display (format "[~a]=\"~a\" " k (hash-get tbl k))))
              keys))
           (displayln ")")))
        ((shell-var-assoc? var)
         ;; Assoc array: declare -A map=([key1]="val1" [key2]="val2" ...)
         (let ((tbl (shell-var-value var)))
           (display (format "declare ~a ~a=(" flag-str name))
           (hash-for-each
            (lambda (k v)
              (display (format "[~a]=\"~a\" " k v)))
            tbl)
           (displayln ")")))
        (else
         (displayln (format "declare ~a ~a=\"~a\"" flag-str name
                           (shell-var-scalar-value var))))))))

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

;; mapfile [-d delim] [-n count] [-O origin] [-s count] [-t] [array]
;; readarray is an alias for mapfile
(def (builtin-mapfile args env)
  (let loop ((args args) (delim "\n") (max-count 0) (origin 0) (skip 0) (trim? #f) (arr-name #f))
    (cond
      ((null? args)
       (let* ((name (or arr-name "MAPFILE"))
              (delim-ch (if (> (string-length delim) 0) (string-ref delim 0) #\newline))
              (lines []))
         ;; Read all lines from stdin
         (let rloop ((lines []) (count 0))
           (let ((line (if (char=? delim-ch #\newline)
                         (read-line)
                         ;; Custom delimiter: read until delim-ch
                         (let ((buf (open-output-string)))
                           (let cloop ()
                             (let ((ch (read-char)))
                               (cond
                                 ((eof-object? ch)
                                  (let ((s (get-output-string buf)))
                                    (if (string=? s "") ch s)))
                                 ((char=? ch delim-ch)
                                  (if trim?
                                    (get-output-string buf)
                                    (begin (display ch buf)
                                           (get-output-string buf))))
                                 (else
                                  (display ch buf)
                                  (cloop)))))))))
             (cond
               ((eof-object? line)
                ;; Done — assign to array
                (let ((kept (if (> skip 0) (list-tail* lines skip) lines)))
                  (let aloop ((lst kept) (idx origin))
                    (when (pair? lst)
                      (env-array-set! env name idx (car lst))
                      (aloop (cdr lst) (+ idx 1)))))
                0)
               ((and (> max-count 0) (>= (- count skip) max-count))
                ;; Reached max count — assign what we have
                (let ((kept (if (> skip 0) (list-tail* lines skip) lines)))
                  (let aloop ((lst kept) (idx origin))
                    (when (pair? lst)
                      (env-array-set! env name idx (car lst))
                      (aloop (cdr lst) (+ idx 1)))))
                0)
               (else
                (let ((line (if (and trim? (char=? delim-ch #\newline))
                              line  ;; read-line already strips newline
                              (if (and (not trim?) (char=? delim-ch #\newline))
                                (string-append line "\n")
                                line))))
                  (rloop (append lines [line]) (+ count 1)))))))))
      ((string=? (car args) "-d")
       (if (pair? (cdr args))
         (loop (cddr args) (cadr args) max-count origin skip trim? arr-name)
         (loop (cdr args) delim max-count origin skip trim? arr-name)))
      ((string=? (car args) "-n")
       (if (pair? (cdr args))
         (loop (cddr args) delim (or (string->number (cadr args)) 0) origin skip trim? arr-name)
         (loop (cdr args) delim max-count origin skip trim? arr-name)))
      ((string=? (car args) "-O")
       (if (pair? (cdr args))
         (loop (cddr args) delim max-count (or (string->number (cadr args)) 0) skip trim? arr-name)
         (loop (cdr args) delim max-count origin skip trim? arr-name)))
      ((string=? (car args) "-s")
       (if (pair? (cdr args))
         (loop (cddr args) delim max-count origin (or (string->number (cadr args)) 0) trim? arr-name)
         (loop (cdr args) delim max-count origin skip trim? arr-name)))
      ((string=? (car args) "-t")
       (loop (cdr args) delim max-count origin skip #t arr-name))
      (else
       (loop (cdr args) delim max-count origin skip trim? (car args))))))

;; Safe list-tail that returns [] if list is too short
(def (list-tail* lst n)
  (if (or (<= n 0) (null? lst))
    lst
    (list-tail* (cdr lst) (- n 1))))

(builtin-register! "mapfile" builtin-mapfile)
(builtin-register! "readarray" builtin-mapfile)

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

;;; --- Full printf implementation ---

;; Format a printf string with arguments
;; Supports argument recycling: if more args than format specifiers, repeat format
(def (shell-printf fmt args)
  (let ((buf (open-output-string)))
    (if (null? args)
      ;; No args: just process format escapes once
      (begin (printf-format-once fmt [] buf)
             (get-output-string buf))
      ;; With args: loop until all consumed (argument recycling)
      (let loop ((remaining args))
        (let ((leftover (printf-format-once fmt remaining buf)))
          (if (or (null? leftover)
                  (equal? leftover remaining)) ;; no args consumed → stop
            (get-output-string buf)
            (loop leftover)))))))

;; Process format string once, consuming args as needed
;; Returns remaining args after one pass through format
(def (printf-format-once fmt args buf)
  (let ((flen (string-length fmt)))
    (let loop ((i 0) (args args))
      (cond
        ((>= i flen) args)
        ;; Format specifier
        ((and (char=? (string-ref fmt i) #\%) (< (+ i 1) flen))
         (let-values (((end consumed-arg) (printf-format-spec fmt (+ i 1) args buf)))
           (loop end consumed-arg)))
        ;; Backslash escape
        ((and (char=? (string-ref fmt i) #\\) (< (+ i 1) flen))
         (let ((end (printf-escape fmt (+ i 1) buf)))
           (loop end args)))
        ;; Regular character
        (else
         (display (string-ref fmt i) buf)
         (loop (+ i 1) args))))))

;; Process a printf format specifier at position i (after the %)
;; Returns (values next-position remaining-args)
(def (printf-format-spec fmt i args buf)
  (let ((flen (string-length fmt)))
    ;; Parse flags: -, +, space, 0, #
    (let flag-loop ((i i) (left-align? #f) (plus-sign? #f) (space-sign? #f)
                    (zero-pad? #f) (alt-form? #f))
      (if (>= i flen)
        (values i args)
        (let ((ch (string-ref fmt i)))
          (cond
            ((char=? ch #\-) (flag-loop (+ i 1) #t plus-sign? space-sign? zero-pad? alt-form?))
            ((char=? ch #\+) (flag-loop (+ i 1) left-align? #t space-sign? zero-pad? alt-form?))
            ((char=? ch #\space) (flag-loop (+ i 1) left-align? plus-sign? #t zero-pad? alt-form?))
            ((char=? ch #\0) (flag-loop (+ i 1) left-align? plus-sign? space-sign? #t alt-form?))
            ((char=? ch #\#) (flag-loop (+ i 1) left-align? plus-sign? space-sign? zero-pad? #t))
            (else
             ;; Parse width (may be *)
             (let-values (((width i args) (parse-printf-number fmt i args)))
               ;; Parse precision
               (let-values (((prec i args)
                             (if (and (< i flen) (char=? (string-ref fmt i) #\.))
                               (parse-printf-number fmt (+ i 1) args)
                               (values #f i args))))
                 ;; Conversion specifier
                 (if (>= i flen)
                   (values i args)
                   (let ((spec (string-ref fmt i))
                         (arg (if (pair? args) (car args) ""))
                         (rest (if (pair? args) (cdr args) [])))
                     (case spec
                       ;; %% - literal percent
                       ((#\%) (display "%" buf) (values (+ i 1) args))
                       ;; %s - string
                       ((#\s)
                        (let* ((s (if (string? arg) arg (if (pair? args) arg "")))
                               (s (if prec (substring s 0 (min prec (string-length s))) s)))
                          (display (pad-string s (or width 0) left-align? #\space) buf)
                          (values (+ i 1) rest)))
                       ;; %d %i - decimal integer
                       ((#\d #\i)
                        (let* ((n (string->integer-safe arg))
                               (s (number->string n))
                               (s (if (and plus-sign? (>= n 0)) (string-append "+" s) s))
                               (s (if (and space-sign? (>= n 0) (not plus-sign?))
                                    (string-append " " s) s))
                               (pad-ch (if (and zero-pad? (not left-align?)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %o - octal
                       ((#\o)
                        (let* ((n (string->integer-safe arg))
                               (s (number->string (abs n) 8))
                               (s (if (< n 0) (string-append "-" s) s))
                               (s (if (and alt-form? (not (string=? s "0")))
                                    (string-append "0" s) s))
                               (pad-ch (if (and zero-pad? (not left-align?)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %x %X - hexadecimal
                       ((#\x #\X)
                        (let* ((n (string->integer-safe arg))
                               (raw (number->string (abs n) 16))
                               (raw (if (char=? spec #\X) (string-upcase raw) raw))
                               (s (if (< n 0) (string-append "-" raw) raw))
                               (s (if (and alt-form? (not (= n 0)))
                                    (string-append (if (char=? spec #\X) "0X" "0x") s) s))
                               (pad-ch (if (and zero-pad? (not left-align?)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %c - character (first char of arg)
                       ((#\c)
                        (when (and (string? arg) (> (string-length arg) 0))
                          (display (string-ref arg 0) buf))
                        (values (+ i 1) rest))
                       ;; %b - interpret backslash escapes in argument
                       ((#\b)
                        (printf-interpret-escapes (if (string? arg) arg "") buf)
                        (values (+ i 1) rest))
                       ;; %q - shell-quoted
                       ((#\q)
                        (display (shell-quote-string (if (string? arg) arg "")) buf)
                        (values (+ i 1) rest))
                       ;; %f %e %g - floating point
                       ((#\f #\e #\g #\E #\G)
                        (let* ((n (string->number-safe arg))
                               (p (or prec 6))
                               (s (format-float n spec p)))
                          (display (pad-string s (or width 0) left-align?
                                              (if (and zero-pad? (not left-align?)) #\0 #\space)) buf)
                          (values (+ i 1) rest)))
                       ;; Unknown specifier
                       (else
                        (display "%" buf)
                        (display (string spec) buf)
                        (values (+ i 1) args))))))))))))))

;; Parse a number (or * for arg-supplied width/precision) in format string
(def (parse-printf-number fmt i args)
  (let ((flen (string-length fmt)))
    (if (>= i flen)
      (values #f i args)
      (if (char=? (string-ref fmt i) #\*)
        ;; Width/precision from argument
        (let ((n (if (pair? args) (string->integer-safe (car args)) 0))
              (rest (if (pair? args) (cdr args) [])))
          (values (abs n) (+ i 1) rest))
        ;; Parse digits
        (let digit-loop ((j i) (n 0) (found? #f))
          (if (and (< j flen) (char-numeric? (string-ref fmt j)))
            (digit-loop (+ j 1)
                       (+ (* n 10) (- (char->integer (string-ref fmt j)) 48))
                       #t)
            (values (if found? n #f) j args)))))))

;; Process a printf backslash escape
(def (printf-escape fmt i buf)
  (let ((flen (string-length fmt)))
    (if (>= i flen)
      (begin (display "\\" buf) i)
      (let ((ch (string-ref fmt i)))
        (case ch
          ((#\n) (display "\n" buf) (+ i 1))
          ((#\t) (display "\t" buf) (+ i 1))
          ((#\r) (display "\r" buf) (+ i 1))
          ((#\a) (display "\a" buf) (+ i 1))
          ((#\b) (display "\b" buf) (+ i 1))
          ((#\f) (display "\x0c;" buf) (+ i 1))
          ((#\v) (display "\x0b;" buf) (+ i 1))
          ((#\\) (display "\\" buf) (+ i 1))
          ((#\') (display "'" buf) (+ i 1))
          ((#\") (display "\"" buf) (+ i 1))
          ;; \0nnn - octal
          ((#\0)
           (let octal-loop ((j (+ i 1)) (n 0) (count 0))
             (if (and (< j flen) (< count 3)
                      (char>=? (string-ref fmt j) #\0)
                      (char<=? (string-ref fmt j) #\7))
               (octal-loop (+ j 1)
                          (+ (* n 8) (- (char->integer (string-ref fmt j)) 48))
                          (+ count 1))
               (begin (display (string (integer->char n)) buf) j))))
          ;; \xHH - hex
          ((#\x)
           (let hex-loop ((j (+ i 1)) (n 0) (count 0))
             (if (and (< j flen) (< count 2))
               (let ((hch (string-ref fmt j)))
                 (cond
                   ((and (char>=? hch #\0) (char<=? hch #\9))
                    (hex-loop (+ j 1) (+ (* n 16) (- (char->integer hch) 48)) (+ count 1)))
                   ((and (char>=? hch #\a) (char<=? hch #\f))
                    (hex-loop (+ j 1) (+ (* n 16) (- (char->integer hch) 87)) (+ count 1)))
                   ((and (char>=? hch #\A) (char<=? hch #\F))
                    (hex-loop (+ j 1) (+ (* n 16) (- (char->integer hch) 55)) (+ count 1)))
                   (else (display (string (integer->char n)) buf) j)))
               (begin (display (string (integer->char n)) buf) j))))
          (else (display "\\" buf) (display (string ch) buf) (+ i 1)))))))

;; Interpret backslash escapes in a string (for %b)
(def (printf-interpret-escapes str buf)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (when (< i len)
        (let ((ch (string-ref str i)))
          (if (char=? ch #\\)
            (if (< (+ i 1) len)
              (let ((next (printf-escape str (+ i 1) buf)))
                (loop next))
              (begin (display "\\" buf) (loop (+ i 1))))
            (begin (display ch buf) (loop (+ i 1)))))))))

;; Pad a string to a minimum width
(def (pad-string s width left-align? pad-ch)
  (let ((len (string-length s)))
    (if (<= width len)
      s
      (let ((padding (make-string (- width len) pad-ch)))
        (if left-align?
          (string-append s padding)
          ;; For zero-padding with sign, put sign before zeros
          (if (and (char=? pad-ch #\0)
                   (> (string-length s) 0)
                   (or (char=? (string-ref s 0) #\-)
                       (char=? (string-ref s 0) #\+)))
            (string-append (substring s 0 1)
                          padding
                          (substring s 1 (string-length s)))
            (string-append padding s)))))))

;; Convert string to integer safely (handles 0x, 0, 'c' char literal)
(def (string->integer-safe s)
  (cond
    ((not (string? s)) 0)
    ((string=? s "") 0)
    ;; Character literal: 'c or "c → ASCII value
    ((and (>= (string-length s) 2)
          (or (char=? (string-ref s 0) #\')
              (char=? (string-ref s 0) #\")))
     (char->integer (string-ref s 1)))
    ;; Hex: 0x...
    ((and (>= (string-length s) 2)
          (char=? (string-ref s 0) #\0)
          (or (char=? (string-ref s 1) #\x)
              (char=? (string-ref s 1) #\X)))
     (or (string->number (substring s 2 (string-length s)) 16) 0))
    ;; Octal: 0...
    ((and (> (string-length s) 1) (char=? (string-ref s 0) #\0))
     (or (string->number s 8) (or (string->number s) 0)))
    (else (or (string->number s) 0))))

;; Convert string to float safely
(def (string->number-safe s)
  (if (string? s) (or (string->number s) 0.0) 0.0))

;; Format a floating-point number
(def (format-float n spec precision)
  (let ((n (inexact->exact (if (number? n) n 0))))
    (let ((n (exact->inexact n)))
      (case spec
        ((#\f #\F) (format-fixed n precision))
        ((#\e #\E) (format-scientific n precision (char=? spec #\E)))
        ((#\g #\G) ;; Use %e if exponent < -4 or >= precision, else %f
         (let ((e (if (= n 0.0) 0 (floor (/ (log (abs n)) (log 10))))))
           (if (or (< e -4) (>= e (or precision 6)))
             (format-scientific n precision (char=? spec #\G))
             (format-fixed n precision))))
        (else (number->string n))))))

;; Simple fixed-point formatting
(def (format-fixed n precision)
  (let* ((s (number->string (exact->inexact n)))
         (dot-pos (string-find-char* s #\.)))
    (if dot-pos
      (let* ((int-part (substring s 0 dot-pos))
             (frac-part (substring s (+ dot-pos 1) (string-length s)))
             (frac-len (string-length frac-part)))
        (cond
          ((= precision 0) int-part)
          ((<= frac-len precision)
           (string-append int-part "." frac-part
                         (make-string (- precision frac-len) #\0)))
          (else
           (string-append int-part "." (substring frac-part 0 precision)))))
      ;; No decimal point
      (if (= precision 0)
        s
        (string-append s "." (make-string precision #\0))))))

;; Simple scientific notation formatting
(def (format-scientific n precision upper?)
  (if (= n 0.0)
    (string-append "0." (make-string (or precision 6) #\0)
                   (if upper? "E+00" "e+00"))
    (let* ((sign (if (< n 0) -1 1))
           (abs-n (abs n))
           (exp (floor (/ (log abs-n) (log 10))))
           (mantissa (* sign (/ abs-n (expt 10.0 exp))))
           (mstr (format-fixed (exact->inexact mantissa) (or precision 6)))
           (echar (if upper? "E" "e"))
           (esign (if (>= exp 0) "+" "-"))
           (estr (number->string (inexact->exact (abs exp)))))
      (string-append mstr echar esign
                    (if (< (string-length estr) 2)
                      (string-append "0" estr) estr)))))

;; Shell-quote a string for %q
(def (shell-quote-string s)
  (if (string=? s "")
    "''"
    (let ((buf (open-output-string)))
      (display "'" buf)
      (let loop ((i 0))
        (when (< i (string-length s))
          (let ((ch (string-ref s i)))
            (if (char=? ch #\')
              (begin (display "'\\''" buf) (loop (+ i 1)))
              (begin (display ch buf) (loop (+ i 1)))))))
      (display "'" buf)
      (get-output-string buf))))

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
         (set! result (cons (cons name (or (shell-var-scalar-value var) "")) result))))
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
