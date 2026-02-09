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
    ;; Parse echo flags: -n, -e, -E, or combinations like -en, -neE
    (let loop ((args args) (newline? #t) (escape? #f))
      (if (and (pair? args)
               (> (string-length (car args)) 1)
               (char=? (string-ref (car args) 0) #\-)
               ;; All chars after - must be n, e, or E
               (let ((s (car args)))
                 (let valid? ((j 1))
                   (if (>= j (string-length s)) #t
                     (let ((c (string-ref s j)))
                       (and (or (char=? c #\n) (char=? c #\e) (char=? c #\E))
                            (valid? (+ j 1))))))))
        ;; Parse flags from this argument
        (let* ((s (car args))
               (has-n? (let find ((j 1)) (if (>= j (string-length s)) #f
                                           (or (char=? (string-ref s j) #\n) (find (+ j 1))))))
               (has-e? (let find ((j 1)) (if (>= j (string-length s)) #f
                                           (or (char=? (string-ref s j) #\e) (find (+ j 1))))))
               (has-E? (let find ((j 1)) (if (>= j (string-length s)) #f
                                           (or (char=? (string-ref s j) #\E) (find (+ j 1)))))))
          (loop (cdr args)
                (if has-n? #f newline?)
                (cond (has-E? #f) (has-e? #t) (else escape?))))
        ;; Print arguments (use call/cc for \c early termination)
        (call/cc
         (lambda (stop)
           (let arg-loop ((rest args) (first? #t))
             (when (pair? rest)
               (unless first? (display " "))
               (if escape?
                 (let ((result (echo-expand-escapes (car rest))))
                   (display (car result))
                   (when (cdr result)  ;; \c encountered — stop all output
                     (force-output)
                     (stop 0)))
                 (display (car rest)))
               (arg-loop (cdr rest) #f)))
           (when newline? (newline))
           (force-output)
           0))))))

;; printf [-v var] format [args...]
(builtin-register! "printf"
  (lambda (args env)
    (if (null? args)
      (begin (fprintf (current-error-port) "printf: usage: printf [-v var] format [arguments]~n") 2)
      ;; Parse -v option and --
      (let loop ((rest args) (var-name #f))
        (cond
          ((null? rest)
           (begin (fprintf (current-error-port) "printf: usage: printf [-v var] format [arguments]~n") 2))
          ((string=? (car rest) "--")
           (loop (cdr rest) var-name))
          ((and (not var-name)
                (>= (length rest) 2)
                (string=? (car rest) "-v"))
           (loop (cddr rest) (cadr rest)))
          (else
           (let ((fmt (car rest))
                 (fmt-args (cdr rest)))
             ;; Format with argument recycling: repeat format until all args consumed
             (parameterize ((*printf-conversion-error* #f))
               (let ((output (shell-printf fmt fmt-args)))
                 (if var-name
                   (begin (env-set! env var-name output)
                          (if (*printf-conversion-error*) 1 0))
                   (begin (display output) (force-output)
                          (if (*printf-conversion-error*) 1 0))))))))))))

;; Helper: strip trailing slash (except for root /)
(def (strip-trailing-slash path)
  (let ((len (string-length path)))
    (if (and (> len 1) (char=? (string-ref path (- len 1)) #\/))
      (substring path 0 (- len 1))
      path)))

;; Helper: check if two paths refer to the same directory (by device+inode)
(def (same-directory? path-a path-b)
  (with-catch
   (lambda (e) #f)
   (lambda ()
     (let ((a (file-info path-a))
           (b (file-info path-b)))
       (and (= (file-info-device a) (file-info-device b))
            (= (file-info-inode a) (file-info-inode b)))))))

;; Helper: resolve logical path for cd
;; Given a base (logical pwd) and a relative target, compute the new logical path
;; Handles ".." by removing last component textually (not resolving symlinks)
(def (resolve-logical-path base target)
  (let ((path (if (and (> (string-length target) 0)
                       (char=? (string-ref target 0) #\/))
                target  ;; absolute path
                (string-append (strip-trailing-slash base) "/" target))))
    ;; Normalize: split on /, resolve . and .., rejoin
    (let* ((parts (string-split path #\/))
           (resolved
            (let loop ((ps parts) (acc []))
              (cond
                ((null? ps) (reverse acc))
                ((string=? (car ps) "") (loop (cdr ps) (if (null? acc) [""] acc)))
                ((string=? (car ps) ".") (loop (cdr ps) acc))
                ((string=? (car ps) "..")
                 (loop (cdr ps)
                       (if (and (pair? acc) (not (string=? (car acc) "")))
                         (cdr acc)
                         acc)))
                (else (loop (cdr ps) (cons (car ps) acc)))))))
      (let ((result (string-join resolved "/")))
        (if (string=? result "") "/" result)))))

;; Helper: search CDPATH for a relative directory
(def (search-cdpath target env)
  (let ((cdpath (env-get env "CDPATH")))
    (if (or (not cdpath) (string=? cdpath "")
            (and (> (string-length target) 0)
                 (char=? (string-ref target 0) #\/)))
      #f  ;; no CDPATH or absolute path
      (let loop ((dirs (string-split cdpath #\:)))
        (if (null? dirs) #f
          (let* ((base (if (string=? (car dirs) "") "." (car dirs)))
                 (full (string-append (strip-trailing-slash base) "/" target)))
            (if (file-exists? full)
              full
              (loop (cdr dirs)))))))))

;; cd [-L|-P] [--] [dir]
(builtin-register! "cd"
  (lambda (args env)
    ;; Parse options
    (let loop ((rest args) (physical? #f))
      (cond
        ;; No more options
        ((or (null? rest)
             (string=? (car rest) "--")
             (not (and (> (string-length (car rest)) 0)
                       (char=? (string-ref (car rest) 0) #\-)))
             (string=? (car rest) "-"))
         (let* ((remaining (if (and (pair? rest) (string=? (car rest) "--"))
                             (cdr rest)
                             rest))
                (dir-arg (cond
                           ((null? remaining)
                            (let ((home (env-get env "HOME")))
                              (if (or (not home) (string=? home ""))
                                (begin
                                  (fprintf (current-error-port) "cd: HOME not set~n")
                                  #f)
                                home)))
                           ((string=? (car remaining) "-")
                            (let ((oldpwd (env-get env "OLDPWD")))
                              (if (not oldpwd)
                                (begin
                                  (fprintf (current-error-port) "cd: OLDPWD not set~n")
                                  #f)
                                oldpwd)))
                           (else (car remaining)))))
           (if (not dir-arg)
             1
             (let* ((expanded (expand-word-nosplit dir-arg env))
                    ;; Try CDPATH for relative paths
                    (actual-dir (or (and (not (and (> (string-length expanded) 0)
                                                   (char=? (string-ref expanded 0) #\/)))
                                        (search-cdpath expanded env))
                                   expanded))
                    (print-dir? (or (and (pair? remaining)
                                        (string=? (car remaining) "-"))
                                   (and (not (string=? actual-dir expanded))
                                        ;; CDPATH found a different dir
                                        #t))))
               (with-catch
                (lambda (e)
                  (fprintf (current-error-port) "cd: ~a: No such file or directory~n" expanded)
                  1)
                (lambda ()
                  (let ((old-pwd (or (*internal-pwd*)
                                     (strip-trailing-slash
                                      (or (env-get env "PWD")
                                          (strip-trailing-slash (current-directory)))))))
                    (let ((new-pwd (if physical?
                                     (begin
                                       (current-directory actual-dir)
                                       (strip-trailing-slash (current-directory)))
                                     ;; Logical mode: compute logical path, then chdir to it
                                     ;; First verify path is reachable physically
                                     (let ((logical (strip-trailing-slash
                                                     (resolve-logical-path old-pwd actual-dir))))
                                       ;; Validate path: try chdir to actual-dir first
                                       ;; (catches nonexistent components like cd BAD/..)
                                       (current-directory actual-dir)
                                       ;; Now chdir to the logical path
                                       (current-directory logical)
                                       logical))))
                      (env-set! env "OLDPWD" old-pwd)
                      (env-export! env "OLDPWD")
                      (env-set! env "PWD" new-pwd)
                      (env-export! env "PWD")
                      (*internal-pwd* new-pwd)
                      (when print-dir?
                        (displayln new-pwd))
                      0))))))))
        ;; -L flag: logical (default)
        ((string=? (car rest) "-L")
         (loop (cdr rest) #f))
        ;; -P flag: physical
        ((string=? (car rest) "-P")
         (loop (cdr rest) #t))
        ;; Unknown option - treat as dir
        (else
         (loop (cons "--" rest) physical?))))))

;; pwd [-L|-P]
(builtin-register! "pwd"
  (lambda (args env)
    (let ((physical? (and (pair? args) (string=? (car args) "-P"))))
      (displayln (strip-trailing-slash
                  (if physical?
                    (current-directory)
                    ;; Logical mode: use internal tracked PWD (not $PWD which user can override)
                    (or (*internal-pwd*)
                        (strip-trailing-slash (current-directory)))))))
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
          ;; Run EXIT trap if set — clear first to prevent re-entrancy
          (let ((exit-trap (trap-get "EXIT")))
            (when (and exit-trap (string? exit-trap))
              (trap-set! "EXIT" 'default)
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
    (cond
      ;; Too many arguments
      ((> (length args) 1)
       (fprintf (current-error-port) "gsh: break: too many arguments~n")
       1)
      ((pair? args)
       (let ((n (string->number (car args))))
         (cond
           ((not n)
            ;; Non-numeric arg: print error, set error status, then break (bash compat)
            (fprintf (current-error-port) "gsh: break: ~a: numeric argument required~n" (car args))
            (env-set-last-status! env 1)
            (shell-break! 1))
           ((<= n 0)
            (fprintf (current-error-port) "gsh: break: ~a: loop count out of range~n" (car args))
            (env-set-last-status! env 1)
            (shell-break! 1))
           (else (shell-break! n)))))
      (else (shell-break! 1)))))

;; continue [n]
(builtin-register! "continue"
  (lambda (args env)
    (cond
      ;; Too many arguments: fatal error
      ((> (length args) 1)
       (fprintf (current-error-port) "gsh: continue: too many arguments~n")
       2)
      ((pair? args)
       (let ((n (string->number (car args))))
         (cond
           ((not n)
            ;; Non-numeric arg: print error, set error status, then continue (bash compat)
            (fprintf (current-error-port) "gsh: continue: ~a: numeric argument required~n" (car args))
            (env-set-last-status! env 1)
            (shell-continue! 1))
           ((<= n 0)
            (fprintf (current-error-port) "gsh: continue: ~a: loop count out of range~n" (car args))
            (env-set-last-status! env 1)
            (shell-continue! 1))
           (else (shell-continue! n)))))
      (else (shell-continue! 1)))))

;; set [options] [-- args...]
(builtin-register! "set"
  (lambda (args env)
    (if (null? args)
      ;; No args: display all variables in re-evaluable format
      (begin
        (for-each
         (lambda (pair)
           (displayln (format "~a=~a" (car pair) (shell-quote-value (cdr pair)))))
         (env-all-variables env))
        0)
      (let loop ((args args))
        (cond
          ((null? args) 0)
          ((string=? (car args) "--")
           ;; Set positional parameters
           (env-set-positional! env (cdr args))
           0)
          ;; "set -" — equivalent to "set +xv" (turn off xtrace and verbose)
          ((string=? (car args) "-")
           (env-option-set! env "xtrace" #f)
           (env-option-set! env "verbose" #f)
           ;; Remaining args become positional parameters
           (when (pair? (cdr args))
             (env-set-positional! env (cdr args)))
           0)
          ;; -o / +o: check if next arg looks like an option name
          ((or (string=? (car args) "-o")
               (string=? (car args) "+o"))
           (let ((enable? (char=? (string-ref (car args) 0) #\-)))
             (if (and (pair? (cdr args))
                      ;; Next arg is an option name if it doesn't start with - or +
                      (not (string-prefix? "-" (cadr args)))
                      (not (string-prefix? "+" (cadr args))))
               ;; -o option-name / +o option-name
               (begin
                 (env-option-set! env (cadr args) enable?)
                 (loop (cddr args)))
               ;; -o / +o without argument: print option list
               (begin
                 (for-each
                  (lambda (opt)
                    (let ((name (car opt)) (on? (cdr opt)))
                      (if enable?
                        (displayln (format "~a\t~a" name (if on? "on" "off")))
                        (displayln (format "set ~ao ~a" (if on? "-" "+") name)))))
                  (env-all-options env))
                 0))))
          ;; Bare "+" is ignored (like a no-op flag)
          ((string=? (car args) "+")
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

(def (shell-quote-value val)
  (string-append "'" (string-replace-all val "'" "'\\''") "'"))

;; shift [n]
(builtin-register! "shift"
  (lambda (args env)
    (if (pair? args)
      (let ((n (string->number (car args))))
        (cond
          ((not n)
           (fprintf (current-error-port) "gsh: shift: ~a: numeric argument required~n" (car args))
           2)
          ((< n 0)
           (fprintf (current-error-port) "gsh: shift: ~a: shift count out of range~n" (car args))
           1)
          (else
           (let ((pos (env-positional-list env)))
             (if (> n (length pos))
               1
               (begin
                 (env-set-positional! env (list-tail pos n))
                 0))))))
      (let ((pos (env-positional-list env)))
        (if (> 1 (length pos))
          1
          (begin
            (env-set-positional! env (list-tail pos 1))
            0))))))

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
  (lambda (args env)
    (parameterize ((*test-var-fn* (lambda (name) (env-get env name)))
                   (*test-option-fn* (lambda (name) (env-option? env name))))
      (test-eval args))))

(builtin-register! "["
  (lambda (args env)
    (if (and (pair? args)
             (string=? (last-elem* args) "]"))
      (let ((inner (butlast args)))
        (parameterize ((*test-var-fn* (lambda (name) (env-get env name)))
                       (*test-option-fn* (lambda (name) (env-option? env name))))
          (with-catch
           (lambda (e) 2)  ;; Any evaluation error → exit 2
           (lambda () (test-eval inner)))))
      (begin
        (fprintf (current-error-port) "[: missing `]'~n")
        2))))

;; type [-afptP] name...
(builtin-register! "type"
  (lambda (args env)
    ;; Parse flags
    (let parse-flags ((args args) (flags []))
      (if (and (pair? args) (> (string-length (car args)) 0)
               (char=? (string-ref (car args) 0) #\-))
        (parse-flags (cdr args) (cons (car args) flags))
        (let loop ((args args) (status 0))
          (if (null? args)
            status
            (let ((name (car args)))
              (cond
                ;; Shell keywords
                ((shell-keyword? name)
                 (displayln (format "~a is a shell keyword" name))
                 (loop (cdr args) status))
                ;; Aliases
                ((alias-get env name)
                 => (lambda (expansion)
                      (displayln (format "~a is aliased to `~a'" name expansion))
                      (loop (cdr args) status)))
                ;; Shell functions
                ((function-lookup env name)
                 (displayln (format "~a is a function" name))
                 (loop (cdr args) status))
                ;; Builtins — distinguish special vs regular
                ((special-builtin? name)
                 (displayln (format "~a is a special shell builtin" name))
                 (loop (cdr args) status))
                ((builtin? name)
                 (displayln (format "~a is a shell builtin" name))
                 (loop (cdr args) status))
                ;; External command
                ((which name)
                 => (lambda (path)
                      (displayln (format "~a is ~a" name path))
                      (loop (cdr args) status)))
                ;; Not found — output to stdout (not stderr) per POSIX
                (else
                 (displayln (format "~a: not found" name))
                 (loop (cdr args) 1))))))))))

(def (shell-keyword? name)
  (member name '("if" "then" "else" "elif" "fi" "case" "esac" "for" "while"
                  "until" "do" "done" "in" "function" "select" "time"
                  "{" "}" "!" "[[" "]]" "coproc")))

;; POSIX special builtins (must be handled differently from regular builtins)
(def (special-builtin? name)
  (and (member name '("." ":" "break" "continue" "eval" "exec" "exit"
                       "export" "readonly" "return" "set" "shift" "trap"
                       "unset"))
       (builtin? name)))

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

;; read [-r] [-p prompt] [-t timeout] [-d delim] [-n count] [-N count] [-s] [-a arr] [-u fd] var...
(builtin-register! "read"
  (lambda (args env)
    (let loop ((args args) (raw? #f) (silent? #f) (prompt "")
               (nchars #f) (nchars-raw? #f) (delim #f) (timeout #f)
               (fd #f) (array-name #f) (vars []))
      (cond
        ((null? args)
         ;; Validate timeout
         (when (and timeout (< timeout 0))
           (fprintf (current-error-port) "read: ~a: invalid timeout specification~n" timeout)
           (set! timeout #f))
         ;; Show prompt on stderr (only if input is a terminal)
         (when (and (> (string-length prompt) 0)
                    (= (ffi-isatty (or fd 0)) 1))
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
               (let* ((got-eof? #f)
                      (line (cond
                              ;; -N nchars: read exactly N bytes, ignore delimiters
                              ((and nchars nchars-raw?)
                               (let ((buf (open-output-string)))
                                 (let rloop ((count 0))
                                   (if (>= count nchars)
                                     (get-output-string buf)
                                     (let ((ch (read-char in-port)))
                                       (if (eof-object? ch)
                                         (begin (set! got-eof? #t)
                                                (let ((s (get-output-string buf)))
                                                  (if (string=? s "") ch s)))
                                         (begin (display ch buf)
                                                (rloop (+ count 1)))))))))
                              ;; -n nchars: read N chars, respects delimiters
                              ;; In non-raw mode, count PROCESSED chars (after backslash removal)
                              (nchars
                               (let ((delim-ch (if delim
                                                 (if (string=? delim "")
                                                   (integer->char 0)
                                                   (string-ref delim 0))
                                                 #\newline))
                                     (buf (open-output-string)))
                                 (if raw?
                                   ;; Raw mode: just count chars
                                   (let rloop ((count 0))
                                     (if (>= count nchars)
                                       (get-output-string buf)
                                       (let ((ch (read-char in-port)))
                                         (cond
                                           ((eof-object? ch)
                                            (set! got-eof? #t)
                                            (let ((s (get-output-string buf)))
                                              (if (string=? s "") ch s)))
                                           ((char=? ch delim-ch)
                                            (get-output-string buf))
                                           (else
                                            (display ch buf)
                                            (rloop (+ count 1)))))))
                                   ;; Non-raw mode: backslash-char counts as 1 processed char
                                   ;; backslash-newline is line continuation (swallowed, not counted)
                                   (let rloop ((count 0))
                                     (if (>= count nchars)
                                       (get-output-string buf)
                                       (let ((ch (read-char in-port)))
                                         (cond
                                           ((eof-object? ch)
                                            (set! got-eof? #t)
                                            (let ((s (get-output-string buf)))
                                              (if (string=? s "") ch s)))
                                           ((char=? ch delim-ch)
                                            (get-output-string buf))
                                           ((char=? ch #\\)
                                            (let ((next (read-char in-port)))
                                              (cond
                                                ((eof-object? next)
                                                 (set! got-eof? #t)
                                                 (get-output-string buf))
                                                ((char=? next #\newline)
                                                 ;; Line continuation: swallow, don't count
                                                 (rloop count))
                                                (else
                                                 ;; Escaped char: output the char, count as 1
                                                 (display next buf)
                                                 (rloop (+ count 1))))))
                                           (else
                                            (display ch buf)
                                            (rloop (+ count 1))))))))))
                              ;; -d delim: read until delimiter
                              (delim
                               (let ((delim-ch (if (string=? delim "")
                                                 (integer->char 0)
                                                 (string-ref delim 0)))
                                     (buf (open-output-string)))
                                 (if raw?
                                   ;; Raw mode: no backslash processing
                                   (let rloop ()
                                     (let ((ch (read-char in-port)))
                                       (cond
                                         ((eof-object? ch)
                                          (set! got-eof? #t)
                                          (let ((s (get-output-string buf)))
                                            (if (string=? s "") ch s)))
                                         ((char=? ch delim-ch)
                                          (get-output-string buf))
                                         (else
                                          (display ch buf)
                                          (rloop)))))
                                   ;; Non-raw mode: backslash-newline is continuation
                                   (let rloop ()
                                     (let ((ch (read-char in-port)))
                                       (cond
                                         ((eof-object? ch)
                                          (set! got-eof? #t)
                                          (let ((s (get-output-string buf)))
                                            (if (string=? s "") ch s)))
                                         ((char=? ch delim-ch)
                                          (get-output-string buf))
                                         ((char=? ch #\\)
                                          (let ((next (read-char in-port)))
                                            (cond
                                              ((eof-object? next)
                                               (set! got-eof? #t)
                                               (get-output-string buf))
                                              ((char=? next #\newline)
                                               ;; Line continuation
                                               (rloop))
                                              (else
                                               ;; Keep escaped char (remove backslash)
                                               (display next buf)
                                               (rloop)))))
                                         (else
                                          (display ch buf)
                                          (rloop))))))))
                              ;; Default: read line
                              (else
                               (if raw?
                                 ;; Raw mode: read char-by-char to detect no-newline
                                 (let ((buf (open-output-string)))
                                   (let rloop ()
                                     (let ((ch (read-char in-port)))
                                       (cond
                                         ((eof-object? ch)
                                          (set! got-eof? #t)
                                          (let ((s (get-output-string buf)))
                                            (if (string=? s "") ch s)))
                                         ((char=? ch #\newline)
                                          (get-output-string buf))
                                         (else
                                          (display ch buf)
                                          (rloop))))))
                                 ;; Cooked mode: backslash-newline is continuation
                                 ;; Read char-by-char to detect no-newline EOF
                                 (let ((buf (open-output-string)))
                                   (let rloop ()
                                     (let ((ch (read-char in-port)))
                                       (cond
                                         ((eof-object? ch)
                                          (set! got-eof? #t)
                                          (let ((s (get-output-string buf)))
                                            (if (string=? s "") ch s)))
                                         ((char=? ch #\newline)
                                          (get-output-string buf))
                                         ((char=? ch #\\)
                                          (let ((next (read-char in-port)))
                                            (cond
                                              ((eof-object? next)
                                               (set! got-eof? #t)
                                               (get-output-string buf))
                                              ((char=? next #\newline)
                                               ;; Line continuation
                                               (rloop))
                                              (else
                                               ;; Keep escaped char WITH backslash
                                               ;; (backslash removal happens during IFS split)
                                               (display #\\ buf)
                                               (display next buf)
                                               (rloop)))))
                                         (else
                                          (display ch buf)
                                          (rloop)))))))))))
                 ;; Reset timeout
                 (when timeout
                   (input-port-timeout-set! in-port +inf.0))
                 (if (eof-object? line)
                   1
                   (let* ((var-names (cond
                                      (array-name [array-name])
                                      ((null? vars) ["REPLY"])
                                      (else (reverse vars))))
                          (use-reply? (and (null? vars) (not array-name)))
                          (ifs (or (env-get env "IFS") " \t\n")))
                     (cond
                       ;; -N mode: no IFS splitting, assign directly to first var
                       (nchars-raw?
                        (env-set! env (car var-names) line)
                        ;; Clear remaining vars
                        (for-each (lambda (v) (env-set! env v "")) (cdr var-names))
                        (if got-eof? 1 0))
                       ;; No vars specified: store in REPLY without splitting
                       ;; In non-raw mode, strip backslashes for REPLY
                       (use-reply?
                        (let ((val (if raw? line (read-strip-backslashes line))))
                          (env-set! env "REPLY" val)
                          (if got-eof? 1 0)))
                       ;; Array mode: split into all fields with IFS
                       (array-name
                        (let ((fields (if raw?
                                       (read-ifs-split-raw line ifs 0)
                                       (read-ifs-split line ifs 0))))
                          (env-array-set-compound! env array-name fields #f)
                          (if got-eof? 1 0)))
                       ;; Regular variable assignment with IFS splitting
                       (else
                        (let ((fields (if raw?
                                       (read-ifs-split-raw line ifs (length var-names))
                                       (read-ifs-split line ifs (length var-names)))))
                          (let field-loop ((names var-names) (fields fields))
                            (cond
                              ((null? names) (if got-eof? 1 0))
                              ((null? fields)
                               (env-set! env (car names) "")
                               (field-loop (cdr names) []))
                              ((null? (cdr names))
                               ;; Last var gets remainder
                               (env-set! env (car names)
                                         (string-join-sp fields))
                               (if got-eof? 1 0))
                              (else
                               (env-set! env (car names) (car fields))
                               (field-loop (cdr names) (cdr fields))))))))))))
             (lambda ()
               ;; Restore echo if we disabled it
               (when tty?
                 (with-catch void
                   (lambda () (tty-mode-set! in-port #t #t #f #f 0))))
               ;; Close fd port if we opened one
               (when fd (close-input-port in-port))))))
        ;; Parse options — support smooshed flags like -rn5, -rd''
        ((and (> (string-length (car args)) 1)
              (char=? (string-ref (car args) 0) #\-))
         (let* ((arg (car args))
                (rest (cdr args)))
           (let parse-flags ((i 1))
             (if (>= i (string-length arg))
               (loop rest raw? silent? prompt nchars nchars-raw? delim timeout fd array-name vars)
               (let ((ch (string-ref arg i)))
                 (case ch
                   ((#\r) (set! raw? #t) (parse-flags (+ i 1)))
                   ((#\s) (set! silent? #t) (parse-flags (+ i 1)))
                   ((#\e) (parse-flags (+ i 1))) ;; -e ignored (readline, not relevant)
                   ((#\n)
                    ;; -nNUM or -n (next arg is count)
                    (let ((num-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length num-str) 0)
                        (let ((n (string->number num-str)))
                          (cond
                            ((not n)
                             (fprintf (current-error-port) "read: ~a: invalid number~n" num-str)
                             2)
                            ((< n 0)
                             (fprintf (current-error-port) "read: ~a: invalid number~n" num-str)
                             2)
                            (else
                             (set! nchars n)
                             (loop rest raw? silent? prompt nchars nchars-raw?
                                   delim timeout fd array-name vars))))
                        (if (pair? rest)
                          (let ((n (string->number (car rest))))
                            (cond
                              ((not n)
                               (fprintf (current-error-port) "read: ~a: invalid number~n" (car rest))
                               2)
                              ((< n 0)
                               (fprintf (current-error-port) "read: ~a: invalid number~n" (car rest))
                               2)
                              (else
                               (set! nchars n)
                               (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\N)
                    ;; -NNUM or -N (next arg is count) — raw char read
                    (let ((num-str (substring arg (+ i 1) (string-length arg))))
                      (set! nchars-raw? #t)
                      (if (> (string-length num-str) 0)
                        (begin (set! nchars (or (string->number num-str) 1))
                               (loop rest raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))
                        (if (pair? rest)
                          (begin (set! nchars (or (string->number (car rest)) 1))
                                 (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                       delim timeout fd array-name vars))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\p)
                    ;; -p PROMPT (next arg or rest of this arg)
                    (let ((p-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length p-str) 0)
                        (begin (set! prompt p-str)
                               (loop rest raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))
                        (if (pair? rest)
                          (begin (set! prompt (car rest))
                                 (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                       delim timeout fd array-name vars))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\d)
                    ;; -d DELIM (next arg or rest of this arg)
                    (let ((d-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length d-str) 0)
                        (begin (set! delim d-str)
                               (loop rest raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))
                        (if (pair? rest)
                          (begin (set! delim (car rest))
                                 (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                       delim timeout fd array-name vars))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\t)
                    ;; -t TIMEOUT
                    (let ((t-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length t-str) 0)
                        (let ((t (string->number t-str)))
                          (if (and t (>= t 0))
                            (begin (set! timeout t)
                                   (loop rest raw? silent? prompt nchars nchars-raw?
                                         delim timeout fd array-name vars))
                            (begin
                              (fprintf (current-error-port) "read: ~a: invalid timeout specification~n" t-str)
                              2)))
                        (if (pair? rest)
                          (let ((t (string->number (car rest))))
                            (if (and t (>= t 0))
                              (begin (set! timeout t)
                                     (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                           delim timeout fd array-name vars))
                              (begin
                                (fprintf (current-error-port) "read: ~a: invalid timeout specification~n" (car rest))
                                2)))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\u)
                    ;; -u FD
                    (let ((u-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length u-str) 0)
                        (let ((n (string->number u-str)))
                          (if (and n (>= n 0))
                            (begin (set! fd n)
                                   (loop rest raw? silent? prompt nchars nchars-raw?
                                         delim timeout fd array-name vars))
                            (begin
                              (fprintf (current-error-port) "read: ~a: invalid file descriptor specification~n" u-str)
                              2)))
                        (if (pair? rest)
                          (let ((n (string->number (car rest))))
                            (if (and n (>= n 0))
                              (begin (set! fd n)
                                     (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                           delim timeout fd array-name vars))
                              (begin
                                (fprintf (current-error-port) "read: ~a: invalid file descriptor specification~n" (car rest))
                                2)))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   ((#\a)
                    ;; -a ARRAY
                    (let ((a-str (substring arg (+ i 1) (string-length arg))))
                      (if (> (string-length a-str) 0)
                        (begin (set! array-name a-str)
                               (loop rest raw? silent? prompt nchars nchars-raw?
                                     delim timeout fd array-name vars))
                        (if (pair? rest)
                          (begin (set! array-name (car rest))
                                 (loop (cdr rest) raw? silent? prompt nchars nchars-raw?
                                       delim timeout fd array-name vars))
                          (loop rest raw? silent? prompt nchars nchars-raw?
                                delim timeout fd array-name vars)))))
                   (else
                    ;; Unknown flag — error
                    (fprintf (current-error-port) "read: -~a: invalid option~n" ch)
                    2)))))))
        (else
         (loop (cdr args) raw? silent? prompt nchars nchars-raw?
               delim timeout fd array-name (cons (car args) vars)))))))

;; Strip backslashes in non-raw read mode
;; In read without -r, backslash-char becomes just char (backslash removed)
;; This does NOT interpret C escapes (\n, \t, etc.) — only removes the backslash
(def (read-strip-backslashes s)
  (let ((len (string-length s))
        (buf (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len) (get-output-string buf))
        ((and (char=? (string-ref s i) #\\) (< (+ i 1) len))
         (display (string-ref s (+ i 1)) buf)
         (loop (+ i 2)))
        ((and (char=? (string-ref s i) #\\) (= (+ i 1) len))
         ;; Trailing backslash - keep it
         (display #\\ buf)
         (loop (+ i 1)))
        (else
         (display (string-ref s i) buf)
         (loop (+ i 1)))))))

;; IFS splitting for read builtin (raw mode — no backslash processing)
;; Rules:
;; - IFS whitespace chars (space, tab, newline) collapse into single delimiters
;; - Non-whitespace IFS chars are individual delimiters (create empty fields)
;; - Leading IFS whitespace is stripped
;; - Trailing IFS whitespace is stripped
;; - When max-fields > 0 and reached, rest goes to last field
;; - When max-fields = 0, split into unlimited fields
(def (read-ifs-split-raw str ifs max-fields)
  (let ((len (string-length str)))
    (if (= len 0)
      []
      (let ((ifs-ws (ifs-whitespace-chars ifs))
            (ifs-nw (ifs-non-whitespace-chars ifs)))
        ;; Skip leading IFS whitespace
        (let skip-lead ((i 0))
          (if (and (< i len) (ifs-ws-member? (string-ref str i) ifs-ws))
            (skip-lead (+ i 1))
            ;; Now split from position i
            (read-ifs-split-from str i len ifs-ws ifs-nw max-fields)))))))

;; IFS splitting for read builtin (non-raw mode — backslash processing during split)
;; Backslash escapes the next character, preventing it from being treated as IFS
;; The backslash itself is removed from the output
(def (read-ifs-split str ifs max-fields)
  (let ((len (string-length str)))
    (if (= len 0)
      []
      (let ((ifs-ws (ifs-whitespace-chars ifs))
            (ifs-nw (ifs-non-whitespace-chars ifs)))
        ;; Skip leading IFS whitespace (backslash-space is NOT whitespace)
        (let skip-lead ((i 0))
          (if (and (< i len)
                   (not (char=? (string-ref str i) #\\))
                   (ifs-ws-member? (string-ref str i) ifs-ws))
            (skip-lead (+ i 1))
            ;; Now split with backslash processing
            (read-ifs-split-bs str i len ifs-ws ifs-nw max-fields)))))))

;; Helper: extract IFS whitespace characters (space, tab, newline in IFS)
(def (ifs-whitespace-chars ifs)
  (let ((buf (open-output-string)))
    (let loop ((i 0))
      (if (>= i (string-length ifs))
        (get-output-string buf)
        (let ((ch (string-ref ifs i)))
          (when (or (char=? ch #\space) (char=? ch #\tab) (char=? ch #\newline))
            (display ch buf))
          (loop (+ i 1)))))))

;; Helper: extract IFS non-whitespace characters
(def (ifs-non-whitespace-chars ifs)
  (let ((buf (open-output-string)))
    (let loop ((i 0))
      (if (>= i (string-length ifs))
        (get-output-string buf)
        (let ((ch (string-ref ifs i)))
          (when (not (or (char=? ch #\space) (char=? ch #\tab) (char=? ch #\newline)))
            (display ch buf))
          (loop (+ i 1)))))))

(def (ifs-ws-member? ch ws)
  (let loop ((i 0))
    (and (< i (string-length ws))
         (or (char=? ch (string-ref ws i))
             (loop (+ i 1))))))

(def (ifs-nw-member? ch nw)
  (let loop ((i 0))
    (and (< i (string-length nw))
         (or (char=? ch (string-ref nw i))
             (loop (+ i 1))))))

;; Raw IFS splitting (no backslash processing)
(def (read-ifs-split-from str start len ifs-ws ifs-nw max-fields)
  (let ((buf (open-output-string))
        (fields [])
        (field-count 1))
    (let loop ((i start))
      (cond
        ((>= i len)
         ;; End of string: strip trailing IFS for last field
         (let* ((s (get-output-string buf))
                (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
           (reverse (if (or (> (string-length s) 0) (pair? fields))
                      (cons s fields)
                      fields))))
        ;; Non-whitespace IFS delimiter
        ((ifs-nw-member? (string-ref str i) ifs-nw)
         ;; Check if we're at max-fields — if so, include delimiter in current field
         (if (and (> max-fields 0) (>= field-count max-fields))
           (begin (display (string-ref str i) buf) (loop (+ i 1)))
           (let ((s (get-output-string buf)))
             ;; Strip trailing IFS whitespace from current field
             (set! fields (cons (strip-trailing-ifs-ws s ifs-ws) fields))
             (set! buf (open-output-string))
             (set! field-count (+ field-count 1))
             ;; Skip IFS whitespace after non-whitespace delimiter
             (let skip ((j (+ i 1)))
               (if (and (< j len) (ifs-ws-member? (string-ref str j) ifs-ws))
                 (skip (+ j 1))
                 ;; If NOW at max fields, take rest as last field
                 (if (and (> max-fields 0) (>= field-count max-fields))
                   (begin
                     (display (substring str j len) buf)
                     (let* ((s (get-output-string buf))
                            (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
                       (reverse (cons s fields))))
                   (loop j)))))))
        ;; IFS whitespace
        ((ifs-ws-member? (string-ref str i) ifs-ws)
         ;; Check if we're at max-fields AND buf has content — if so, include ws in current field
         ;; NOTE: get-output-string resets the port in Gambit, so save to var first
         (let ((buf-content (get-output-string buf)))
           (if (and (> max-fields 0) (>= field-count max-fields)
                    (> (string-length buf-content) 0))
             ;; At max fields: include buffered content + rest of string
             (let* ((full (string-append buf-content (substring str i len)))
                    (full (strip-trailing-ifs-for-read full ifs-ws ifs-nw)))
               (reverse (cons full fields)))
             (begin
               (when (> (string-length buf-content) 0)
                 (set! fields (cons buf-content fields))
                 (set! field-count (+ field-count 1)))
               (set! buf (open-output-string))
               ;; Skip consecutive IFS whitespace
               (let skip ((j (+ i 1)))
                 (if (and (< j len) (ifs-ws-member? (string-ref str j) ifs-ws))
                   (skip (+ j 1))
                   ;; If NOW at max fields, take rest as last field
                   (if (and (> max-fields 0) (>= field-count max-fields))
                     (begin
                       (display (substring str j len) buf)
                       (let* ((s (get-output-string buf))
                              (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
                         (reverse (cons s fields))))
                     (loop j))))))))
        ;; Regular character
        (else
         (display (string-ref str i) buf)
         (loop (+ i 1)))))))

;; Non-raw IFS splitting with backslash processing
(def (read-ifs-split-bs str start len ifs-ws ifs-nw max-fields)
  ;; Helper to process remaining with backslash removal for last field
  (define (process-rest-bs j buf)
    (let rloop ((j j))
      (cond
        ((>= j len) #!void)
        ((and (char=? (string-ref str j) #\\) (< (+ j 1) len))
         (display (string-ref str (+ j 1)) buf)
         (rloop (+ j 2)))
        (else
         (display (string-ref str j) buf)
         (rloop (+ j 1))))))
  (let ((buf (open-output-string))
        (fields [])
        (field-count 1))
    (let loop ((i start))
      (cond
        ((>= i len)
         ;; End of string: strip trailing IFS for last field
         (let* ((s (get-output-string buf))
                (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
           (reverse (if (or (> (string-length s) 0) (pair? fields))
                      (cons s fields)
                      fields))))
        ;; Backslash: escape next character (prevent IFS matching)
        ;; But only process backslash specially if NOT at max-fields
        ;; (at max-fields, backslash still escapes to prevent IFS matching)
        ((and (char=? (string-ref str i) #\\) (< (+ i 1) len))
         (display (string-ref str (+ i 1)) buf)
         (loop (+ i 2)))
        ;; Trailing backslash
        ((and (char=? (string-ref str i) #\\) (= (+ i 1) len))
         (display #\\ buf)
         (loop (+ i 1)))
        ;; Non-whitespace IFS delimiter
        ((ifs-nw-member? (string-ref str i) ifs-nw)
         ;; Check if we're at max-fields — if so, include delimiter in current field
         (if (and (> max-fields 0) (>= field-count max-fields))
           (begin (display (string-ref str i) buf) (loop (+ i 1)))
           (let ((s (get-output-string buf)))
             (set! fields (cons (strip-trailing-ifs-ws s ifs-ws) fields))
             (set! buf (open-output-string))
             (set! field-count (+ field-count 1))
             ;; Skip IFS whitespace after non-whitespace delimiter
             (let skip ((j (+ i 1)))
               (if (and (< j len)
                        (not (char=? (string-ref str j) #\\))
                        (ifs-ws-member? (string-ref str j) ifs-ws))
                 (skip (+ j 1))
                 ;; If NOW at max fields, process rest with backslash removal
                 (if (and (> max-fields 0) (>= field-count max-fields))
                   (begin
                     (process-rest-bs j buf)
                     (let* ((s (get-output-string buf))
                            (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
                       (reverse (cons s fields))))
                   (loop j)))))))
        ;; IFS whitespace
        ((ifs-ws-member? (string-ref str i) ifs-ws)
         ;; Check if we're at max-fields AND buf has content — keep ws in current field
         ;; NOTE: get-output-string resets the port in Gambit, so save to var first
         (let ((buf-content (get-output-string buf)))
           (if (and (> max-fields 0) (>= field-count max-fields)
                    (> (string-length buf-content) 0))
             ;; At max fields: include buffered content + rest of string with bs processing
             (let ((new-buf (open-output-string)))
               (display buf-content new-buf)
               (process-rest-bs i new-buf)
               (let* ((s (get-output-string new-buf))
                      (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
                 (reverse (cons s fields))))
             (begin
               (when (> (string-length buf-content) 0)
                 (set! fields (cons buf-content fields))
                 (set! field-count (+ field-count 1)))
               (set! buf (open-output-string))
               ;; Skip consecutive IFS whitespace
               (let skip ((j (+ i 1)))
                 (if (and (< j len)
                          (not (char=? (string-ref str j) #\\))
                          (ifs-ws-member? (string-ref str j) ifs-ws))
                   (skip (+ j 1))
                   ;; If NOW at max fields, process rest with backslash removal
                   (if (and (> max-fields 0) (>= field-count max-fields))
                     (begin
                       (process-rest-bs j buf)
                       (let* ((s (get-output-string buf))
                              (s (strip-trailing-ifs-for-read s ifs-ws ifs-nw)))
                         (reverse (cons s fields))))
                     (loop j))))))))
        ;; Regular character
        (else
         (display (string-ref str i) buf)
         (loop (+ i 1)))))))

;; Strip trailing IFS whitespace from a string
(def (strip-trailing-ifs-ws s ws)
  (if (= (string-length ws) 0)
    s
    (let loop ((end (string-length s)))
      (if (and (> end 0) (ifs-ws-member? (string-ref s (- end 1)) ws))
        (loop (- end 1))
        (substring s 0 end)))))

;; Strip trailing IFS from the last field for read:
;; 1. Strip trailing IFS whitespace
;; 2. If the last char is a non-ws IFS char AND it is NOT preceded by another non-ws IFS char,
;;    strip that one trailing non-ws IFS char (and any IFS whitespace before it)
(def (strip-trailing-ifs-for-read s ifs-ws ifs-nw)
  (let* ((s1 (strip-trailing-ifs-ws s ifs-ws))
         (len (string-length s1)))
    (if (and (> len 0)
             (ifs-nw-member? (string-ref s1 (- len 1)) ifs-nw)
             ;; Only strip if NOT preceded by another non-ws IFS char
             (or (= len 1)
                 (not (ifs-nw-member? (string-ref s1 (- len 2)) ifs-nw))))
      ;; Strip the trailing non-ws IFS char
      (strip-trailing-ifs-ws (substring s1 0 (- len 1)) ifs-ws)
      s1)))

;; trap ['command'] [signal ...]
(builtin-register! "trap"
  (lambda (args env)
    ;; Helper: print a trap entry with canonical signal display name
    (def (print-trap short-name action)
      (let ((display-name (signal-display-name short-name)))
        (if (string? action)
          (displayln (format "trap -- '~a' ~a" action display-name))
          (displayln (format "trap -- '' ~a" display-name)))))
    ;; Helper: normalize signal arg, print error if invalid
    (def (normalize-or-error sig)
      (let ((n (normalize-signal-arg sig)))
        (unless n
          (fprintf (current-error-port) "trap: ~a: invalid signal specification~n" sig))
        n))
    ;; Helper: check if string is an unsigned decimal integer (POSIX heuristic)
    ;; "0", "42", "07" are unsigned integers; "-1", " 42 ", "abc" are not
    (def (unsigned-integer? s)
      (and (> (string-length s) 0)
           (let loop ((i 0))
             (if (>= i (string-length s))
               #t
               (and (char-numeric? (string-ref s i))
                    (loop (+ i 1)))))))
    ;; Helper: set trap action for given signals
    (def (trap-set-action action signals)
      (let ((status 0))
        (for-each
         (lambda (sig)
           (let ((norm (normalize-or-error sig)))
             (if norm
               (cond
                 ((string=? action "-") (trap-set! norm 'default))
                 ((string=? action "") (trap-set! norm 'ignore))
                 (else (trap-set! norm action)))
               (set! status 1))))
         signals)
        status))
    ;; Main dispatch
    (cond
      ((null? args)
       ;; Print all traps
       (for-each
        (lambda (pair) (print-trap (car pair) (cdr pair)))
        (trap-list))
       0)
      ((string=? (car args) "-l")
       ;; List signal names
       (for-each displayln (signal-name-list))
       0)
      ((string=? (car args) "-p")
       ;; Print specific traps
       (let ((sigs (cdr args)))
         (for-each
          (lambda (sig)
            (let ((norm (normalize-or-error sig)))
              (when norm
                (let ((action (trap-get norm)))
                  (when action (print-trap norm action))))))
          (if (pair? sigs) sigs (map car (trap-list)))))
       0)
      ;; Unrecognized -X options (e.g. -1, -e) are errors
      ((and (> (string-length (car args)) 1)
            (char=? (string-ref (car args) 0) #\-)
            (not (string=? (car args) "-"))
            (not (string=? (car args) "--"))
            (not (string=? (car args) "-l"))
            (not (string=? (car args) "-p")))
       (fprintf (current-error-port) "trap: ~a: invalid signal specification~n" (car args))
       1)
      ((string=? (car args) "--")
       ;; Skip --, process rest
       (let ((rest (cdr args)))
         (if (null? rest)
           ;; trap -- alone = print traps
           (begin
             (for-each (lambda (pair) (print-trap (car pair) (cdr pair))) (trap-list))
             0)
           ;; Check if next is also a signal (single-arg clear) or action + signals
           (if (= (length rest) 1)
             ;; Could be: trap -- SIGNAL (clear) or trap -- action (with no signals = error)
             ;; Per bash: trap -- EXIT clears EXIT trap
             (let ((norm (normalize-or-error (car rest))))
               (if norm
                 (begin (trap-set! norm 'default) 0)
                 1))
             (trap-set-action (car rest) (cdr rest))))))
      ;; Single argument (no action, just signal name): clear the trap
      ;; trap EXIT = clear EXIT trap, trap 0 = clear EXIT trap
      ((= (length args) 1)
       (let ((norm (normalize-or-error (car args))))
         (if norm
           (begin (trap-set! norm 'default) 0)
           1)))
      ;; Two or more args: check POSIX unsigned integer heuristic
      ;; If first arg is an unsigned decimal integer, all args are signals to reset
      ((>= (length args) 2)
       (if (unsigned-integer? (car args))
         ;; POSIX: all operands are conditions to reset
         (let ((status 0))
           (for-each
            (lambda (sig)
              (let ((norm (normalize-or-error sig)))
                (if norm
                  (trap-set! norm 'default)
                  (set! status 1))))
            args)
           status)
         ;; Normal case: first arg is action, rest are signals
         (trap-set-action (car args) (cdr args))))
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
             (let ((new-pwd (strip-trailing-slash (current-directory))))
               (env-set! env "PWD" new-pwd)
               (*internal-pwd* new-pwd))
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
          (let ((new-pwd (strip-trailing-slash (current-directory))))
            (env-set! env "PWD" new-pwd)
            (*internal-pwd* new-pwd))
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
  (test-eval-or args))

;; Parse: expr -o expr -o ...
(def (test-eval-or args)
  ;; Find first -o at top level (not inside parens)
  ;; Only treat -o as binary OR if there's something on both sides
  (let split ((rest args) (left []) (depth 0))
    (cond
      ((null? rest)
       (test-eval-and (reverse left)))
      ;; Track ( ) nesting
      ((string=? (car rest) "(")
       (split (cdr rest) (cons (car rest) left) (+ depth 1)))
      ((string=? (car rest) ")")
       (split (cdr rest) (cons (car rest) left) (max 0 (- depth 1))))
      ((and (= depth 0)
            (string=? (car rest) "-o")
            (pair? left)         ;; must have left operand
            (pair? (cdr rest)))  ;; must have right operand
       ;; Evaluate left side; if true, short-circuit
       (let ((lresult (test-eval-and (reverse left))))
         (if (= lresult 0) 0
           (test-eval-or (cdr rest)))))
      (else
       (split (cdr rest) (cons (car rest) left) depth)))))

;; Parse: expr -a expr -a ...
(def (test-eval-and args)
  (let split ((rest args) (left []) (depth 0))
    (cond
      ((null? rest)
       (test-eval-not (reverse left)))
      ;; Track ( ) nesting
      ((string=? (car rest) "(")
       (split (cdr rest) (cons (car rest) left) (+ depth 1)))
      ((string=? (car rest) ")")
       (split (cdr rest) (cons (car rest) left) (max 0 (- depth 1))))
      ((and (= depth 0) (string=? (car rest) "-a"))
       ;; Check if this is unary -a (file exists) or binary -a (AND)
       ;; It's binary -a if we have something on both sides
       (if (and (pair? left) (pair? (cdr rest)))
         (let ((lresult (test-eval-not (reverse left))))
           (if (= lresult 0)
             (test-eval-and (cdr rest))
             1))
         ;; Could be unary, don't split here
         (split (cdr rest) (cons (car rest) left) depth)))
      (else
       (split (cdr rest) (cons (car rest) left) depth)))))

;; Find matching ) for ( at the start of args. Returns index or #f.
(def (test-find-close-paren args)
  (let loop ((rest (cdr args)) (depth 1) (idx 1))
    (cond
      ((null? rest) #f)
      ((string=? (car rest) "(") (loop (cdr rest) (+ depth 1) (+ idx 1)))
      ((string=? (car rest) ")")
       (if (= depth 1) idx
         (loop (cdr rest) (- depth 1) (+ idx 1))))
      (else (loop (cdr rest) depth (+ idx 1))))))

;; Parse: ! expr | ( expr ) | primary
(def (test-eval-not args)
  (let ((len (length args)))
    (cond
      ((= len 0) 1)
      ;; Negation: ! expr
      ((string=? (car args) "!")
       (let ((result (test-eval-not (cdr args))))
         (if (= result 0) 1 (if (= result 1) 0 result))))
      ;; Parenthesized: ( ... ) — find matching close paren
      ((and (string=? (car args) "(") (>= len 3))
       (let ((close-idx (test-find-close-paren args)))
         (if (and close-idx (= close-idx (- len 1)))
           ;; ( expr ) — entire args is parenthesized
           (test-eval (list-head (cdr args) (- close-idx 1)))
           ;; ( not at end — fall through to normal evaluation
           (test-eval-primary args len))))
      ;; Primary expressions
      (else (test-eval-primary args len)))))

;; Handle primary test expressions by argument count
(def (test-eval-primary args len)
  (cond
    ;; 1 arg: non-empty string test
    ((= len 1)
     (if (> (string-length (car args)) 0) 0 1))
    ;; 2 args: unary operator
    ((= len 2)
     (test-unary (car args) (cadr args)))
    ;; 3 args: binary operator OR special forms
    ((= len 3)
     (let ((a (car args)) (b (cadr args)) (c (caddr args)))
       (cond
         ;; ! unary-op arg
         ((string=? a "!")
          (let ((r (test-eval-not (cdr args))))
            (if (= r 0) 1 (if (= r 1) 0 r))))
         ;; ( expr )
         ((and (string=? a "(") (string=? c ")"))
          (test-eval-not (list b)))
         ;; binary operator
         (else (test-binary a b c)))))
    ;; 4 args: ! with 3 arg expr, or ( expr op expr )
    ((= len 4)
     (let ((a (car args)) (d (list-ref args 3)))
       (cond
         ((string=? a "!")
          (let ((r (test-eval (cdr args))))
            (if (= r 0) 1 (if (= r 1) 0 r))))
         ;; ( expr op expr ) - parenthesized binary
         ((and (string=? a "(") (string=? d ")"))
          (test-eval (list (cadr args) (caddr args))))
         (else 2))))
    ;; 5+ args: should have been handled by -a/-o splitting
    ;; Try to handle remaining cases
    (else
     ;; Check for ! at the start
     (if (string=? (car args) "!")
       (let ((r (test-eval (cdr args))))
         (if (= r 0) 1 (if (= r 1) 0 r)))
       2))))

(def (test-unary flag arg)
  (case (string->symbol flag)
    ((-z) (if (= (string-length arg) 0) 0 1))
    ((-n) (if (> (string-length arg) 0) 0 1))
    ((-e -a) (if (file-exists? arg) 0 1))  ;; -a as unary is alias for -e
    ((-f) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'regular)) 0 1))
    ((-d) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'directory)) 0 1))
    ((-r) (if (= (ffi-access arg 4) 0) 0 1))  ;; R_OK = 4
    ((-w) (if (= (ffi-access arg 2) 0) 0 1))  ;; W_OK = 2
    ((-x) (if (= (ffi-access arg 1) 0) 0 1))  ;; X_OK = 1
    ((-s) (if (and (file-exists? arg)
                    (> (file-info-size (file-info arg)) 0)) 0 1))
    ((-L -h) ;; Symlink: use file-info with #f to not follow symlinks
     (with-catch (lambda (e) 1)
       (lambda ()
         (let ((fi (file-info arg #f)))
           (if (eq? (file-info-type fi) 'symbolic-link) 0 1)))))
    ((-t) (let ((fd (string->number arg)))
             (if (not fd) 2  ;; invalid fd → exit 2
               (if (= (ffi-isatty fd) 1) 0 1))))
    ((-p) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'fifo)) 0 1))
    ((-b) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'block-special)) 0 1))
    ((-c) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'character-special)) 0 1))
    ((-v) ;; True if the named variable is set
     (if (*test-var-fn*)
       (if ((*test-var-fn*) arg) 0 1)
       1))
    ((-o) ;; Shell option test
     (if (*test-option-fn*)
       (if ((*test-option-fn*) arg) 0 1)
       1))
    ((-G) (with-catch (lambda (e) 1)
            (lambda () (if (and (file-exists? arg)
                                (= (file-info-group (file-info arg)) (ffi-getegid))) 0 1))))
    ((-O) (with-catch (lambda (e) 1)
            (lambda () (if (and (file-exists? arg)
                                (= (file-info-owner (file-info arg)) (ffi-geteuid))) 0 1))))
    ((-u) (with-catch (lambda (e) 1)  ;; setuid
            (lambda () (if (and (file-exists? arg)
                                (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o4000)))) 0 1))))
    ((-g) (with-catch (lambda (e) 1)  ;; setgid
            (lambda () (if (and (file-exists? arg)
                                (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o2000)))) 0 1))))
    ((-k) (with-catch (lambda (e) 1)  ;; sticky bit
            (lambda () (if (and (file-exists? arg)
                                (not (zero? (bitwise-and (file-info-mode (file-info arg)) #o1000)))) 0 1))))
    ((-S) (if (and (file-exists? arg)
                    (eq? (file-info-type (file-info arg)) 'socket)) 0 1))
    ((-N) (with-catch (lambda (e) 1)  ;; modified since last read
            (lambda ()
              (if (file-exists? arg)
                (let ((fi (file-info arg)))
                  (if (> (time->seconds (file-info-last-modification-time fi))
                         (time->seconds (file-info-last-access-time fi))) 0 1))
                1))))
    (else
     ;; Not a recognized flag, treat as non-empty string test for both args
     ;; This handles cases like [ "str1" "str2" ] which is a syntax error → exit 2
     2)))

;; Parameter for checking variable existence (set by executor)
(def *test-var-fn* (make-parameter #f))
;; Parameter for checking shell options (set by executor)
(def *test-option-fn* (make-parameter #f))

;; Parse a shell integer for [ comparisons: handles decimal, 0x hex, 0 octal, N#val
(def (test-parse-integer str)
  (let ((s (test-string-trim str)))
    (cond
      ((string=? s "") #f)
      ;; Negative
      ((and (> (string-length s) 0) (char=? (string-ref s 0) #\-))
       (let ((n (test-parse-integer (substring s 1 (string-length s)))))
         (and n (- n))))
      ;; [/test treats all numbers as decimal — no 0x hex, no 0 octal
      ;; Plain decimal only
      (else (string->number s)))))

(def (test-string-trim s)
  (let* ((len (string-length s))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i len))
                (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                  (loop (- i 1)) i))))
    (substring s start end)))

(def (test-binary left op right)
  (case (string->symbol op)
    ((= ==) (if (string=? left right) 0 1))
    ((!=) (if (not (string=? left right)) 0 1))
    ((<) (if (string<? left right) 0 1))
    ((>) (if (string>? left right) 0 1))
    ((-eq) (test-int-cmp = left right))
    ((-ne) (test-int-cmp (lambda (a b) (not (= a b))) left right))
    ((-lt) (test-int-cmp < left right))
    ((-le) (test-int-cmp <= left right))
    ((-gt) (test-int-cmp > left right))
    ((-ge) (test-int-cmp >= left right))
    ((-nt) (if (and (file-exists? left) (file-exists? right)
                     (> (time->seconds (file-info-last-modification-time (file-info left)))
                        (time->seconds (file-info-last-modification-time (file-info right))))) 0 1))
    ((-ot) (if (and (file-exists? left) (file-exists? right)
                     (< (time->seconds (file-info-last-modification-time (file-info left)))
                        (time->seconds (file-info-last-modification-time (file-info right))))) 0 1))
    ((-ef) (if (and (file-exists? left) (file-exists? right)
                     (let ((li (file-info left)) (ri (file-info right)))
                       (and (= (file-info-device li) (file-info-device ri))
                            (= (file-info-inode li) (file-info-inode ri))))) 0 1))
    ;; Binary -a (AND) and -o (OR)
    ((-a) (let ((l (if (> (string-length left) 0) 0 1))
                (r (if (> (string-length right) 0) 0 1)))
            (if (and (= l 0) (= r 0)) 0 1)))
    ((-o) (let ((l (if (> (string-length left) 0) 0 1))
                (r (if (> (string-length right) 0) 0 1)))
            (if (or (= l 0) (= r 0)) 0 1)))
    (else 2)))  ;; Unknown operator → exit 2

;; Integer comparison for test: returns 0 (true), 1 (false), 2 (error)
(def (test-int-cmp cmp left right)
  (let ((l (test-parse-integer left))
        (r (test-parse-integer right)))
    (cond
      ((not l)
       (fprintf (current-error-port) "test: ~a: integer expression expected~n" left)
       2)
      ((not r)
       (fprintf (current-error-port) "test: ~a: integer expression expected~n" right)
       2)
      (else (if (cmp l r) 0 1)))))

;;; --- Helpers ---

;; Returns (cons text stop?) — stop? is #t when \c was encountered
(def (echo-expand-escapes str)
  (let ((len (string-length str))
        (buf (open-output-string)))
    (let loop ((i 0))
      (cond
        ((>= i len) (cons (get-output-string buf) #f))
        ((and (char=? (string-ref str i) #\\) (< (+ i 1) len))
         (let ((next (string-ref str (+ i 1))))
           (case next
             ((#\n) (display "\n" buf) (loop (+ i 2)))
             ((#\t) (display "\t" buf) (loop (+ i 2)))
             ((#\r) (display "\r" buf) (loop (+ i 2)))
             ((#\a) (display "\a" buf) (loop (+ i 2)))
             ((#\b) (display "\b" buf) (loop (+ i 2)))
             ((#\e #\E) (display (string (integer->char #x1b)) buf) (loop (+ i 2)))
             ((#\f) (display "\x0c;" buf) (loop (+ i 2)))
             ((#\v) (display "\x0b;" buf) (loop (+ i 2)))
             ((#\\) (display "\\" buf) (loop (+ i 2)))
             ((#\0) ;; \0nnn - octal (up to 3 digits after the 0)
              (let oloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 3)
                         (char>=? (string-ref str j) #\0)
                         (char<=? (string-ref str j) #\7))
                  (oloop (+ j 1)
                         (+ (* val 8) (- (char->integer (string-ref str j))
                                         (char->integer #\0)))
                         (+ count 1))
                  (begin
                    (display (string (integer->char (modulo val 256))) buf)
                    (loop j)))))
             ((#\x) ;; \xHH - hex byte (up to 2 digits)
              (let hloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 2))
                  (let ((hv (hex-digit-value (string-ref str j))))
                    (if hv
                      (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                      (if (= count 0)
                        ;; No hex digits after \x → literal \x
                        (begin (display "\\x" buf) (loop j))
                        (begin (display (string (integer->char val)) buf) (loop j)))))
                  (if (= count 0)
                    (begin (display "\\x" buf) (loop j))
                    (begin (display (string (integer->char val)) buf) (loop j))))))
             ((#\u) ;; \uNNNN - unicode (up to 4 hex digits)
              (let hloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 4))
                  (let ((hv (hex-digit-value (string-ref str j))))
                    (if hv
                      (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                      (if (= count 0)
                        (begin (display "\\u" buf) (loop j))
                        (begin (display-unicode-char val buf) (loop j)))))
                  (if (= count 0)
                    (begin (display "\\u" buf) (loop j))
                    (begin (display-unicode-char val buf) (loop j))))))
             ((#\U) ;; \UNNNNNNNN - unicode (up to 8 hex digits)
              (let hloop ((j (+ i 2)) (val 0) (count 0))
                (if (and (< j len) (< count 8))
                  (let ((hv (hex-digit-value (string-ref str j))))
                    (if hv
                      (hloop (+ j 1) (+ (* val 16) hv) (+ count 1))
                      (if (= count 0)
                        (begin (display "\\U" buf) (loop j))
                        (begin (display-unicode-char val buf) (loop j)))))
                  (if (= count 0)
                    (begin (display "\\U" buf) (loop j))
                    (begin (display-unicode-char val buf) (loop j))))))
             ((#\c) (cons (get-output-string buf) #t))  ;; \c = stop all output
             (else (display "\\" buf) (display (string next) buf) (loop (+ i 2))))))
        (else
         (display (string-ref str i) buf)
         (loop (+ i 1)))))))

;;; --- Full printf implementation ---

;; Format a printf string with arguments
;; Supports argument recycling: if more args than format specifiers, repeat format
(def (shell-printf fmt args)
  (let ((buf (open-output-string)))
    (parameterize ((*printf-stop* #f))
      (if (null? args)
        ;; No args: just process format escapes once
        (begin (printf-format-once fmt [] buf)
               (get-output-string buf))
        ;; With args: loop until all consumed (argument recycling)
        (let loop ((remaining args))
          (if (*printf-stop*)
            (get-output-string buf)
            (let ((leftover (printf-format-once fmt remaining buf)))
              (if (or (null? leftover)
                      (equal? leftover remaining)) ;; no args consumed → stop
                (get-output-string buf)
                (loop leftover)))))))))

;; Process format string once, consuming args as needed
;; Returns remaining args after one pass through format
(def (printf-format-once fmt args buf)
  (let ((flen (string-length fmt)))
    (let loop ((i 0) (args args))
      (cond
        ((or (>= i flen) (*printf-stop*)) args)
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
    ;; Check for %(strftime)T format
    (if (and (< i flen) (char=? (string-ref fmt i) #\()
             (let ((close (string-find-char-from fmt #\) (+ i 1))))
               (and close (< (+ close 1) flen)
                    (char=? (string-ref fmt (+ close 1)) #\T))))
      (let* ((close (string-find-char-from fmt #\) (+ i 1)))
             (strfmt (substring fmt (+ i 1) close))
             (arg (if (pair? args) (car args) ""))
             (rest (if (pair? args) (cdr args) []))
             (epoch (cond
                      ((string=? arg "") (inexact->exact (floor (time->seconds (current-time)))))
                      ((string=? arg "-1") (inexact->exact (floor (time->seconds (current-time)))))
                      ((string=? arg "-2")
                       (inexact->exact (floor (time->seconds (current-time)))))
                      (else (or (string->number arg) 0))))
             (result (ffi-strftime strfmt epoch)))
        (display result buf)
        (values (+ close 2) rest))
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
               ;; Parse precision: .N or just . (=0)
               (let-values (((prec i args)
                             (if (and (< i flen) (char=? (string-ref fmt i) #\.))
                               (let-values (((p j a) (parse-printf-number fmt (+ i 1) args)))
                                 (values (or p 0) j a))  ;; . alone means precision 0
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
                       ;; %d %i - signed decimal integer
                       ((#\d #\i)
                        (let* ((n (string->integer-safe arg))
                               (neg? (< n 0))
                               (digits (number->string (abs n)))
                               ;; Precision: minimum number of digits (zero-pad)
                               (digits (if (and prec (> prec (string-length digits)))
                                         (string-append (make-string (- prec (string-length digits)) #\0)
                                                        digits)
                                         digits))
                               ;; Precision of 0 with value 0 produces empty string
                               (digits (if (and prec (= prec 0) (= n 0)) "" digits))
                               (s (if neg? (string-append "-" digits) digits))
                               (s (if (and plus-sign? (not neg?)) (string-append "+" s) s))
                               (s (if (and space-sign? (not neg?) (not plus-sign?))
                                    (string-append " " s) s))
                               ;; Zero-pad only when no precision specified
                               (pad-ch (if (and zero-pad? (not left-align?) (not prec)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %u - unsigned decimal integer
                       ((#\u)
                        (let* ((n (string->integer-safe arg))
                               ;; Two's complement for negative numbers (64-bit)
                               (n (if (< n 0) (+ (expt 2 64) n) n))
                               (digits (number->string n))
                               (digits (if (and prec (> prec (string-length digits)))
                                         (string-append (make-string (- prec (string-length digits)) #\0)
                                                        digits)
                                         digits))
                               (digits (if (and prec (= prec 0) (= n 0)) "" digits))
                               (pad-ch (if (and zero-pad? (not left-align?) (not prec)) #\0 #\space)))
                          (display (pad-string digits (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %o - octal
                       ((#\o)
                        (let* ((n (string->integer-safe arg))
                               ;; Two's complement for negative numbers (64-bit)
                               (n (if (< n 0) (+ (expt 2 64) n) n))
                               (digits (number->string n 8))
                               (digits (if (and prec (> prec (string-length digits)))
                                         (string-append (make-string (- prec (string-length digits)) #\0)
                                                        digits)
                                         digits))
                               (digits (if (and prec (= prec 0) (= n 0)) "" digits))
                               (s (if (and alt-form? (not (string=? digits ""))
                                           (not (char=? (string-ref digits 0) #\0)))
                                    (string-append "0" digits) digits))
                               (pad-ch (if (and zero-pad? (not left-align?) (not prec)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %x %X - hexadecimal
                       ((#\x #\X)
                        (let* ((n (string->integer-safe arg))
                               ;; Two's complement for negative numbers (64-bit)
                               (n (if (< n 0) (+ (expt 2 64) n) n))
                               (raw (number->string n 16))
                               (raw (if (char=? spec #\X) (string-upcase raw) raw))
                               (digits (if (and prec (> prec (string-length raw)))
                                         (string-append (make-string (- prec (string-length raw)) #\0)
                                                        raw)
                                         raw))
                               (digits (if (and prec (= prec 0) (= n 0)) "" digits))
                               (s (if (and alt-form? (not (= n 0)))
                                    (string-append (if (char=? spec #\X) "0X" "0x") digits) digits))
                               (pad-ch (if (and zero-pad? (not left-align?) (not prec)) #\0 #\space)))
                          (display (pad-string s (or width 0) left-align? pad-ch) buf)
                          (values (+ i 1) rest)))
                       ;; %c - character (first char of arg)
                       ((#\c)
                        (when (and (string? arg) (> (string-length arg) 0))
                          (display (string-ref arg 0) buf))
                        (values (+ i 1) rest))
                       ;; %b - interpret backslash escapes in argument
                       ((#\b)
                        (let ((stopped? (printf-interpret-b-escapes (if (string? arg) arg "") buf)))
                          (when stopped? (*printf-stop* #t))
                          (values (+ i 1) rest)))
                       ;; %q - shell-quoted
                       ((#\q)
                        (display (shell-quote-string (if (string? arg) arg "")) buf)
                        (values (+ i 1) rest))
                       ;; %f %e %g and uppercase variants - floating point
                       ((#\f #\F #\e #\g #\E #\G)
                        (let* ((n (string->number-safe arg))
                               (p (or prec 6))
                               (s (format-float n spec p)))
                          (display (pad-string s (or width 0) left-align?
                                              (if (and zero-pad? (not left-align?)) #\0 #\space)) buf)
                          (values (+ i 1) rest)))
                       ;; Unknown specifier
                       (else
                        (fprintf (current-error-port) "printf: %~a: invalid format character~n" (string spec))
                        (*printf-conversion-error* #t)
                        (values (+ i 1) args)))))))))))))))

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
;; Helper: convert hex digit char to its integer value, or #f
(def (hex-digit-value ch)
  (cond
    ((and (char>=? ch #\0) (char<=? ch #\9)) (- (char->integer ch) 48))
    ((and (char>=? ch #\a) (char<=? ch #\f)) (- (char->integer ch) 87))
    ((and (char>=? ch #\A) (char<=? ch #\F)) (- (char->integer ch) 55))
    (else #f)))

;; Helper: display a unicode code point as UTF-8
(def (display-unicode-char n buf)
  (if (and (>= n 0) (<= n #x10FFFF))
    (display (string (integer->char n)) buf)
    (display (string (integer->char #xFFFD)) buf)))  ;; replacement char for invalid

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
          ;; \e / \E - escape (0x1b)
          ((#\e #\E) (display (string (integer->char #x1b)) buf) (+ i 1))
          ;; \xHH - hex byte
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
          ;; \uNNNN - unicode (4 hex digits)
          ((#\u)
           (let hex-loop ((j (+ i 1)) (n 0) (count 0))
             (if (and (< j flen) (< count 4))
               (let* ((hch (string-ref fmt j))
                      (hv (hex-digit-value hch)))
                 (if hv
                   (hex-loop (+ j 1) (+ (* n 16) hv) (+ count 1))
                   (begin (display-unicode-char n buf) j)))
               (begin (display-unicode-char n buf) j))))
          ;; \UNNNNNNNN - unicode (8 hex digits)
          ((#\U)
           (let hex-loop ((j (+ i 1)) (n 0) (count 0))
             (if (and (< j flen) (< count 8))
               (let* ((hch (string-ref fmt j))
                      (hv (hex-digit-value hch)))
                 (if hv
                   (hex-loop (+ j 1) (+ (* n 16) hv) (+ count 1))
                   (begin (display-unicode-char n buf) j)))
               (begin (display-unicode-char n buf) j))))
          ;; \c - stop output (only used in %b context, but handle here too)
          ((#\c) (+ i 1))  ;; caller should check for this
          (else (display "\\" buf) (display (string ch) buf) (+ i 1)))))))

;; Interpret backslash escapes in a string (for %b format)
;; Returns #t if \c was encountered (stop all output), #f otherwise
;; %b differences from format-string escapes:
;; - \c stops ALL remaining printf output
;; - \NNN (3-digit octal without leading 0) is supported
;; - \0NNN (4-digit with leading 0) is also supported
(def (printf-interpret-b-escapes str buf)
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (>= i len)
        #f  ;; normal completion
        (let ((ch (string-ref str i)))
          (if (char=? ch #\\)
            (if (< (+ i 1) len)
              (let ((next-ch (string-ref str (+ i 1))))
                (cond
                  ;; \c - stop all output
                  ((char=? next-ch #\c) #t)
                  ;; \0NNN - 4-digit octal (up to 3 after the 0)
                  ((char=? next-ch #\0)
                   (let oloop ((j (+ i 2)) (val 0) (count 0))
                     (if (and (< j len) (< count 3)
                              (char>=? (string-ref str j) #\0)
                              (char<=? (string-ref str j) #\7))
                       (oloop (+ j 1)
                              (+ (* val 8) (- (char->integer (string-ref str j)) 48))
                              (+ count 1))
                       (begin (display (string (integer->char (modulo val 256))) buf)
                              (loop j)))))
                  ;; \NNN - 3-digit octal (without leading 0, 1-7 start)
                  ((and (char>=? next-ch #\1) (char<=? next-ch #\7))
                   (let oloop ((j (+ i 1)) (val 0) (count 0))
                     (if (and (< j len) (< count 3)
                              (char>=? (string-ref str j) #\0)
                              (char<=? (string-ref str j) #\7))
                       (oloop (+ j 1)
                              (+ (* val 8) (- (char->integer (string-ref str j)) 48))
                              (+ count 1))
                       (begin (display (string (integer->char (modulo val 256))) buf)
                              (loop j)))))
                  ;; All other escapes: delegate to printf-escape
                  (else
                   (let ((next (printf-escape str (+ i 1) buf)))
                     (loop next)))))
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

;; Convert string to integer safely (handles 0x, 0, 'c' char literal, +prefix, leading spaces)
;; Parameter to track conversion errors during printf
(def *printf-conversion-error* (make-parameter #f))
;; Parameter to signal \c (stop all output) from %b
(def *printf-stop* (make-parameter #f))

(def (string->integer-safe s)
  (if (or (not (string? s)) (string=? s ""))
    0
    ;; Strip leading whitespace
    (let* ((s (let trim ((i 0))
               (if (and (< i (string-length s))
                        (or (char=? (string-ref s i) #\space)
                            (char=? (string-ref s i) #\tab)))
                 (trim (+ i 1))
                 (substring s i (string-length s))))))
      (cond
        ((string=? s "") 0)
        ;; Character literal: 'c or "c -> ASCII value
        ((and (>= (string-length s) 2)
              (or (char=? (string-ref s 0) #\')
                  (char=? (string-ref s 0) #\")))
         (char->integer (string-ref s 1)))
        ;; Reject base#value syntax (e.g. 64#a)
        ((let loop ((i 0)) (and (< i (string-length s))
                                (or (char=? (string-ref s i) #\#) (loop (+ i 1)))))
         (fprintf (current-error-port) "printf: ~a: invalid number~n" s)
         (*printf-conversion-error* #t)
         0)
        (else
         ;; Handle optional sign prefix
         (let* ((neg? (and (> (string-length s) 0) (char=? (string-ref s 0) #\-)))
                (pos? (and (> (string-length s) 0) (char=? (string-ref s 0) #\+)))
                (s2 (if (or neg? pos?) (substring s 1 (string-length s)) s)))
           ;; Try strict parse first; if it fails, try partial (leading digits)
           (let ((strict-val
                  (cond
                    ((string=? s2 "") #f)
                    ;; Hex: 0x...
                    ((and (>= (string-length s2) 2)
                          (char=? (string-ref s2 0) #\0)
                          (or (char=? (string-ref s2 1) #\x)
                              (char=? (string-ref s2 1) #\X)))
                     (string->number (substring s2 2 (string-length s2)) 16))
                    ;; Octal: 0...
                    ((and (> (string-length s2) 1) (char=? (string-ref s2 0) #\0))
                     (or (string->number s2 8) (string->number s2)))
                    (else (string->number s2)))))
             (if strict-val
               (if neg? (- strict-val) strict-val)
               ;; Partial parse: extract leading digits
               (let ((partial (parse-leading-integer s2)))
                 (fprintf (current-error-port) "printf: ~a: invalid number~n" s)
                 (*printf-conversion-error* #t)
                 (if neg? (- partial) partial))))))))))

;; Parse leading decimal digits from a string, returning the integer value
(def (parse-leading-integer s)
  (let loop ((i 0) (n 0))
    (if (and (< i (string-length s))
             (char>=? (string-ref s i) #\0)
             (char<=? (string-ref s i) #\9))
      (loop (+ i 1) (+ (* n 10) (- (char->integer (string-ref s i)) 48)))
      n)))

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
         (let* ((p (max 1 (or precision 6)))
                (e (if (= n 0.0) 0 (inexact->exact (floor (/ (log (abs n)) (log 10)))))))
           (if (or (< e -4) (>= e p))
             (strip-trailing-zeros (format-scientific n (- p 1) (char=? spec #\G)))
             (strip-trailing-zeros (format-fixed n (max 0 (- p 1 (inexact->exact e))))))))
        (else (number->string n))))))

;; Remove trailing zeros after decimal point for %g format
(def (string-contains-char? s ch)
  (let loop ((i 0))
    (and (< i (string-length s))
         (or (char=? (string-ref s i) ch) (loop (+ i 1))))))

(def (strip-trailing-zeros s)
  (if (string-contains-char? s #\.)
    (let* ((s (let loop ((i (- (string-length s) 1)))
               (if (and (>= i 0) (char=? (string-ref s i) #\0))
                 (loop (- i 1))
                 (substring s 0 (+ i 1)))))
           ;; Remove trailing dot too
           (s (if (and (> (string-length s) 0)
                       (char=? (string-ref s (- (string-length s) 1)) #\.))
                (substring s 0 (- (string-length s) 1))
                s)))
      s)
    s))

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

(def (list-head lst n)
  (if (or (<= n 0) (null? lst)) []
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

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
                 (arith-env-getter env)
                 (arith-env-setter env)))))

;; Import arith-eval from arithmetic module
(import :gsh/arithmetic)
