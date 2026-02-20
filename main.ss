;;; main.ss — Entry point and REPL loop for gsh
;;; The main executable target.

(export main)
(import :std/sugar
        :std/format
        :std/getopt
        :gsh/util
        :gsh/ast
        :gsh/environment
        :gsh/lexer
        :gsh/parser
        :gsh/executor
        :gsh/expander
        :gsh/functions
        :gsh/registry
        :gsh/builtins
        :gsh/history
        :gsh/prompt
        :gsh/lineedit
        :gsh/completion
        :gsh/signals
        :gsh/jobs
        :gsh/script
        :gsh/startup
        :gsh/arithmetic
        :gsh/ffi)

;;; --- CLI argument parsing ---

(def (parse-args args)
  ;; Returns a hash with keys: 'command 'script 'login? 'interactive? 'args
  (let ((result (hash (command #f)
                      (script #f)
                      (login? #f)
                      (interactive? #f)
                      (verbose? #f)
                      (errexit? #f)
                      (args []))))
    (let loop ((args args))
      (cond
        ((null? args) result)
        ;; -c "command"
        ((string=? (car args) "-c")
         (when (pair? (cdr args))
           (hash-put! result 'command (cadr args))
           (when (pair? (cddr args))
             (hash-put! result 'args (cddr args))))
         result)
        ;; --login / -l
        ((or (string=? (car args) "--login")
             (string=? (car args) "-l"))
         (hash-put! result 'login? #t)
         (loop (cdr args)))
        ;; -i: force interactive
        ((string=? (car args) "-i")
         (hash-put! result 'interactive? #t)
         (loop (cdr args)))
        ;; -e: errexit
        ((string=? (car args) "-e")
         (hash-put! result 'errexit? #t)
         (loop (cdr args)))
        ;; -x: xtrace
        ((string=? (car args) "-x")
         (hash-put! result 'verbose? #t)
         (loop (cdr args)))
        ;; --: end of options
        ((string=? (car args) "--")
         (when (pair? (cdr args))
           (hash-put! result 'script (cadr args))
           (hash-put! result 'args (cddr args)))
         result)
        ;; -* other flags: ignore for now
        ((and (> (string-length (car args)) 0)
              (char=? (string-ref (car args) 0) #\-))
         (loop (cdr args)))
        ;; Script filename
        (else
         (hash-put! result 'script (car args))
         (hash-put! result 'args (cdr args))
         result)))
    result))

;;; --- Environment initialization ---

;; Register builtins that need main.ss-level callbacks
;; (can't be defined in builtins.ss due to circular imports)
(def (register-late-builtins! env)
  ;; Set the execute-input callback for eval and command substitution
  (*execute-input* (lambda (input env) (execute-input input env)))
  ;; Set the arithmetic evaluation callback for integer variable attributes
  (*arith-eval-fn* arith-eval)
  ;; Set the execute-external callback for 'command' builtin
  (*execute-external-fn* execute-external)
  ;; Set the trap processing callback for between-command signal handling
  (*process-traps-fn* (lambda (env) (process-traps! env)))

  ;; source / . — source a file into the current environment
  (let ((source-handler
         (lambda (args env)
           ;; Skip leading -- (source accepts/ignores it)
           (let ((args (if (and (pair? args) (string=? (car args) "--"))
                         (cdr args)
                         args)))
             (if (null? args)
               (begin
                 (fprintf (current-error-port) "gsh: source: filename argument required~n")
                 2)
               (let* ((filename (car args))
                      ;; Search PATH if file doesn't contain /
                      ;; source doesn't require execute permission, just a regular file
                      ;; Use env's PATH so prefix assignments like PATH=x . file work
                      (filepath (if (string-contains? filename "/")
                                  filename
                                  (or (find-file-in-path filename (env-get env "PATH"))
                                      filename))))
                 ;; Set positional params if extra args given, restore after
                 (if (pair? (cdr args))
                   (let ((saved-pos (shell-environment-positional env)))
                     (env-set-positional! env (cdr args))
                     (let ((result (source-file! filepath env)))
                       (set! (shell-environment-positional env) saved-pos)
                       result))
                   (source-file! filepath env))))))))
    (builtin-register! "source" source-handler)
    (builtin-register! "." source-handler)))

(def (init-shell-env args-hash)
  (let ((env (make-shell-environment)))
    (env-init! env)
    ;; Set shell-specific defaults
    (env-set! env "SHELL" (or (getenv "SHELL" #f) "/usr/local/bin/gsh"))
    (env-set! env "GSH_VERSION" "0.1.0")
    ;; Set default PS1 if not inherited
    (unless (env-get env "PS1")
      (env-set! env "PS1" "\\u@\\h:\\w\\$ "))
    (unless (env-get env "PS2")
      (env-set! env "PS2" "> "))
    (unless (env-get env "PS4")
      (env-set! env "PS4" "+ "))
    ;; Set HISTFILE/HISTSIZE defaults
    (unless (env-get env "HISTFILE")
      (let ((home (or (env-get env "HOME") (home-directory))))
        (env-set! env "HISTFILE" (string-append home "/.gsh_history"))))
    (unless (env-get env "HISTSIZE")
      (env-set! env "HISTSIZE" "1000"))
    ;; Verbose mode
    (when (hash-ref args-hash 'verbose?)
      (env-option-set! env "xtrace" #t))
    ;; Errexit mode
    (when (hash-ref args-hash 'errexit?)
      (env-option-set! env "errexit" #t))
    ;; Register builtins that need main.ss callbacks
    (register-late-builtins! env)
    env))

;;; --- REPL ---

(def (repl env)
  ;; Initialize history
  (let ((histfile (or (env-get env "HISTFILE") "~/.gsh_history"))
        (histsize (or (string->number (or (env-get env "HISTSIZE") "1000")) 1000)))
    (history-init! histfile histsize))
  ;; Set up signal handlers
  (setup-default-signal-handlers!)
  ;; Save terminal state (slot 0) so we can restore after foreground commands
  ;; that corrupt terminal settings (e.g. top, vim)
  (when (= (ffi-isatty 0) 1)
    (ffi-termios-save 0 0))
  ;; Determine edit mode
  (let ((edit-mode (if (env-option? env "vi") 'vi 'emacs)))
    ;; Main REPL loop
    (let loop ((cmd-num 1))
      (let* (;; Build prompt
             (ps1 (or (env-get env "PS1") "$ "))
             (prompt-str (expand-prompt ps1
                                        (lambda (name) (env-get env name))
                                        (job-count)
                                        cmd-num
                                        (history-count)))
             ;; Create completion function
             (complete-fn (lambda (line cursor)
                           (complete-word line cursor env)))
             ;; Read input with line editor
             (input (line-edit prompt-str complete-fn edit-mode)))
        (cond
          ;; EOF
          ((eq? input 'eof)
           (when (env-option? env "ignoreeof")
             (fprintf (current-error-port) "Use \"exit\" to leave the shell.~n")
             (loop cmd-num))
           ;; Run EXIT trap before exiting
           (run-exit-trap! env)
           (newline (current-error-port)))
          ;; Empty line
          ((string=? (string-trim-whitespace input) "")
           (loop cmd-num))
          ;; Non-empty input
          (else
           ;; History expansion
           ;; history-expand returns (cons expanded-string execute?)
           (let ((result (with-catch
                           (lambda (e)
                             (fprintf (current-error-port) "gsh: ~a~n"
                                      (exception-message e))
                             #f)
                           (lambda () (history-expand input)))))
             (when result
               (let ((expanded (car result))
                     (execute? (cdr result)))
                 ;; Show expanded line if it changed
                 (when (not (string=? expanded input))
                   (displayln expanded (current-error-port)))
                 ;; Add to history (even for :p)
                 (history-add! expanded)
                 ;; Parse and execute (unless :p modifier was used)
                 (when execute?
                   (let ((status (with-catch
                                  (lambda (e)
                                    (cond
                                      ((break-exception? e) 0)
                                      ((continue-exception? e) 0)
                                      ((return-exception? e) (return-exception-status e))
                                      (else (raise e))))
                                  (lambda () (execute-input expanded env)))))
                     (env-set-last-status! env status)
                     ;; Process pending signals and execute trap commands
                     (process-traps! env)
                     ;; ERR trap: execute if last command failed
                     (when (and (not (= status 0)) (trap-get "ERR"))
                       (let ((action (trap-get "ERR")))
                         (when (string? action)
                           (execute-input action env)))))))))
           ;; Restore terminal to sane state after command
           ;; (programs like top/vim may leave echo off or raw mode on).
           ;; Must use Gambit's tty-mode-set! to keep port state in sync;
           ;; raw tcsetattr behind Gambit's back desynchronizes its cache.
           (when (= (ffi-isatty 0) 1)
             (tty-mode-set! (current-input-port) #t #t #f #f 0))
           (loop (+ cmd-num 1))))))))

(def (execute-input input env)
  ;; Parse and execute a line of input
  (with-catch
   (lambda (e)
     (cond
       ((nounset-exception? e) (raise e))
       ((errexit-exception? e) (errexit-exception-status e))
       ((subshell-exit-exception? e) (raise e))
       ((break-exception? e) (raise e))
       ((continue-exception? e) (raise e))
       ((return-exception? e) (raise e))
       (else
        (let ((msg (exception-message e)))
          (fprintf (current-error-port) "gsh: ~a~n" msg)
          ;; POSIX: syntax errors / unclosed bad substitution → exit code 2
          (if (and (string? msg)
                   (or (string-prefix? "parse error" msg)
                       (string-prefix? "bad substitution: unclosed" msg)))
            2 1)))))
   (lambda ()
     (let ((cmd (with-catch
                 (lambda (e)
                   (fprintf (current-error-port) "gsh: syntax error: ~a~n"
                            (exception-message e))
                   'error)
                 (lambda ()
                   (parse-complete-command input (env-shopt? env "extglob"))))))
       (cond
         ((eq? cmd 'error) 2)
         ((not cmd) 0)
         (else (execute-command cmd env)))))))

;;; --- Trap processing ---

;; Process pending signals and execute their trap commands
(def (process-traps! env)
  (let ((signals (pending-signals!)))
    (for-each
     (lambda (sig-name)
       (cond
         ;; CHLD: update job status and notify
         ((string=? sig-name "CHLD")
          (job-update-status!)
          (job-notify!))
         ;; WINCH: terminal resize — update COLUMNS/LINES
         ((string=? sig-name "WINCH")
          #!void)  ;; TODO: update terminal dimensions
         (else #!void))
       ;; Check trap table for this signal
       ;; Save and restore $? so trap doesn't affect main flow
       (let ((action (trap-get sig-name)))
         (when (and action (string? action))
           (let ((saved-status (shell-environment-last-status env)))
             (execute-input action env)
             (env-set-last-status! env saved-status)))))
     signals)))

;; Execute EXIT trap and clean up
;; Clears the trap BEFORE executing to prevent re-entrancy
;; (e.g. exit called inside the trap handler)
(def (run-exit-trap! env)
  (let ((action (trap-get "EXIT")))
    (when (and action (string? action))
      (trap-set! "EXIT" 'default)
      (execute-input action env))))

;;; --- Helpers ---

(def (string-trim-whitespace str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))

;;; --- Internal fd management ---

;; Move Gambit's internal scheduler pipe fds to high numbers (>= 100)
;; so fds 3-9 are free for user shell redirects (exec 3>, etc.)
(def (move-internal-fds-high!)
  (ffi-move-gambit-fds 255))

;;; --- Entry point ---

(def (main . args)
  ;; Move Gambit's internal fds (3-9) to high numbers so user can use exec 3>, etc.
  (move-internal-fds-high!)
  (let* ((args-hash (parse-args args))
         (command (hash-ref args-hash 'command))
         (script (hash-ref args-hash 'script))
         (login? (hash-ref args-hash 'login?))
         (env (init-shell-env args-hash)))
    (cond
      ;; -c "command": execute command string
      (command
       (let ((script-args (hash-ref args-hash 'args)))
         (when (pair? script-args)
           (env-set-shell-name! env (car script-args))
           (env-set-positional! env (cdr script-args)))
         ;; Set up signal handlers so signals are queued (not fatal)
         ;; and EXIT traps can fire on signal-driven exit
         (setup-noninteractive-signal-handlers!)
         ;; Load non-interactive startup
         (load-startup-files! env login? #f)
         ;; Try parse-complete-command first (handles here-docs correctly).
         ;; If parse fails, fall back to execute-string for line-by-line
         ;; execution so earlier commands run before later parse errors
         ;; (e.g. "trap 'echo bye' EXIT; for" — trap must execute).
         (let* ((interactive? (hash-ref args-hash 'interactive?))
                ;; Interactive -c: always use line-by-line execution so nounset
                ;; errors only abort the current line (bash behavior)
                (parse-ok? (and (not interactive?)
                                (with-catch (lambda (e) #f)
                                  (lambda ()
                                    (parse-complete-command command (env-shopt? env "extglob"))
                                    #t))))
                (status (with-catch
                         (lambda (e)
                           (cond
                             ((subshell-exit-exception? e)
                              (subshell-exit-exception-status e))
                             ((errexit-exception? e)
                              (errexit-exception-status e))
                             ((nounset-exception? e) (nounset-exception-status e))
                             ((break-exception? e) 0)
                             ((continue-exception? e) 0)
                             ((return-exception? e) (return-exception-status e))
                             (else
                              (let ((msg (exception-message e)))
                                (fprintf (current-error-port) "gsh: ~a~n" msg)
                                ;; POSIX: syntax errors / unclosed bad substitution → exit code 2
                                (if (and (string? msg)
                                         (or (string-prefix? "parse error" msg)
                                             (string-prefix? "bad substitution: unclosed" msg)))
                                  2 1)))))
                         (lambda ()
                           (if parse-ok?
                             (execute-input command env)
                             (execute-string command env interactive?))))))
           ;; Process any pending signals before exit
           (process-traps! env)
           (run-exit-trap! env)
           (exit status))))
      ;; Script file
      (script
       (let ((script-args (hash-ref args-hash 'args)))
         ;; Set up signal handlers so signals are queued (not fatal)
         (setup-noninteractive-signal-handlers!)
         ;; Load non-interactive startup
         (load-startup-files! env login? #f)
         (let ((status (execute-script script script-args env)))
           (run-exit-trap! env)
           (exit status))))
      ;; Interactive shell
      (else
       (let ((interactive? (or (hash-ref args-hash 'interactive?)
                               (with-catch (lambda (e) #f)
                                 (lambda ()
                                   (= (ffi-isatty 0) 1))))))
         (when interactive?
           (hash-put! args-hash 'interactive? #t))
         ;; Load startup files
         (load-startup-files! env login? interactive?)
         (if interactive?
           ;; Interactive REPL
           (begin
             (*interactive-shell* #t)
             (repl env)
             ;; Run logout for login shells
             (when login?
               (run-logout! env))
             (history-save!)
             (exit 0))
           ;; Non-interactive: read from stdin
           (let ((status (execute-stdin env)))
             (exit status))))))))

(def (execute-stdin env)
  ;; Read and execute commands from stdin
  (let loop ((status 0))
    (let ((line (read-line)))
      (if (eof-object? line)
        (begin
          (run-exit-trap! env)
          status)
        (let ((new-status (with-catch
                           (lambda (e)
                             (cond
                               ((nounset-exception? e) (nounset-exception-status e))
                               ((break-exception? e) 0)
                               ((continue-exception? e) 0)
                               ((return-exception? e) (return-exception-status e))
                               (else (raise e))))
                           (lambda () (execute-input line env)))))
          (env-set-last-status! env new-status)
          ;; Process pending signals and execute trap commands
          (process-traps! env)
          ;; ERR trap: execute if last command failed
          (when (and (not (= new-status 0)) (trap-get "ERR"))
            (let ((action (trap-get "ERR")))
              (when (string? action)
                (execute-input action env))))
          ;; errexit: stop executing in non-interactive mode
          (if (and (not (= new-status 0))
                   (env-option? env "errexit"))
            (begin
              (run-exit-trap! env)
              new-status)
            (loop new-status)))))))
