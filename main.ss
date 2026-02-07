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

  ;; source / . — source a file into the current environment
  (let ((source-handler
         (lambda (args env)
           (if (null? args)
             (begin
               (fprintf (current-error-port) "gsh: source: filename argument required~n")
               2)
             (let* ((filename (car args))
                    ;; Search PATH if file doesn't contain /
                    (filepath (if (string-contains? filename "/")
                                filename
                                (or (which filename) filename))))
               ;; Set positional params if extra args given
               (when (pair? (cdr args))
                 (let ((saved-pos (shell-environment-positional env)))
                   (env-set-positional! env (cdr args))))
               (source-file! filepath env))))))
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
           (let ((expanded (with-catch
                            (lambda (e)
                              (fprintf (current-error-port) "gsh: ~a~n"
                                       (exception-message e))
                              #f)
                            (lambda () (history-expand input)))))
             (when expanded
               ;; Show expanded line if it changed
               (when (not (string=? expanded input))
                 (displayln expanded (current-error-port)))
               ;; Add to history
               (history-add! expanded)
               ;; Parse and execute
               (let ((status (execute-input expanded env)))
                 (env-set-last-status! env status)
                 ;; Process pending signals and execute trap commands
                 (process-traps! env)
                 ;; ERR trap: execute if last command failed
                 (when (and (not (= status 0)) (trap-get "ERR"))
                   (let ((action (trap-get "ERR")))
                     (when (string? action)
                       (execute-input action env)))))))
           (loop (+ cmd-num 1))))))))

(def (execute-input input env)
  ;; Parse and execute a line of input
  (with-catch
   (lambda (e)
     (cond
       ((errexit-exception? e) (errexit-exception-status e))
       (else
        (fprintf (current-error-port) "gsh: ~a~n" (exception-message e))
        1)))
   (lambda ()
     (let ((cmd (with-catch
                 (lambda (e)
                   (fprintf (current-error-port) "gsh: syntax error: ~a~n"
                            (exception-message e))
                   'error)
                 (lambda ()
                   (parse-complete-command input)))))
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
       (let ((action (trap-get sig-name)))
         (when (and action (string? action))
           (execute-input action env))))
     signals)))

;; Execute EXIT trap and clean up
(def (run-exit-trap! env)
  (let ((action (trap-get "EXIT")))
    (when (and action (string? action))
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

;;; --- Entry point ---

(def (main . args)
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
         ;; Load non-interactive startup
         (load-startup-files! env login? #f)
         (let ((status (execute-input command env)))
           (run-exit-trap! env)
           (exit status))))
      ;; Script file
      (script
       (let ((script-args (hash-ref args-hash 'args)))
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
        (let ((new-status (execute-input line env)))
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
