;;; script.ss — Script execution for gsh
;;; Handles running script files and sourcing files into the current environment.

(export #t)
(import :std/sugar
        :std/format
        :gsh/util
        :gsh/ast
        :gsh/environment
        :gsh/functions
        :gsh/lexer
        :gsh/parser
        :gsh/executor
        :gsh/signals
        :gsh/jobs)

;;; --- Public interface ---

;; Execute a script file with arguments.
;; Sets $0 to filename, $1.. to args.
;; Returns exit status.
(def (execute-script filename args env)
  (if (not (file-exists? filename))
    (begin
      (fprintf (current-error-port) "gsh: ~a: No such file or directory~n" filename)
      127)
    (with-catch
     (lambda (e)
       (cond
         ((break-exception? e) 0)
         ((continue-exception? e) 0)
         (else
          (fprintf (current-error-port) "gsh: ~a: ~a~n" filename (exception-message e))
          1)))
     (lambda ()
       (let* ((content (read-file-to-string filename))
              ;; Strip shebang if present
              (script-content (strip-shebang content))
              ;; Create child environment for script
              (script-env (env-push-scope env)))
         ;; Set positional parameters
         (env-set-shell-name! script-env filename)
         (env-set-positional! script-env args)
         ;; Set LINENO tracking
         (env-set! script-env "LINENO" "0")
         ;; Execute the script content
         (execute-string script-content script-env))))))

;; Source a file into the current environment (like bash's `source` or `.`)
;; Runs in the CURRENT environment, not a child.
;; Returns exit status.
(def (source-file! filename env)
  (if (not (file-exists? filename))
    (begin
      (fprintf (current-error-port) "gsh: ~a: No such file or directory~n" filename)
      1)
    (with-catch
     (lambda (e)
       (cond
         ;; break/continue must propagate to caller's loop
         ((break-exception? e) (raise e))
         ((continue-exception? e) (raise e))
         ;; return exits the sourced file, not the calling function
         ((return-exception? e) (return-exception-status e))
         ((errexit-exception? e) (raise e))
         ((subshell-exit-exception? e) (raise e))
         ((nounset-exception? e) (raise e))
         (else
          (fprintf (current-error-port) "gsh: ~a: ~a~n" filename (exception-message e))
          1)))
     (lambda ()
       (let* ((content (read-file-to-string filename))
              (script-content (strip-shebang content)))
         (execute-string script-content env))))))

;;; --- String execution ---

;; Parse and execute a string of shell commands.
;; Used by both execute-script and source-file!
(def (execute-string input env (interactive? #f))
  (let ((lexer (make-shell-lexer input (env-shopt? env "extglob"))))
    (let loop ((status 0))
      (let ((cmd (with-catch
                  (lambda (e)
                    (fprintf (current-error-port) "gsh: syntax error: ~a~n"
                             (exception-message e))
                    'error)
                  (lambda ()
                    ;; Update lexer extglob flag in case shopt changed it
                    (set! (lexer-extglob? lexer) (env-shopt? env "extglob"))
                    (parse-one-line lexer (env-shopt? env "extglob"))))))
        (cond
          ((eq? cmd 'error) 2)  ;; syntax error
          ((not cmd) status)     ;; end of input
          (else
           (let ((new-status
                  (with-catch
                   (lambda (e)
                     (cond
                       ((nounset-exception? e)
                        ;; In interactive mode, nounset only aborts current line
                        (if interactive?
                          (nounset-exception-status e)
                          (raise e)))
                       ((errexit-exception? e)
                        (errexit-exception-status e))
                       ((break-exception? e) (raise e))
                       ((continue-exception? e) (raise e))
                       ((subshell-exit-exception? e) (raise e))
                       ((return-exception? e) (raise e))
                       (else
                        ;; Catch-all: print error and continue
                        (let ((msg (exception-message e)))
                          (fprintf (current-error-port) "gsh: ~a~n" msg)
                          ;; POSIX: syntax errors / unclosed bad substitution → exit code 2
                          (if (and (string? msg)
                                   (or (string-prefix? "parse error" msg)
                                       (string-prefix? "bad substitution: unclosed" msg)))
                            2 1)))))
                   (lambda ()
                     (execute-command cmd env)))))
             (env-set-last-status! env new-status)
             ;; Process pending signals between commands
             (process-pending-traps! env)
             ;; If errexit triggered, stop executing further commands
             (if (and (not (= new-status 0))
                      (env-option? env "errexit")
                      (not (*in-condition-context*)))
               new-status
               (loop new-status)))))))))

;; Process pending signals and execute trap commands
;; Lightweight version for script.ss (avoids circular import with main.ss)
;; Traps execute with $? isolated — they don't affect the main script's $?
(def (process-pending-traps! env)
  (let ((signals (pending-signals!)))
    (for-each
     (lambda (sig-name)
       (cond
         ((string=? sig-name "CHLD")
          (job-update-status!)
          (job-notify!))
         (else #!void))
       (let ((action (trap-get sig-name)))
         (when (and action (string? action))
           ;; Save and restore $? so trap doesn't affect main flow
           (let ((saved-status (shell-environment-last-status env))
                 (exec-fn (*execute-input*)))
             (when exec-fn
               (exec-fn action env))
             (env-set-last-status! env saved-status)))))
     signals)))

;;; --- Helpers ---

(def (strip-shebang content)
  ;; Remove #! line if present
  (if (and (>= (string-length content) 2)
           (char=? (string-ref content 0) #\#)
           (char=? (string-ref content 1) #\!))
    ;; Find end of first line
    (let loop ((i 0))
      (cond
        ((>= i (string-length content)) "")
        ((char=? (string-ref content i) #\newline)
         (substring content (+ i 1) (string-length content)))
        (else (loop (+ i 1)))))
    content))

(def (read-file-to-string filename)
  ;; Read entire file contents as a string
  (call-with-input-file filename
    (lambda (port)
      (let ((out (open-output-string)))
        (let loop ()
          (let ((ch (read-char port)))
            (unless (eof-object? ch)
              (write-char ch out)
              (loop))))
        (get-output-string out)))))
