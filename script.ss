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
        :gsh/executor)

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
       (fprintf (current-error-port) "gsh: ~a: ~a~n" filename (exception-message e))
       1)
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
       (fprintf (current-error-port) "gsh: ~a: ~a~n" filename (exception-message e))
       1)
     (lambda ()
       (let* ((content (read-file-to-string filename))
              (script-content (strip-shebang content)))
         (execute-string script-content env))))))

;;; --- String execution ---

;; Parse and execute a string of shell commands.
;; Used by both execute-script and source-file!
(def (execute-string input env)
  (let ((lexer (make-shell-lexer input)))
    (let loop ((status 0))
      (let ((cmd (with-catch
                  (lambda (e)
                    (fprintf (current-error-port) "gsh: syntax error: ~a~n"
                             (exception-message e))
                    'error)
                  (lambda ()
                    (parse-complete-command lexer)))))
        (cond
          ((eq? cmd 'error) 2)  ;; syntax error
          ((not cmd) status)     ;; end of input
          (else
           (let ((new-status
                  (with-catch
                   (lambda (e)
                     (if (errexit-exception? e)
                       (errexit-exception-status e)
                       (raise e)))
                   (lambda ()
                     (execute-command cmd env)))))
             ;; If errexit triggered, stop executing further commands
             (if (and (not (= new-status 0))
                      (env-option? env "errexit")
                      (not (*in-condition-context*)))
               new-status
               (loop new-status)))))))))

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
