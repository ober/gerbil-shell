;;; executor.ss — Command dispatch and execution for gsh
;;; Walks the AST and executes commands.

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/environment
        :gsh/expander
        :gsh/builtins
        :gsh/functions
        :gsh/pipeline
        :gsh/redirect
        :gsh/control
        :gsh/jobs
        :gsh/util
        :gsh/ffi)

;;; --- Public interface ---

;; Execute a command AST node, return exit status (0-255)
(def (execute-command cmd env)
  (cond
    ((not cmd) 0)
    ((simple-command? cmd) (execute-simple-command cmd env))
    ((ast-pipeline? cmd) (execute-ast-pipeline cmd env))
    ((and-or-list? cmd) (execute-and-or cmd env))
    ((command-list? cmd) (execute-command-list cmd env))
    ((subshell? cmd) (execute-subshell cmd env))
    ((brace-group? cmd) (execute-brace-group cmd env))
    ((if-command? cmd) (execute-if cmd env execute-command))
    ((for-command? cmd) (execute-for cmd env execute-command))
    ((while-command? cmd) (execute-while cmd env execute-command))
    ((until-command? cmd) (execute-until cmd env execute-command))
    ((case-command? cmd) (execute-case cmd env execute-command))
    ((select-command? cmd) (execute-select cmd env execute-command))
    ((function-def? cmd) (execute-function-def cmd env))
    (else
     (fprintf (current-error-port) "gsh: unknown command type~n")
     1)))

;;; --- Assignment helper ---

(def (apply-assignment! asgn env)
  (let ((name (assignment-name asgn))
        (val (expand-word-nosplit (assignment-value asgn) env))
        (op (assignment-op asgn)))
    (if (eq? op '+=)
      (let ((old (or (env-get env name) "")))
        (env-set! env name (string-append old val)))
      (env-set! env name val))))

;;; --- Simple command execution ---

(def (execute-simple-command cmd env)
  (let* ((assignments (simple-command-assignments cmd))
         (raw-words (simple-command-words cmd))
         (redirections (simple-command-redirections cmd)))
    ;; If no command words, just apply assignments
    (if (null? raw-words)
      (begin
        (for-each (lambda (asgn) (apply-assignment! asgn env)) assignments)
        0)
      ;; Expand words
      (let* ((expanded (expand-words raw-words env))
             (cmd-name (if (pair? expanded) (car expanded) #f))
             (args (if (pair? expanded) (cdr expanded) [])))
        (unless cmd-name
          (env-set-last-status! env 0)
          (return-status env 0))
        ;; Apply xtrace if enabled
        (when (env-option? env "xtrace")
          (let ((ps4 (or (env-get env "PS4") "+ ")))
            (fprintf (current-error-port) "~a~a~n"
                     ps4 (string-join-words expanded))))
        ;; Apply command-scoped assignments
        (let ((temp-env (if (pair? assignments)
                          (let ((child (env-push-scope env)))
                            (for-each
                             (lambda (asgn) (apply-assignment! asgn child))
                             assignments)
                            child)
                          env)))
          ;; Apply redirections
          (let ((saved (apply-redirections redirections temp-env)))
            (let ((status
                   (with-catch
                    (lambda (e)
                      (cond
                        ((return-exception? e) (return-exception-status e))
                        ((break-exception? e) (raise e))
                        ((continue-exception? e) (raise e))
                        ((errexit-exception? e) (raise e))
                        ((subshell-exit-exception? e) (raise e))
                        (else
                         (fprintf (current-error-port) "gsh: ~a~n" (exception-message e))
                         1)))
                    (lambda ()
                      (cond
                        ;; Check for shell function
                        ((function-lookup env cmd-name)
                         => (lambda (func)
                              (function-call func args temp-env execute-command)))
                        ;; Check for builtin
                        ((builtin-lookup cmd-name)
                         => (lambda (handler)
                              (handler args temp-env)))
                        ;; External command
                        (else
                         (execute-external cmd-name args temp-env)))))))
              ;; Restore redirections
              (restore-redirections saved)
              ;; Set last status
              (env-set-last-status! env status)
              ;; Check errexit
              (check-errexit! env status)
              status)))))))

;;; --- External command execution ---

(def (execute-external cmd-name args env)
  (let ((path (which cmd-name)))
    (if (not path)
      (begin
        (fprintf (current-error-port) "gsh: ~a: command not found~n" cmd-name)
        127)
      (with-catch
       (lambda (e)
         (fprintf (current-error-port) "gsh: ~a: ~a~n" cmd-name (exception-message e))
         126)
       (lambda ()
         (let* ((proc (open-process
                       [path: path
                        arguments: args
                        environment: (env-exported-alist env)
                        stdin-redirection: #f
                        stdout-redirection: #f
                        stderr-redirection: #f]))
                (pid (process-pid proc)))
           (let-values (((exit-code stopped?)
                         (wait-for-foreground-process pid proc)))
             (if stopped?
               ;; Ctrl-Z: add to job table as stopped
               (let ((cmd-text (string-join-words (cons cmd-name args)))
                     (job (job-table-add! [(cons pid proc)] "" pid)))
                 (set! (job-command-text job) cmd-text)
                 (set! (job-status job) 'stopped)
                 (for-each (lambda (p) (set! (job-process-status p) 'stopped))
                           (job-processes job))
                 (fprintf (current-error-port) "~n[~a]+  Stopped                 ~a~n"
                          (job-id job) cmd-text)
                 (+ 128 20))  ;; 148 = 128 + SIGTSTP(20)
               ;; Normal exit
               (begin
                 (close-port proc)
                 exit-code)))))))))

;;; --- Pipeline execution ---

(def (execute-ast-pipeline cmd env)
  (let* ((commands (ast-pipeline-commands cmd))
         (bang? (ast-pipeline-bang? cmd))
         ;; ! prefix suppresses errexit
         (status (if bang?
                   (parameterize ((*in-condition-context* #t))
                     (execute-pipeline commands env execute-command))
                   (execute-pipeline commands env execute-command))))
    (let ((final (if bang? (if (= status 0) 1 0) status)))
      (env-set-last-status! env final)
      final)))

;;; --- And-or list execution ---

(def (execute-and-or cmd env)
  (let* ((rest-items (and-or-list-rest cmd))
         ;; If there are && or || operators, the first command is in condition context
         (first-in-condition? (pair? rest-items))
         (status (if first-in-condition?
                   (parameterize ((*in-condition-context* #t))
                     (execute-command (and-or-list-first cmd) env))
                   (execute-command (and-or-list-first cmd) env))))
    (let loop ((rest rest-items) (status status))
      (if (null? rest)
        (begin (env-set-last-status! env status) status)
        (let* ((item (car rest))
               (op (car item))
               (pipeline (cdr item))
               ;; All but the last element in && / || chain are condition context
               (is-condition? (pair? (cdr rest))))
          (cond
            ;; && — run next only if status = 0
            ((and (eq? op 'and) (= status 0))
             (if is-condition?
               (loop (cdr rest) (parameterize ((*in-condition-context* #t))
                                  (execute-command pipeline env)))
               (loop (cdr rest) (execute-command pipeline env))))
            ;; || — run next only if status != 0
            ((and (eq? op 'or) (not (= status 0)))
             (if is-condition?
               (loop (cdr rest) (parameterize ((*in-condition-context* #t))
                                  (execute-command pipeline env)))
               (loop (cdr rest) (execute-command pipeline env))))
            (else
             (loop (cdr rest) status))))))))

;;; --- Command list execution ---

(def (execute-command-list cmd env)
  (let loop ((items (command-list-items cmd)) (status 0))
    (if (null? items)
      (begin (env-set-last-status! env status) status)
      (let* ((item (car items))
             (mode (car item))
             (command (cdr item)))
        (case mode
          ((sequential)
           (let ((new-status (execute-command command env)))
             (loop (cdr items) new-status)))
          ((background)
           ;; Launch command in background
           (let ((result (launch-background command env)))
             (env-set-last-bg-pid! env (car result))
             (let ((job (job-table-add! (cdr result)
                                        (or (ast->command-text command) "&")
                                        (car result))))
               (fprintf (current-error-port) "[~a] ~a~n"
                        (job-id job) (car result)))
             (loop (cdr items) 0)))
          (else
           (loop (cdr items) (execute-command command env))))))))

;;; --- Compound commands ---

(def (execute-subshell cmd env)
  ;; Execute in a fully cloned environment — variable changes, cd, etc.
  ;; don't affect the parent.
  (let ((child-env (env-clone env))
        (saved-cwd (current-directory)))
    (let ((status (with-catch
                   (lambda (e)
                     (cond
                       ((subshell-exit-exception? e) (subshell-exit-exception-status e))
                       ((errexit-exception? e) (errexit-exception-status e))
                       (else (raise e))))
                   (lambda ()
                     (parameterize ((*in-subshell* #t))
                       (execute-command (subshell-body cmd) child-env))))))
      ;; Restore parent's working directory
      (current-directory saved-cwd)
      (env-set-last-status! env status)
      status)))

(def (execute-brace-group cmd env)
  (execute-command (brace-group-body cmd) env))

(def (execute-function-def cmd env)
  (function-define! env
                    (function-def-name cmd)
                    (function-def-body cmd)
                    (function-def-redirections cmd))
  0)

;;; --- Errexit checking ---

;; Check errexit after a command completes.
;; Raises errexit-exception if set -e is active, status is non-zero,
;; and we're not in a condition context.
(def (check-errexit! env status)
  (when (and (not (= status 0))
             (env-option? env "errexit")
             (not (*in-condition-context*)))
    (raise (make-errexit-exception status))))

;;; --- Helpers ---

(def (return-status env status)
  (env-set-last-status! env status)
  status)

(def (string-join-words words)
  (if (null? words) ""
      (let loop ((rest (cdr words)) (acc (car words)))
        (if (null? rest) acc
            (loop (cdr rest) (string-append acc " " (car rest)))))))

;;; --- Background execution ---

;; Launch a command in the background.
;; Returns (pid . process-list) where process-list is for job-table-add!
;; For external commands: launches via open-process, returns real PID
;; For builtins/functions/compound: launches in a thread, returns 0
(def (launch-background cmd env)
  (if (simple-command? cmd)
    (launch-background-simple cmd env)
    ;; Compound command: run in thread
    (begin
      (spawn (lambda () (execute-command cmd env)))
      (cons 0 []))))

(def (launch-background-simple cmd env)
  (let* ((raw-words (simple-command-words cmd))
         (redirections (simple-command-redirections cmd))
         (assignments (simple-command-assignments cmd))
         (expanded (expand-words raw-words env))
         (cmd-name (if (pair? expanded) (car expanded) #f))
         (args (if (pair? expanded) (cdr expanded) [])))
    (if (and cmd-name
             (not (function-lookup env cmd-name))
             (not (builtin-lookup cmd-name)))
      ;; External command: launch without waiting
      (let ((path (which cmd-name)))
        (if path
          (let* ((temp-env (if (pair? assignments)
                             (let ((child (env-push-scope env)))
                               (for-each
                                (lambda (asgn) (apply-assignment! asgn child))
                                assignments)
                               child)
                             env))
                 (proc (open-process
                        [path: path
                         arguments: args
                         environment: (env-exported-alist temp-env)
                         stdin-redirection: #f
                         stdout-redirection: #f
                         stderr-redirection: #f]))
                 (pid (process-pid proc)))
            ;; Put background process in its own process group
            ;; so it doesn't receive terminal signals (SIGINT, SIGTSTP)
            (with-catch (lambda (e) #!void) ;; ignore if setpgid fails (e.g. child already exited)
              (lambda () (ffi-setpgid pid pid)))
            (cons pid [(cons pid proc)]))
          ;; Command not found — still launch in thread for error message
          (begin
            (spawn (lambda () (execute-command cmd env)))
            (cons 0 []))))
      ;; Builtin or function: run in thread
      (begin
        (spawn (lambda () (execute-command cmd env)))
        (cons 0 [])))))

;; Extract command text from an AST for display in job listing
(def (ast->command-text cmd)
  (cond
    ((simple-command? cmd)
     (let ((words (simple-command-words cmd)))
       (if (pair? words)
         (string-join-words words)
         #f)))
    ((ast-pipeline? cmd)
     (let ((cmds (ast-pipeline-commands cmd)))
       (string-join-words
        (map (lambda (c) (or (ast->command-text c) "?")) cmds))))
    (else #f)))
