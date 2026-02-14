;;; pipeline.ss — Pipe management for gsh
;;; Connects N commands in a pipeline using OS pipes.
;;;
;;; Strategy: for each command, dup2 the appropriate pipe fds onto real fds 0/1,
;;; launch the command with stdin/stdout-redirection: #f (inherit real fds),
;;; then immediately close the pipe fd in the parent so downstream commands
;;; see EOF when the producer finishes.

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/ffi
        :gsh/environment
        :gsh/expander
        :gsh/redirect
        :gsh/builtins
        :gsh/functions
        :gsh/util)

;; Parameters for pipeline stdin/stdout fds.
;; When set (non-#f), execute-external should dup2 these onto fd 0/1
;; before open-process so external commands in pipeline threads
;; inherit the pipe endpoints.
(def *pipeline-stdin-fd* (make-parameter #f))
(def *pipeline-stdout-fd* (make-parameter #f))

;;; --- Public interface ---

(def (execute-pipeline commands env execute-fn (pipe-types #f))
  (if (= (length commands) 1)
    (let ((status (execute-fn (car commands) env)))
      ;; Return list of exit codes for PIPESTATUS
      [status])
    (execute-piped-commands commands env execute-fn
                            (or pipe-types (make-list (- (length commands) 1) 'PIPE)))))

;;; --- Multi-command pipeline ---

(def (execute-piped-commands commands env execute-fn (pipe-types #f))
  (let ((ptypes (or pipe-types (make-list (- (length commands) 1) 'PIPE))))
  (parameterize ((*procsub-cleanups* []))
  (let* ((n (length commands))
         ;; Create n-1 pipes: each is [read-fd write-fd]
         (pipes (make-pipes (- n 1)))
         ;; Save original fds
         (saved-stdin-fd (ffi-dup 0))
         (saved-stdout-fd (ffi-dup 1))
         (saved-stderr-fd (ffi-dup 2))
         (saved-stdin-port (current-input-port))
         (saved-stdout-port (current-output-port))
         (saved-stderr-port (current-error-port)))
    (let ((procs
           (let loop ((cmds commands) (idx 0) (procs []))
             (if (null? cmds)
               (reverse procs)
               (let* ((is-first? (= idx 0))
                      (is-last? (null? (cdr cmds)))
                      ;; This command reads from the pipe connecting it to the previous
                      (in-pipe (and (not is-first?) (list-ref pipes (- idx 1))))
                      ;; This command writes to the pipe connecting it to the next
                      (out-pipe (and (not is-last?) (list-ref pipes idx)))
                      ;; Check if this connection uses |& (stderr also goes to pipe)
                      (pipeamp? (and (not is-last?)
                                     (< idx (length ptypes))
                                     (eq? (list-ref ptypes idx) 'PIPEAMP))))

                 ;; Redirect real fd 0 to pipe read-end (if not first command)
                 (when in-pipe
                   (ffi-dup2 (car in-pipe) 0))

                 ;; Redirect real fd 1 to pipe write-end (if not last command)
                 (when out-pipe
                   (ffi-dup2 (cadr out-pipe) 1))

                 ;; For |&, also redirect fd 2 to the pipe write-end
                 (when pipeamp?
                   (ffi-dup2 (cadr out-pipe) 2))

                 ;; Launch the command (it inherits current real fds 0/1)
                 (let ((proc (launch-pipeline-command
                              (car cmds) env execute-fn
                              (not is-first?) (not is-last?))))

                   ;; Restore real fd 0, 1, and 2 for the parent
                   (when in-pipe
                     (ffi-dup2 saved-stdin-fd 0))
                   (when out-pipe
                     (ffi-dup2 saved-stdout-fd 1))
                   (when pipeamp?
                     (ffi-dup2 saved-stderr-fd 2))

                   ;; CRITICAL: Close pipe ends in the parent after launching
                   ;; the command that uses them. This ensures:
                   ;; - The write-end is only held by the producing process
                   ;; - The read-end is only held by the consuming process
                   ;; Without this, readers never see EOF.
                   (when in-pipe
                     (ffi-close-fd (car in-pipe))   ;; close read-end
                     (set-car! in-pipe -1))          ;; mark as closed
                   (when out-pipe
                     (ffi-close-fd (cadr out-pipe))  ;; close write-end
                     (set-car! (cdr out-pipe) -1))   ;; mark as closed

                   (loop (cdr cmds) (+ idx 1) (cons proc procs))))))))
      ;; Close any remaining pipe fds (shouldn't be any, but be safe)
      (for-each
       (lambda (p)
         (when (>= (car p) 0)
           (with-catch void (lambda () (ffi-close-fd (car p)))))
         (when (>= (cadr p) 0)
           (with-catch void (lambda () (ffi-close-fd (cadr p))))))
       pipes)
      ;; Restore original fds and ports
      (ffi-dup2 saved-stdin-fd 0)
      (ffi-dup2 saved-stdout-fd 1)
      (ffi-dup2 saved-stderr-fd 2)
      (ffi-close-fd saved-stdin-fd)
      (ffi-close-fd saved-stdout-fd)
      (ffi-close-fd saved-stderr-fd)
      (current-input-port saved-stdin-port)
      (current-output-port saved-stdout-port)
      (current-error-port saved-stderr-port)
      ;; Wait for all processes/threads
      (let ((exit-codes (wait-for-all procs)))
        ;; Clean up any process substitution FIFOs
        (run-procsub-cleanups!)
        ;; Return the full list of exit codes (caller sets PIPESTATUS)
        exit-codes))))))

(def (make-pipes n)
  (let loop ((i 0) (pipes []))
    (if (>= i n)
      (reverse pipes)
      (let-values (((read-fd write-fd) (ffi-pipe-raw)))
        (loop (+ i 1) (cons [read-fd write-fd] pipes))))))

;;; --- Launch helpers ---

;; Launch a single command in the pipeline.
;; Real fds 0/1 are already set to pipe ends (or original fds).
(def (launch-pipeline-command cmd env execute-fn has-pipe-in? has-pipe-out?)
  (cond
    ((simple-command? cmd)
     (let* ((words (expand-words (simple-command-words cmd) env))
            (cmd-name (if (pair? words) (car words) #f))
            (redirections (simple-command-redirections cmd)))
       (cond
         ;; Check builtins FIRST — before external commands
         ((and cmd-name (builtin-lookup cmd-name))
          (launch-thread-piped cmd env execute-fn has-pipe-in? has-pipe-out?))
         ;; External command — inherits real fds 0/1 directly
         ((and cmd-name (which cmd-name))
          (let* ((path (which cmd-name))
                 (args (if (pair? words) (cdr words) []))
                 (redir-saved (if (pair? redirections)
                                (with-catch
                                 (lambda (e) #f)
                                 (lambda () (apply-redirections redirections env)))
                                []))
                 (proc (open-process
                        [path: path
                         arguments: args
                         environment: (env-exported-alist env)
                         stdin-redirection: #f
                         stdout-redirection: #f
                         stderr-redirection: #f])))
            ;; Restore redirections in parent (child already inherited the fds)
            (when (pair? redir-saved)
              (restore-redirections redir-saved))
            proc))
         ;; Shell function or unknown — run in thread
         (else
          (launch-thread-piped cmd env execute-fn has-pipe-in? has-pipe-out?)))))
    (else
     (launch-thread-piped cmd env execute-fn has-pipe-in? has-pipe-out?))))

;; Launch a builtin/function in a thread.
;; Create Gambit character ports wrapping current real fds 0/1.
;; Each pipeline component runs as a subshell (exit only exits the component).
(def (launch-thread-piped cmd env execute-fn has-pipe-in? has-pipe-out?)
  (let* ((exit-box (box 0))
         ;; Dup the current real fds so the thread has its own copy
         ;; (parent will restore 0/1 after this returns)
         (thread-in-fd (if has-pipe-in? (ffi-dup 0) #f))
         (thread-out-fd (if has-pipe-out? (ffi-dup 1) #f)))
    (let ((t (spawn
              (lambda ()
                (let ((in-port (if thread-in-fd
                                (open-input-file
                                 (string-append "/dev/fd/" (number->string thread-in-fd)))
                                (current-input-port)))
                      (out-port (if thread-out-fd
                                 (open-output-file
                                  (string-append "/dev/fd/" (number->string thread-out-fd)))
                                 (current-output-port))))
                  (parameterize ((current-input-port in-port)
                                 (current-output-port out-port)
                                 (*in-subshell* #t)
                                 (*pipeline-stdin-fd* thread-in-fd)
                                 (*pipeline-stdout-fd* thread-out-fd))
                    (let ((status (with-catch
                                   (lambda (e)
                                     (cond
                                       ((subshell-exit-exception? e)
                                        (subshell-exit-exception-status e))
                                       ((errexit-exception? e)
                                        (errexit-exception-status e))
                                       (else (raise e))))
                                   (lambda ()
                                     (execute-fn cmd env)))))
                      (set-box! exit-box status)
                      (when thread-out-fd
                        (force-output out-port)
                        (close-port out-port)
                        (ffi-close-fd thread-out-fd))
                      (when thread-in-fd
                        (close-port in-port)
                        (ffi-close-fd thread-in-fd)))))))))
      (list 'thread t exit-box))))

;; Wait for all processes/threads to complete
(def (wait-for-all procs)
  (map
   (lambda (proc)
     (cond
       ((port? proc)
        (let ((raw-status (process-status proc)))
          (close-port proc)
          (status->exit-code raw-status)))
       ((and (list? proc) (eq? (car proc) 'thread))
        ;; Catch exceptions from pipeline threads (e.g. subshell-exit-exception
        ;; from 'exit N' in a brace group) and use the exit code from the box
        (with-catch
         (lambda (e)
           (cond
             ((subshell-exit-exception? e) (subshell-exit-exception-status e))
             ((errexit-exception? e) (errexit-exception-status e))
             (else (unbox (caddr proc)))))
         (lambda ()
           (thread-join! (cadr proc))
           (unbox (caddr proc)))))
       (else 0)))
   procs))

;;; --- Helpers ---

(def (last-elem lst)
  (if (null? (cdr lst)) (car lst) (last-elem (cdr lst))))

(def (void) #!void)
