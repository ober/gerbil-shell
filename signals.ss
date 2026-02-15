;;; signals.ss — Signal handling and traps for gsh

(export #t)
(import :std/sugar
        :std/format
        :std/sort
        :std/os/signal
        :std/os/signal-handler
        :gsh/ffi
        :gsh/util)

;;; --- Trap table ---
;; Maps signal names to actions:
;;   string -> command to execute
;;   'ignore -> ignore the signal
;;   'default -> restore default behavior

(defstruct trap-entry (signal action) transparent: #t)

;; Global trap table (managed by the shell environment)
(def *trap-table* (make-hash-table))

;; Well-known signal name -> number mapping
(def *signal-names*
  (hash
   ("HUP"    SIGHUP)
   ("INT"    SIGINT)
   ("QUIT"   SIGQUIT)
   ("ILL"    SIGILL)
   ("TRAP"   SIGTRAP)
   ("ABRT"   SIGABRT)
   ("FPE"    SIGFPE)
   ("KILL"   SIGKILL)
   ("SEGV"   SIGSEGV)
   ("PIPE"   SIGPIPE)
   ("ALRM"   SIGALRM)
   ("TERM"   SIGTERM)
   ("USR1"   SIGUSR1)
   ("USR2"   SIGUSR2)
   ("CHLD"   SIGCHLD)
   ("CONT"   SIGCONT)
   ("STOP"   SIGSTOP)
   ("TSTP"   SIGTSTP)
   ("TTIN"   SIGTTIN)
   ("TTOU"   SIGTTOU)
   ("WINCH"  SIGWINCH)
   ("URG"    SIGURG)
   ("IO"     SIGIO)
   ("XCPU"   SIGXCPU)
   ("XFSZ"   SIGXFSZ)
   ("VTALRM" SIGVTALRM)
   ("PROF"   SIGPROF)
   ("SYS"    SIGSYS)))

;; Pseudo-signals (not real OS signals)
(def *pseudo-signals* '("EXIT" "DEBUG" "RETURN" "ERR"))

;; Reverse mapping: signal number -> short name
(def *signal-number-to-name* (make-hash-table))
(hash-for-each (lambda (name num) (hash-put! *signal-number-to-name* num name)) *signal-names*)

;; Normalize a signal argument to canonical short name (e.g. "INT", "EXIT")
;; Handles: SIGINT -> INT, INT -> INT, 2 -> INT, 0 -> EXIT, etc.
(def (normalize-signal-arg arg)
  (let ((uarg (string-upcase arg)))
    ;; Strip SIG prefix
    (let ((stripped (if (and (> (string-length uarg) 3)
                             (string=? (substring uarg 0 3) "SIG"))
                     (substring uarg 3 (string-length uarg))
                     uarg)))
      ;; Check if it's a number
      (let ((num (string->number stripped)))
        (cond
          ;; Signal number: 0 = EXIT, others look up
          ((and num (= num 0)) "EXIT")
          ((and num (hash-get *signal-number-to-name* num))
           => (lambda (name) name))
          ;; Valid signal number but no name in table — return as-is
          ((and num (integer? num) (> num 0) (<= num 64))
           (number->string num))
          ;; Known signal name
          ((hash-get *signal-names* stripped) stripped)
          ;; Pseudo-signal
          ((member stripped *pseudo-signals*) stripped)
          ;; Unknown
          (else #f))))))

;; Get canonical display name for trap -p output
;; Pseudo signals: EXIT, DEBUG, RETURN, ERR (no SIG prefix)
;; Real signals: SIGHUP, SIGINT, SIGTERM, etc.
(def (signal-display-name short-name)
  (if (member short-name *pseudo-signals*)
    short-name
    (string-append "SIG" short-name)))

;; Convert signal name to number (or #f for pseudo/unknown)
(def (signal-name->number name)
  (let ((uname (string-upcase name)))
    ;; Strip SIG prefix if present
    (let ((stripped (if (and (> (string-length uname) 3)
                             (string=? (substring uname 0 3) "SIG"))
                     (substring uname 3 (string-length uname))
                     uname)))
      (hash-get *signal-names* stripped))))

;; Convert signal number to name
(def (signal-number->name num)
  (let ((result #f))
    (hash-for-each
     (lambda (name sig)
       (when (= sig num)
         (set! result name)))
     *signal-names*)
    result))

;; Human-readable signal descriptions (matching strsignal/bash output)
(def *signal-descriptions*
  (hash
   ("HUP" "Hangup") ("INT" "Interrupt") ("QUIT" "Quit")
   ("ILL" "Illegal instruction") ("TRAP" "Trace/breakpoint trap")
   ("ABRT" "Aborted") ("FPE" "Floating point exception")
   ("KILL" "Killed") ("SEGV" "Segmentation fault")
   ("PIPE" "Broken pipe") ("ALRM" "Alarm clock") ("TERM" "Terminated")
   ("USR1" "User defined signal 1") ("USR2" "User defined signal 2")
   ("CHLD" "Child exited") ("CONT" "Continued") ("STOP" "Stopped (signal)")
   ("TSTP" "Stopped") ("TTIN" "Stopped (tty input)")
   ("TTOU" "Stopped (tty output)")))

;; Get human-readable description for a signal number
(def (signal-description signum)
  (let ((name (signal-number->name signum)))
    (and name (hash-get *signal-descriptions* name))))

;;; --- Trap operations ---

;; Set a trap for a signal
;; signal-name should be a normalized short name (e.g. "INT", "EXIT")
;; action: string (command), "" or 'ignore (ignore), 'default or #f (reset)
(def (trap-set! signal-name action)
  (let ((uname (or (normalize-signal-arg signal-name)
                   (string-upcase signal-name))))
    (cond
      ;; Reset to default
      ((or (eq? action 'default) (not action) (string=? (if (string? action) action "") "-"))
       (hash-remove! *trap-table* uname)
       (let ((signum (signal-name->number uname)))
         (when signum
           (with-catch (lambda (e) #!void) ;; ignore error if no handler installed
             (lambda () (remove-signal-handler! signum))))))
      ;; Ignore signal
      ((or (eq? action 'ignore) (and (string? action) (string=? action "")))
       (hash-put! *trap-table* uname 'ignore)
       (let ((signum (signal-name->number uname)))
         (when signum
           (add-signal-handler! signum (lambda () #!void)))))
      ;; Set command handler
      ((string? action)
       (hash-put! *trap-table* uname action)
       ;; For real signals, install a handler that flags for later execution
       (let ((signum (signal-name->number uname)))
         (when signum
           (add-signal-handler! signum
             (lambda ()
               ;; The actual command execution happens in the main loop
               ;; Here we just record that the signal was received
               (set! *pending-signals* (cons uname *pending-signals*)))))))
      (else
       (error (format "trap: invalid action: ~a" action))))))

;; Get the trap action for a signal
(def (trap-get signal-name)
  (let ((uname (or (normalize-signal-arg signal-name)
                   (string-upcase signal-name))))
    (hash-get *trap-table* uname)))

;; List all traps as alist of (signal-name . action)
(def (trap-list)
  (hash->list *trap-table*))

;;; --- Pending signal queue ---

(def *pending-signals* [])

;; Check and clear pending signals, return list of signal names
(def (pending-signals!)
  (let ((pending *pending-signals*))
    (set! *pending-signals* [])
    (reverse pending)))

;;; --- Default signal setup for interactive shell ---

(def (setup-default-signal-handlers!)
  ;; SIGINT: interrupt current command
  (add-signal-handler! SIGINT
    (lambda ()
      (set! *pending-signals* (cons "INT" *pending-signals*))))
  ;; SIGQUIT: ignore in interactive mode
  (add-signal-handler! SIGQUIT (lambda () #!void))
  ;; SIGTERM: flag for exit
  (add-signal-handler! SIGTERM
    (lambda ()
      (set! *pending-signals* (cons "TERM" *pending-signals*))))
  ;; SIGTSTP: ignore for the shell itself (children get it)
  (add-signal-handler! SIGTSTP (lambda () #!void))
  ;; SIGPIPE: ignore (let write fail with error)
  (add-signal-handler! SIGPIPE (lambda () #!void))
  ;; SIGWINCH: record for terminal resize
  (add-signal-handler! SIGWINCH
    (lambda ()
      (set! *pending-signals* (cons "WINCH" *pending-signals*))))
  ;; SIGCHLD: record for job status updates
  (add-signal-handler! SIGCHLD
    (lambda ()
      (set! *pending-signals* (cons "CHLD" *pending-signals*)))))

;;; --- Signal context for command execution ---

;; Run a thunk with appropriate signal handling for foreground command execution
(def (with-signal-context thunk)
  ;; Clear pending signals before running
  (set! *pending-signals* [])
  (thunk))

;;; --- Utility ---

;; List all known signal names
(def (signal-name-list)
  (sort! (hash-keys *signal-names*) string<?))
