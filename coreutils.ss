;;; -*- Gerbil -*-
;;; gerbil-coreutils integration — register coreutils commands as shell builtins
;;;
;;; Commands are registered as "fallback builtins": they only activate when no
;;; external command with the same name exists in PATH. This preserves system
;;; command behavior on full systems while providing builtins in minimal environments
;;; (e.g., the static binary without /usr/bin).
;;;
;;; The coreutils main functions call `exit` on errors (via `die` and `call-with-getopt`).
;;; Since Gambit's `exit` hard-terminates the process, we intercept it by temporarily
;;; overriding the global `exit` binding with a continuation escape during each invocation.

(export register-coreutils!)

(import :gsh/registry
        ;; Each coreutils module exports `main`. Use rename-in to avoid conflicts
        ;; and ensure runtime loading (only-in without names doesn't link modules).
        (rename-in :gerbil-coreutils/basename (main cu-basename))
        (rename-in :gerbil-coreutils/dirname  (main cu-dirname))
        (rename-in :gerbil-coreutils/cat      (main cu-cat))
        (rename-in :gerbil-coreutils/head     (main cu-head))
        (rename-in :gerbil-coreutils/tail     (main cu-tail))
        (rename-in :gerbil-coreutils/tac      (main cu-tac))
        (rename-in :gerbil-coreutils/tee      (main cu-tee))
        (rename-in :gerbil-coreutils/wc       (main cu-wc))
        (rename-in :gerbil-coreutils/nl       (main cu-nl))
        (rename-in :gerbil-coreutils/sort     (main cu-sort))
        (rename-in :gerbil-coreutils/uniq     (main cu-uniq))
        (rename-in :gerbil-coreutils/cut      (main cu-cut))
        (rename-in :gerbil-coreutils/paste    (main cu-paste))
        (rename-in :gerbil-coreutils/join     (main cu-join))
        (rename-in :gerbil-coreutils/comm     (main cu-comm))
        (rename-in :gerbil-coreutils/tr       (main cu-tr))
        (rename-in :gerbil-coreutils/fold     (main cu-fold))
        (rename-in :gerbil-coreutils/expand   (main cu-expand))
        (rename-in :gerbil-coreutils/unexpand (main cu-unexpand))
        (rename-in :gerbil-coreutils/numfmt   (main cu-numfmt))
        (rename-in :gerbil-coreutils/link     (main cu-link))
        (rename-in :gerbil-coreutils/unlink   (main cu-unlink))
        (rename-in :gerbil-coreutils/sleep    (main cu-sleep))
        (rename-in :gerbil-coreutils/printenv (main cu-printenv))
        (rename-in :gerbil-coreutils/whoami   (main cu-whoami))
        (rename-in :gerbil-coreutils/logname  (main cu-logname))
        (rename-in :gerbil-coreutils/yes      (main cu-yes)))

;;; Check if a command name exists as an external command in PATH.
(def (command-in-path? name)
  (let ((path (getenv "PATH" "")))
    (let loop ((dirs (string-split path #\:)))
      (and (pair? dirs)
           (or (file-exists? (string-append (car dirs) "/" name))
               (loop (cdr dirs)))))))

;;; Wrap a coreutils `main` function as a shell builtin handler.
;;; The handler signature is (lambda (args env) ...) returning an integer exit status.
;;;
;;; Coreutils main functions are (def (main . args)) where args does NOT include
;;; the program name (that's set via parameterize on program-name inside each main).
;;; They may call `exit` on errors, so we intercept it via set! + call/cc + dynamic-wind.
(def (make-coreutils-builtin name main-fn)
  (lambda (args env)
    (let ((saved-exit #f))
      (let ((status
             (call-with-current-continuation
              (lambda (k)
                (dynamic-wind
                  (lambda () (set! saved-exit exit) (set! exit (lambda (code) (k code))))
                  (lambda ()
                    (with-exception-catcher
                     (lambda (e)
                       (with-exception-catcher
                        (lambda (_) (void))
                        (lambda ()
                          (display-exception e (current-error-port))))
                       1)
                     (lambda ()
                       (apply main-fn args)
                       0)))
                  (lambda () (set! exit saved-exit)))))))
        (if (fixnum? status) status 0)))))

;;; Register all coreutils commands as shell builtins.
;;; Only registers commands that:
;;;   1. gsh doesn't already implement natively (true, false, echo, pwd, printf, test, read)
;;;   2. Don't exist as external commands in PATH (fallback behavior)
(def (register-coreutils!)
  (for-each
   (lambda (entry)
     (let ((name (car entry))
           (main-fn (cdr entry)))
       (unless (command-in-path? name)
         (builtin-register! name (make-coreutils-builtin name main-fn)))))
   `(;; path utilities
     ("basename"  . ,cu-basename)
     ("dirname"   . ,cu-dirname)
     ;; file viewing
     ("cat"       . ,cu-cat)
     ("head"      . ,cu-head)
     ("tail"      . ,cu-tail)
     ("tac"       . ,cu-tac)
     ("tee"       . ,cu-tee)
     ;; counting / numbering
     ("wc"        . ,cu-wc)
     ("nl"        . ,cu-nl)
     ;; sorting
     ("sort"      . ,cu-sort)
     ("uniq"      . ,cu-uniq)
     ;; field processing
     ("cut"       . ,cu-cut)
     ("paste"     . ,cu-paste)
     ("join"      . ,cu-join)
     ("comm"      . ,cu-comm)
     ;; text transformation
     ("tr"        . ,cu-tr)
     ("fold"      . ,cu-fold)
     ("expand"    . ,cu-expand)
     ("unexpand"  . ,cu-unexpand)
     ("numfmt"    . ,cu-numfmt)
     ;; filesystem
     ("link"      . ,cu-link)
     ("unlink"    . ,cu-unlink)
     ;; timing
     ("sleep"     . ,cu-sleep)
     ;; environment
     ("printenv"  . ,cu-printenv)
     ("whoami"    . ,cu-whoami)
     ("logname"   . ,cu-logname)
     ;; output
     ("yes"       . ,cu-yes))))
