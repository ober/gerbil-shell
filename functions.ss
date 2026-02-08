;;; functions.ss — Shell functions and aliases for gsh

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/environment)

;;; --- Shell functions ---

(defstruct shell-function (name body redirections) transparent: #t)

;; Define a shell function
(def (function-define! env name body (redirections []))
  (hash-put! (shell-environment-functions env) name
             (make-shell-function name body redirections)))

;; Look up a function by name
(def (function-lookup env name)
  (hash-get (shell-environment-functions env) name))

;; Unset a function
(def (function-unset! env name)
  (hash-remove! (shell-environment-functions env) name))

;; List all function names
(def (function-list env)
  (hash-keys (shell-environment-functions env)))

;; Call a function with arguments
;; execute-fn is the executor callback to avoid circular dependency
;; Returns exit status
(def (function-call func args env execute-fn)
  (let* ((child-env (env-push-scope env))
         ;; Set positional parameters
         (_ (env-set-positional! child-env args))
         ;; Set FUNCNAME
         (_ (env-set! child-env "FUNCNAME" (shell-function-name func))))
    ;; Execute the function body
    ;; break/continue/return are handled by the caller
    (with-catch
     (lambda (e)
       (if (return-exception? e)
         (return-exception-status e)
         (raise e)))
     (lambda ()
       (let ((status (execute-fn (shell-function-body func) child-env)))
         ;; Copy last-status back to parent
         (env-set-last-status! env status)
         status)))))

;;; --- Return exception ---
;; Used to implement 'return' from functions

(defstruct return-exception (status) transparent: #t)

(def (shell-return! (status 0))
  (raise (make-return-exception status)))

;;; --- Break/Continue exceptions ---
;; Used to implement 'break' and 'continue' in loops

(defstruct break-exception (levels) transparent: #t)
(defstruct continue-exception (levels) transparent: #t)

(def (shell-break! (levels 1))
  (raise (make-break-exception levels)))

(def (shell-continue! (levels 1))
  (raise (make-continue-exception levels)))

;;; --- Errexit exception ---
;; Raised when set -e is active and a command fails outside a condition context

(defstruct errexit-exception (status) transparent: #t)

;;; --- Nounset exception ---
;; Raised when set -u is active and an unbound variable is referenced

(defstruct nounset-exception (status) transparent: #t)

;;; --- Subshell exit exception ---
;; Raised by `exit` builtin when running inside a subshell

(defstruct subshell-exit-exception (status) transparent: #t)

;;; --- Aliases ---

;; Set an alias
(def (alias-set! env name value)
  (hash-put! (shell-environment-aliases env) name value))

;; Get an alias value
(def (alias-get env name)
  (hash-get (shell-environment-aliases env) name))

;; Remove an alias
(def (alias-unset! env name)
  (hash-remove! (shell-environment-aliases env) name))

;; Remove all aliases
(def (alias-clear! env)
  ;; Replace with new empty table
  (hash-for-each
   (lambda (k v) (hash-remove! (shell-environment-aliases env) k))
   (shell-environment-aliases env)))

;; List all aliases as alist
(def (alias-list env)
  (hash->list (shell-environment-aliases env)))

;; Expand aliases in a word (first word of simple command)
;; Returns expanded string or #f if no alias
(def (alias-expand env word)
  (let ((val (alias-get env word)))
    (if val
      ;; If alias ends with space, the next word should also be checked
      val
      #f)))

;; Check if alias value ends with space (triggers next-word expansion)
(def (alias-continues? value)
  (and (> (string-length value) 0)
       (char=? (string-ref value (- (string-length value) 1)) #\space)))
