;;; environment.ss — Shell variables & environment for gsh

(export #t)
(import :std/sugar
        :std/iter
        :std/format
        :std/misc/hash
        :gsh/ffi)

;;; --- Callback parameter for execute-input ---
;;; Breaks circular dependency: builtins.ss and expander.ss need to call
;;; execute-input (defined in main.ss), but main.ss imports them.
;;; Solution: store it as a parameter, set it from main.ss at startup.
(def *execute-input* (make-parameter #f))

;;; --- Callback parameter for arithmetic evaluation ---
;;; Used by apply-var-attrs when integer attribute is set.
;;; Set from main.ss at startup to arith-eval.
(def *arith-eval-fn* (make-parameter #f))

;;; --- Condition context for errexit suppression ---
;;; When #t, errexit (set -e) does not trigger on command failure.
;;; Set to #t in: if-test, while/until-test, && / || LHS, ! prefix.
(def *in-condition-context* (make-parameter #f))

;;; --- Subshell context ---
;;; When #t, `exit` raises an exception instead of terminating the process.
(def *in-subshell* (make-parameter #f))

;;; --- Shell variable ---
;; Attributes: exported?, readonly?, local?, integer?, uppercase?, lowercase?, nameref?
(defstruct shell-var (value exported? readonly? local?
                      integer? uppercase? lowercase? nameref?) transparent: #t)

;;; --- Shell environment ---
(defclass shell-environment (vars        ;; hash-table: name -> shell-var
                             parent      ;; parent environment or #f
                             functions   ;; hash-table: name -> function-def
                             aliases     ;; hash-table: name -> string
                             options     ;; hash-table: option-name -> bool
                             shopts      ;; hash-table: shopt-name -> bool
                             positional  ;; vector of positional params ($1..$N)
                             last-status ;; $? - exit status of last command
                             last-bg-pid ;; $! - PID of last background process
                             shell-pid   ;; $$ - PID of the shell
                             shell-name  ;; $0
                             start-time  ;; for $SECONDS
                             cmd-number  ;; for \# in prompt
                             traps       ;; hash-table: signal-name -> action
                             dir-stack   ;; list of directories (pushd/popd)
                             )
  constructor: :init!
  transparent: #t)

(defmethod {:init! shell-environment}
  (lambda (self parent: (parent #f) name: (name "gsh"))
    (set! self.vars (make-hash-table))
    (set! self.parent parent)
    (set! self.functions (if parent (shell-environment-functions parent) (make-hash-table)))
    (set! self.aliases (if parent (shell-environment-aliases parent) (make-hash-table)))
    (set! self.options (if parent (shell-environment-options parent) (make-hash-table)))
    (set! self.shopts (if parent (shell-environment-shopts parent) (make-hash-table)))
    (set! self.positional (if parent (shell-environment-positional parent) (vector)))
    (set! self.last-status 0)
    (set! self.last-bg-pid 0)
    (set! self.shell-pid (if parent (shell-environment-shell-pid parent) (##os-getpid)))
    (set! self.shell-name name)
    (set! self.start-time (if parent (shell-environment-start-time parent) (time->seconds (current-time))))
    (set! self.cmd-number (if parent (shell-environment-cmd-number parent) 0))
    (set! self.traps (if parent (shell-environment-traps parent) (make-hash-table)))
    (set! self.dir-stack (if parent (shell-environment-dir-stack parent) []))))

;;; --- Variable operations ---

;; Get a variable's value, checking scope chain
(def (env-get env name)
  (cond
    ;; Special variables
    ((string=? name "?") (number->string (shell-environment-last-status env)))
    ((string=? name "$") (number->string (shell-environment-shell-pid env)))
    ((string=? name "!") (number->string (shell-environment-last-bg-pid env)))
    ((string=? name "#") (number->string (vector-length (shell-environment-positional env))))
    ((string=? name "0") (shell-environment-shell-name env))
    ((string=? name "-") (options->flag-string env))
    ((string=? name "_") (env-get-local env "_"))  ;; last arg of prev command
    ((string=? name "RANDOM") (number->string (random-integer 32768)))
    ((string=? name "SECONDS")
     (number->string (inexact->exact
                      (floor (- (time->seconds (current-time))
                                (shell-environment-start-time env))))))
    ((string=? name "LINENO") (or (env-get-local env "LINENO") "0"))
    ;; Positional parameters $1..$N
    ((positional-index name)
     => (lambda (idx)
          (let ((pos (shell-environment-positional env)))
            (if (and (> idx 0) (<= idx (vector-length pos)))
              (vector-ref pos (- idx 1))
              ""))))
    ;; Regular variable lookup
    (else
     (env-get-chain env name))))

;; Get from local scope only
(def (env-get-local env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (and var (shell-var-value var))))

;; Get from scope chain
(def (env-get-chain env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (if var
      (shell-var-value var)
      (let ((parent (shell-environment-parent env)))
        (if parent
          (env-get-chain parent name)
          ;; Fall back to OS environment
          (getenv name #f))))))

;; Apply variable attributes (integer, uppercase, lowercase) to a value
;; env is optional — when provided, arithmetic evaluation can reference variables
(def (apply-var-attrs var value (env #f))
  (let ((v (if (shell-var-integer? var)
             (let ((arith-fn (*arith-eval-fn*)))
               (if arith-fn
                 ;; Use arithmetic evaluation (handles expressions like "2+3")
                 (number->string
                  (arith-fn value
                            (if env
                              (lambda (name) (or (string->number (or (env-get env name) "0")) 0))
                              (lambda (name) (or (string->number (or name "0")) 0)))
                            (if env
                              (lambda (name val) (env-set! env name (number->string val)))
                              (lambda (name val) #!void))))
                 ;; Fallback: simple string->number
                 (let ((n (or (string->number value) 0)))
                   (number->string n))))
             value)))
    (cond
      ((shell-var-uppercase? var) (string-upcase v))
      ((shell-var-lowercase? var) (string-downcase v))
      (else v))))

;; Look up the shell-var struct from scope chain (not value, the struct itself)
(def (env-get-var env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (if var var
        (let ((parent (shell-environment-parent env)))
          (if parent (env-get-var parent name) #f)))))

;; Set a variable — respects scope chain for non-local vars
(def (env-set! env name value)
  (let ((existing (hash-get (shell-environment-vars env) name)))
    (cond
      ((and existing (shell-var-readonly? existing))
       (error (format "~a: readonly variable" name)))
      (existing
       (let ((final-value (apply-var-attrs existing value env)))
         (set! (shell-var-value existing) final-value)
         ;; If exported, also update OS env
         (when (shell-var-exported? existing)
           (setenv name final-value))))
      (else
       ;; Not in local scope — check parent chain
       (let ((owner (find-var-owner-in-chain (shell-environment-parent env) name)))
         (if owner
           ;; Variable exists in an ancestor scope — modify it there
           (let ((parent-var (hash-get (shell-environment-vars owner) name)))
             (when (and parent-var (shell-var-readonly? parent-var))
               (error (format "~a: readonly variable" name)))
             (let ((final-value (apply-var-attrs parent-var value env)))
               (set! (shell-var-value parent-var) final-value)
               (when (shell-var-exported? parent-var)
                 (setenv name final-value))))
           ;; Variable doesn't exist anywhere in the chain
           ;; Create in ROOT scope (bash behavior: vars in functions are global)
           (let* ((root (env-root env))
                  (os-val (getenv name #f)))
             (if os-val
               ;; Exists in OS env — create as exported
               (begin
                 (hash-put! (shell-environment-vars root) name
                            (make-shell-var value #t #f #f #f #f #f #f))
                 (setenv name value))
               (hash-put! (shell-environment-vars root) name
                          (make-shell-var value #f #f #f #f #f #f #f))))))))))

;; Set the shell name ($0)
(def (env-set-shell-name! env name)
  (shell-environment-shell-name-set! env name))

;; Mark variable as exported
(def (env-export! env name (value #f))
  (when value (env-set! env name value))
  (let ((var (hash-get (shell-environment-vars env) name)))
    (if var
      (begin
        (set! (shell-var-exported? var) #t)
        (setenv name (shell-var-value var)))
      ;; Create empty exported variable
      (when value
        (hash-put! (shell-environment-vars env) name
                   (make-shell-var value #t #f #f #f #f #f #f))
        (setenv name value)))))

;; Unset a variable
(def (env-unset! env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (when (and var (shell-var-readonly? var))
      (error (format "~a: readonly variable" name)))
    (hash-remove! (shell-environment-vars env) name)))

;; Mark variable as readonly
(def (env-readonly! env name (value #f))
  (when value (env-set! env name value))
  (let ((var (hash-get (shell-environment-vars env) name)))
    (when var
      (set! (shell-var-readonly? var) #t))))

;; Get an alist of all exported variables for passing to child processes
(def (env-exported-alist env)
  (let ((result (make-hash-table)))
    ;; Start with OS environment
    (for-each (lambda (pair) (hash-put! result (car pair) (cdr pair)))
              (get-environment-variables))
    ;; Override with our exported vars
    (env-collect-exports! env result)
    (hash-map (lambda (k v) (string-append k "=" v)) result)))

;; Collect exported variables from scope chain into a hash table
(def (env-collect-exports! env target)
  (let ((parent (shell-environment-parent env)))
    (when parent (env-collect-exports! parent target)))
  (hash-for-each
   (lambda (name var)
     (when (shell-var-exported? var)
       (hash-put! target name (shell-var-value var))))
   (shell-environment-vars env)))

;; Push a new scope (for function calls)
(def (env-push-scope env)
  (make-shell-environment parent: env name: (shell-environment-shell-name env)))

;; Deep clone an environment (for subshells)
;; Creates a fully independent copy — mutations don't affect the original.
(def (env-clone env)
  (let ((clone (make-shell-environment name: (shell-environment-shell-name env))))
    ;; Deep copy all hash tables so mutations are isolated
    (set! (shell-environment-vars clone) (clone-var-table env))
    (set! (shell-environment-functions clone) (hash-copy (shell-environment-functions env)))
    (set! (shell-environment-aliases clone) (hash-copy (shell-environment-aliases env)))
    (set! (shell-environment-options clone) (hash-copy (shell-environment-options env)))
    (set! (shell-environment-shopts clone) (hash-copy (shell-environment-shopts env)))
    (set! (shell-environment-positional clone) (vector-copy (shell-environment-positional env)))
    (set! (shell-environment-last-status clone) (shell-environment-last-status env))
    (set! (shell-environment-last-bg-pid clone) (shell-environment-last-bg-pid env))
    (set! (shell-environment-shell-pid clone) (shell-environment-shell-pid env))
    (set! (shell-environment-start-time clone) (shell-environment-start-time env))
    (set! (shell-environment-cmd-number clone) (shell-environment-cmd-number env))
    (set! (shell-environment-traps clone) (hash-copy (shell-environment-traps env)))
    (set! (shell-environment-dir-stack clone) (shell-environment-dir-stack env))
    clone))

;; Clone the var table from full scope chain into a flat hash table
(def (clone-var-table env)
  (let ((result (make-hash-table)))
    (clone-vars-from-chain! env result)
    result))

(def (clone-vars-from-chain! env target)
  (let ((parent (shell-environment-parent env)))
    (when parent (clone-vars-from-chain! parent target)))
  (hash-for-each
   (lambda (name var)
     (hash-put! target name
                (make-shell-var (shell-var-value var)
                                (shell-var-exported? var)
                                (shell-var-readonly? var)
                                (shell-var-local? var)
                                (shell-var-integer? var)
                                (shell-var-uppercase? var)
                                (shell-var-lowercase? var)
                                (shell-var-nameref? var))))
   (shell-environment-vars env)))

;; Pop back to parent scope
(def (env-pop-scope env)
  (or (shell-environment-parent env)
      (error "cannot pop root environment")))

;;; --- Positional parameters ---

(def (env-set-positional! env args)
  (set! (shell-environment-positional env) (list->vector args)))

(def (env-positional-list env)
  (vector->list (shell-environment-positional env)))

;; $* — all positional params as single string separated by first char of IFS
(def (env-star env)
  (let* ((pos (env-positional-list env))
         (ifs (or (env-get env "IFS") " \t\n"))
         (sep (if (> (string-length ifs) 0)
                (string (string-ref ifs 0))
                "")))
    (string-join-with sep pos)))

;; $@ — all positional params as separate words
(def (env-at env)
  (env-positional-list env))

;;; --- Shell options ---

(def (env-option-set! env name value)
  (hash-put! (shell-environment-options env) name value))

(def (env-option? env name)
  (hash-get (shell-environment-options env) name))

(def (env-shopt-set! env name value)
  (hash-put! (shell-environment-shopts env) name value))

(def (env-shopt? env name)
  (hash-get (shell-environment-shopts env) name))

;;; --- Set last status ---

(def (env-set-last-status! env status)
  (set! (shell-environment-last-status env) status))

(def (env-set-last-bg-pid! env pid)
  (set! (shell-environment-last-bg-pid env) pid))

(def (env-inc-cmd-number! env)
  (set! (shell-environment-cmd-number env)
        (+ 1 (shell-environment-cmd-number env))))

;;; --- Initialize environment from OS ---

(def (env-init! env)
  ;; Import all environment variables
  (for-each
   (lambda (pair)
     (hash-put! (shell-environment-vars env) (car pair)
                (make-shell-var (cdr pair) #t #f #f #f #f #f #f)))
   (get-environment-variables))
  ;; Set defaults if not present
  (unless (env-get-local env "IFS")
    (env-set! env "IFS" " \t\n"))
  (unless (env-get-local env "PS1")
    (env-set! env "PS1" "\\u@\\h:\\w\\$ "))
  (unless (env-get-local env "PS2")
    (env-set! env "PS2" "> "))
  (unless (env-get-local env "PS4")
    (env-set! env "PS4" "+ "))
  (unless (env-get-local env "HISTFILE")
    (env-set! env "HISTFILE" (string-append (or (getenv "HOME" #f) ".") "/.gsh_history")))
  (unless (env-get-local env "HISTSIZE")
    (env-set! env "HISTSIZE" "1000"))
  (unless (env-get-local env "HISTFILESIZE")
    (env-set! env "HISTFILESIZE" "2000"))
  ;; Set PWD
  (env-set! env "PWD" (current-directory))
  ;; Set SHLVL
  (let* ((shlvl-str (or (getenv "SHLVL" #f) "0"))
         (shlvl (or (string->number shlvl-str) 0)))
    (env-set! env "SHLVL" (number->string (+ shlvl 1)))
    (env-export! env "SHLVL"))
  ;; Set PPID
  (env-set! env "PPID" (number->string (##os-getppid)))
  ;; Set shell-specific vars
  (env-set! env "SHELL" (or (getenv "SHELL" #f) "/bin/gsh"))
  ;; Default shell options
  (env-option-set! env "hashall" #t)
  (env-option-set! env "interactive-comments" #t)
  ;; Default shopt options
  (env-shopt-set! env "cmdhist" #t)
  (env-shopt-set! env "complete_fullquote" #t)
  (env-shopt-set! env "extquote" #t)
  (env-shopt-set! env "force_fignore" #t)
  (env-shopt-set! env "hostcomplete" #t)
  (env-shopt-set! env "interactive_comments" #t)
  (env-shopt-set! env "progcomp" #t)
  (env-shopt-set! env "promptvars" #t)
  (env-shopt-set! env "sourcepath" #t))

;;; --- Helpers ---

(def (positional-index name)
  (and (> (string-length name) 0)
       (let loop ((i 0))
         (if (>= i (string-length name))
           (string->number name)
           (and (char-numeric? (string-ref name i))
                (loop (+ i 1)))))))

(def (find-var-in-chain env name)
  (let ((var (hash-get (shell-environment-vars env) name)))
    (or var
        (let ((parent (shell-environment-parent env)))
          (and parent (find-var-in-chain parent name))))))

;; Get the root (outermost) environment scope
(def (env-root env)
  (let ((parent (shell-environment-parent env)))
    (if parent (env-root parent) env)))

;; Find the environment scope that owns a variable (for setting in parent chain)
(def (find-var-owner-in-chain env name)
  (and env
       (let ((var (hash-get (shell-environment-vars env) name)))
         (if var
           env
           (find-var-owner-in-chain (shell-environment-parent env) name)))))

(def (options->flag-string env)
  (let ((flags []))
    (when (env-option? env "errexit") (set! flags (cons #\e flags)))
    (when (env-option? env "noglob") (set! flags (cons #\f flags)))
    (when (env-option? env "hashall") (set! flags (cons #\h flags)))
    (when (env-option? env "monitor") (set! flags (cons #\m flags)))
    (when (env-option? env "noclobber") (set! flags (cons #\C flags)))
    (when (env-option? env "nounset") (set! flags (cons #\u flags)))
    (when (env-option? env "xtrace") (set! flags (cons #\x flags)))
    (when (env-option? env "verbose") (set! flags (cons #\v flags)))
    (list->string (reverse flags))))

(def (string-join-with sep lst)
  (if (null? lst) ""
      (let loop ((rest (cdr lst)) (acc (car lst)))
        (if (null? rest) acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))
