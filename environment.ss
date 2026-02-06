;;; environment.ss — Shell variables & environment for gsh

(export #t)
(import :std/sugar
        :std/iter
        :std/misc/hash
        :gsh/ffi)

;;; --- Shell variable ---
(defstruct shell-var (value exported? readonly? local?) transparent: #t)

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
    (set! self.functions (if parent parent.functions (make-hash-table)))
    (set! self.aliases (if parent parent.aliases (make-hash-table)))
    (set! self.options (if parent parent.options (make-hash-table)))
    (set! self.shopts (if parent parent.shopts (make-hash-table)))
    (set! self.positional (if parent parent.positional (vector)))
    (set! self.last-status 0)
    (set! self.last-bg-pid 0)
    (set! self.shell-pid (if parent parent.shell-pid (##os-getpid)))
    (set! self.shell-name name)
    (set! self.start-time (if parent parent.start-time (time->seconds (current-time))))
    (set! self.cmd-number (if parent parent.cmd-number 0))
    (set! self.traps (if parent parent.traps (make-hash-table)))
    (set! self.dir-stack (if parent parent.dir-stack []))))

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

;; Set a variable
(def (env-set! env name value)
  (let ((existing (hash-get (shell-environment-vars env) name)))
    (cond
      ((and existing (shell-var-readonly? existing))
       (error (format "~a: readonly variable" name)))
      (existing
       (set! (shell-var-value existing) value)
       ;; If exported, also update OS env
       (when (shell-var-exported? existing)
         (setenv name value)))
      (else
       (let ((parent-var (find-var-in-chain env name)))
         (if (and parent-var (shell-var-exported? parent-var))
           ;; Inherit exported status
           (begin
             (hash-put! (shell-environment-vars env) name
                        (make-shell-var value #t #f #f))
             (setenv name value))
           (hash-put! (shell-environment-vars env) name
                      (make-shell-var value #f #f #f))))))))

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
                   (make-shell-var value #t #f #f))
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
                (make-shell-var (cdr pair) #t #f #f)))
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
