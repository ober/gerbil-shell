;;; static-compat.ss — Static binary compatibility for gsh
;;; Patches the Gerbil module loader to use .scm files when .o1
;;; loading is unavailable (statically-linked binary, no dlopen).
;;;
;;; Both script.ss (,eval) and compiler.ss (,use) need this patching.
;;; The patch uses ##global-var-set! because compiled set! on runtime
;;; loader globals doesn't propagate to load-module's internal references.
;;; Must patch both 'load-module and '__load-module — the latter is the
;;; compiled Gerbil name used by .ssi (%#ref load-module) resolution.

(export ensure-static-compat!
        patch-loader-post-gxi!
        scm-only-load-module-active?)

(import :gerbil/runtime/loader
        :gsh/ffi)

;;; --- State ---

(def *static-binary-patched* #f)
(def *scm-only-load-module* #f)

(def (scm-only-load-module-active?)
  "Check if the scm-only load-module patch is active."
  (and *scm-only-load-module* #t))

;;; --- Detection ---

(def (static-binary?)
  "Detect if running as a statically-linked binary.
   Tests dlopen with a path — on musl static, dlopen(NULL) succeeds
   but dlopen(path) fails with 'Dynamic loading not supported'."
  (ffi-static-binary?))

;;; --- .scm-only module finder ---

(def (find-library-module-scm-only modpath)
  "Find a module's .scm file, skipping .o1 files.
   Used in static binaries where dlopen is unavailable."
  (let lp ((rest (load-path)))
    (if (pair? rest)
      (let* ((dir (car rest))
             (npath (path-expand modpath (path-expand dir)))
             (spath (string-append npath ".scm")))
        (if (file-exists? spath)
          (path-normalize spath)
          (lp (cdr rest))))
      #f)))

;;; --- .scm loader ---
;;; Uses ##eval-top with ##interaction-cte to evaluate forms in the top-level
;;; interaction environment. Before __load-gxi this uses Gambit's raw evaluator.
;;; After __load-gxi, works for most modules but may fail for cross-sub-module
;;; references in the current expansion context.

(def (load-scm-skip-declare path)
  "Load a .scm file, skipping (declare ...) forms.
   Redirects stdout to /dev/null during evaluation to suppress
   any spurious output from evaluated forms."
  (let ((null-port (open-output-file "/dev/null")))
    (dynamic-wind
      (lambda () #f)
      (lambda ()
        (parameterize ((current-output-port null-port))
          (call-with-input-file path
            (lambda (port)
              (let loop ()
                (let ((form (read port)))
                  (unless (eof-object? form)
                    (unless (and (pair? form) (eq? (car form) 'declare))
                      (##eval-top form ##interaction-cte))
                    (loop))))))))
      (lambda () (close-output-port null-port)))))

;;; --- Pre-load sub-modules for builtin parents ---
;;; Must run AFTER patch-loader-scm-only! so that any module loading
;;; triggered during pre-loading goes through the .scm-only replacement.

(def (has-tilde? s)
  "Check if string contains ~ character."
  (let loop ((i 0))
    (if (>= i (string-length s))
      #f
      (if (char=? (string-ref s i) #\~)
        #t
        (loop (+ i 1))))))

(def (pre-load-builtin-submodules!)
  "Pre-load ~N.scm sub-module files for all 'builtin parent modules.
   Scans each load-path directory for matching .scm files.
   Must be called after patch-loader-scm-only! so any cascading
   module loads use the .scm-only path."
  (for-each
   (lambda (dir)
     (when (file-exists? dir)
       (pre-load-dir dir dir)))
   (load-path)))

(def (pre-load-dir root-dir current-dir)
  "Recursively scan directory for sub-module .scm files and pre-load them."
  (with-catch
   (lambda (e) #f) ;; Ignore directory access errors
   (lambda ()
     (for-each
      (lambda (entry)
        (let ((full-path (path-expand entry current-dir)))
          (cond
           ;; .scm file with ~ in name → sub-module candidate
           ((and (> (string-length entry) 4)
                 (let ((len (string-length entry)))
                   (string=? (substring entry (- len 4) len) ".scm"))
                 (has-tilde? entry))
            (let* ((relpath (substring full-path
                                       (+ 1 (string-length root-dir))
                                       (string-length full-path)))
                   (modpath (substring relpath 0 (- (string-length relpath) 4))))
              (unless (hash-get __modules modpath)
                (with-catch
                 (lambda (e) #f) ;; Ignore load errors
                 (lambda ()
                   (load-scm-skip-declare full-path)
                   (mutex-lock! __load-mx)
                   (hash-put! __modules modpath full-path)
                   (mutex-unlock! __load-mx))))))
           ;; Directory (not hidden) → recurse
           ((and (not (char=? (string-ref entry 0) #\.))
                 (with-catch (lambda (e) #f)
                   (lambda () (eq? (file-info-type (file-info full-path)) 'directory))))
            (pre-load-dir root-dir full-path))
           (else #f))))
      (directory-files current-dir)))))

;;; --- Replacement load-module ---

(def (make-scm-only-load-module scm-finder)
  "Build a replacement load-module that uses .scm files only.
   Replicates the caching/mutex logic from gerbil/runtime/loader.
   Handles modules loaded on demand during and after __load-gxi."
  (lambda (modpath)
    (mutex-lock! __load-mx)
    (let ((state (hash-get __modules modpath)))
      (cond
       ((and state (or (eq? state 'builtin) (string? state)))
        (mutex-unlock! __load-mx) state)
       ((and state (eq? state 'loading))
        (mutex-unlock! __load-mx __load-cv)
        ;; Retry — load-module will be the patched version
        ((make-scm-only-load-module scm-finder) modpath))
       ((and state (pair? state) (eq? (car state) 'error))
        (mutex-unlock! __load-mx)
        (raise (cadr state)))
       (else
        (let ((path (scm-finder modpath)))
          (if (not path)
            (begin (mutex-unlock! __load-mx)
                   (error "module not found" modpath))
            (begin
              (hash-put! __modules modpath 'loading)
              (mutex-unlock! __load-mx)
              (with-catch
               (lambda (exn)
                 (mutex-lock! __load-mx)
                 (hash-put! __modules modpath (list 'error exn))
                 (condition-variable-broadcast! __load-cv)
                 (mutex-unlock! __load-mx)
                 (raise exn))
               (lambda ()
                 (load-scm-skip-declare path)
                 (mutex-lock! __load-mx)
                 (hash-put! __modules modpath path)
                 (when (not (hash-get __load-order modpath))
                   (hash-put! __load-order modpath __load-order-next)
                   (set! __load-order-next (+ 1 __load-order-next)))
                 (condition-variable-broadcast! __load-cv)
                 (mutex-unlock! __load-mx)
                 path))))))))))

;;; --- Patching ---

(def (patch-loader-scm-only!)
  "Override load-module in Gambit's global variable table so .ssi files
   (which are interpreted by Gambit) see the patched version.
   Patches both 'load-module and '__load-module (the compiled Gerbil name).
   Also stores the replacement in a Gambit global for post-__load-gxi patching."
  (let ((new-lm (make-scm-only-load-module find-library-module-scm-only)))
    (set! *scm-only-load-module* new-lm)
    ;; Patch both Gambit globals
    (for-each
     (lambda (sym)
       (let ((gv (##make-global-var sym)))
         (##global-var-set! gv new-lm)
         (##global-var-primitive-set! gv new-lm)))
     '(load-module __load-module))
    ;; Also store in a Gambit global we can reference by name from eval later
    (let ((gv2 (##make-global-var '__gsh-scm-load-module)))
      (##global-var-set! gv2 new-lm)
      (##global-var-primitive-set! gv2 new-lm))))

(def (patch-loader-post-gxi!)
  "After __load-gxi, (import :gerbil/core) overwrites the load-module binding.
   Re-patch both the Gambit globals (for .ssi interpretation) and the Gerbil
   eval binding (for the compiler/expander)."
  ;; Re-patch both Gambit globals (overwritten by import :gerbil/core)
  (for-each
   (lambda (sym)
     (let ((gv (##make-global-var sym)))
       (##global-var-set! gv *scm-only-load-module*)
       (##global-var-primitive-set! gv *scm-only-load-module*)))
   '(load-module __load-module))
  ;; Patch Gerbil eval binding
  (eval '(set! load-module __gsh-scm-load-module)))

;;; --- Public entry points ---

(def (ensure-static-compat!)
  "If running as a static binary, patch the module loader before __load-gxi.
   Patches load-module first, then pre-loads sub-module .scm files.
   Order matters: patching must happen before pre-loading so that any
   cascading module loads during pre-loading use the .scm-only path.
   Idempotent — only patches once."
  (unless *static-binary-patched*
    (set! *static-binary-patched* #t)
    (when (static-binary?)
      (patch-loader-scm-only!)
      (pre-load-builtin-submodules!))))
