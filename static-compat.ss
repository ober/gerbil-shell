;;; static-compat.ss -- Static binary compatibility for gsh
;;; Patches the Gerbil module loader to use .scm files when .o1
;;; loading is unavailable (statically-linked binary, no dlopen).
;;;
;;; Both script.ss (,eval) and compiler.ss (,use) need this patching.
;;; The patch uses ##global-var-set! because compiled set! on runtime
;;; loader globals doesn't propagate to load-module's internal references.
;;; Must patch both 'load-module and '__load-module -- the latter is the
;;; compiled Gerbil name used by .ssi (%#ref load-module) resolution.
;;;
;;; Embedded .ssi support: when embedded_ssi.o is linked in, extracts
;;; .ssi files to a cache directory so the expander can find them.
;;; This eliminates the need for -:~~=/path/to/gerbil at runtime.

(export ensure-static-compat!
        ensure-static-compile-env!
        patch-loader-post-gxi!
        scm-only-load-module-active?
        static-binary?)

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
   Tests dlopen with a path -- on musl static, dlopen(NULL) succeeds
   but dlopen(path) fails with 'Dynamic loading not supported'."
  (ffi-static-binary?))

;;; --- Embedded .ssi extraction ---

(def *ssi-cache-dir* #f)

(def (extract-embedded-ssi!)
  "Extract embedded .ssi and .scm files to a cache directory and add to load-path.
   The .ssi files are needed by the expander's import-module to resolve
   module metadata. The .scm files contain runtime module code that
   load-module needs (numbered sub-modules like expander~1, ~2, etc.).
   Uses ~/.cache/gsh/lib/ as the cache directory with sentinel files
   to avoid re-extracting on subsequent runs."
  (let* ((cache-base (or (getenv "XDG_CACHE_HOME" #f)
                         (let ((home (getenv "HOME" #f)))
                           (and home (string-append home "/.cache")))))
         (cache-dir (and cache-base
                         (string-append cache-base "/gsh/lib"))))
    (when cache-dir
      ;; Extract .ssi files
      (when (> (ffi-has-embedded-ssi) 0)
        (let ((sentinel (string-append cache-dir "/.ssi-extracted")))
          (unless (file-exists? sentinel)
            (let ((count (ffi-extract-embedded-ssi cache-dir)))
              (when (> count 0)
                (with-output-to-file sentinel
                  (lambda () (display count))))))))
      ;; Extract .scm archive (compressed runtime modules)
      (when (> (ffi-has-embedded-scm) 0)
        (let ((sentinel (string-append cache-dir "/.scm-extracted")))
          (unless (file-exists? sentinel)
            (let ((rc (ffi-extract-embedded-scm cache-dir)))
              (when (= rc 0)
                (with-output-to-file sentinel
                  (lambda () (display "ok"))))))))
      ;; Add to load-path so expander finds .ssi and .scm files here
      (set! *ssi-cache-dir* cache-dir)
      (unless (member cache-dir (load-path))
        (add-load-path! cache-dir)))))

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
;;; The expander references internal bindings (like __foldr1) from sub-modules
;;; that must be loaded proactively -- on-demand loading via load-module is
;;; insufficient because the expander expects these bindings to already exist.
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
  "Pre-load ~N.scm sub-module files from the embedded runtime cache.
   Only scans the cache directory (not other load-path entries like
   installed packages, which may contain test files with side effects).
   Must be called after extract-embedded-ssi! and patch-loader-scm-only!."
  (when (and *ssi-cache-dir* (file-exists? *ssi-cache-dir*))
    (pre-load-dir *ssi-cache-dir* *ssi-cache-dir*)))

(def (pre-load-dir root-dir current-dir)
  "Recursively scan directory for sub-module .scm files and pre-load them."
  (with-catch
   (lambda (e) #f) ;; Ignore directory access errors
   (lambda ()
     (for-each
      (lambda (entry)
        (let ((full-path (path-expand entry current-dir)))
          (cond
           ;; .scm file with ~ in name -> sub-module candidate
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
           ;; Directory (not hidden) -> recurse
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
        ;; Retry
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
   Extracts embedded .ssi and .scm files, patches load-module, then
   pre-loads sub-module .scm files so the expander has all bindings ready.
   Order matters: extract -> patch -> pre-load.
   Idempotent -- only patches once."
  (unless *static-binary-patched*
    (set! *static-binary-patched* #t)
    (when (static-binary?)
      (extract-embedded-ssi!)
      (patch-loader-scm-only!)
      (pre-load-builtin-submodules!))))

;;; --- Compile-file environment for static binary ---

(def *static-compile-env-ready* #f)

(def (ensure-static-compile-env!)
  "Set up the compile-file environment for the static binary.
   Extracts embedded gambit.h to ~/.cache/gsh/include/,
   generates a minimal gambuild-C script at ~/.cache/gsh/bin/,
   and sets ~~ to ~/.cache/gsh/ so compile-file can find them.
   Returns #t if environment was set up successfully, #f otherwise.
   Idempotent."
  (cond
   (*static-compile-env-ready* #t)
   ((not (static-binary?)) #f)
   ((= (ffi-has-embedded-headers) 0) #f)
   (else
    (let* ((cache-base (or (getenv "XDG_CACHE_HOME" #f)
                           (let ((home (getenv "HOME" #f)))
                             (and home (string-append home "/.cache")))))
           (gambit-home (and cache-base (string-append cache-base "/gsh")))
           (include-dir (and gambit-home (string-append gambit-home "/include")))
           (bin-dir (and gambit-home (string-append gambit-home "/bin")))
           (lib-dir (and gambit-home (string-append gambit-home "/lib"))))
      (if (not gambit-home)
        #f
        (with-catch
         (lambda (e) #f)
         (lambda ()
           ;; Extract embedded headers
           (create-directory* include-dir)
           (ffi-extract-embedded-headers include-dir)
           ;; Create lib/ directory (GAMBITDIR_LIB reference)
           (create-directory* lib-dir)
           ;; Generate minimal gambuild-C
           (create-directory* bin-dir)
           (let ((gambuild-path (string-append bin-dir "/gambuild-C")))
             (unless (file-exists? gambuild-path)
               (with-output-to-file gambuild-path
                 (lambda ()
                   (display gambuild-c-script)))
               ;; Make executable
               (##shell-command
                (string-append "chmod +x " gambuild-path))))
           ;; Set ~~ to our cache directory
           (##set-gambitdir! (string-append gambit-home "/"))
           (set! *static-compile-env-ready* #t)
           #t)))))))

(def gambuild-c-script
  "#!/bin/sh
# Minimal gambuild-C for gsh static binary
# Only handles dyn operation (compile .c -> .o1)
case \"$1\" in
  dyn)
    ${BUILD_DYN_CC_PARAM:-cc} \\
      -O1 -shared -fPIC -fwrapv -fno-strict-aliasing \\
      -D___SINGLE_HOST -D___DYNAMIC \\
      -I\"$GAMBITDIR_INCLUDE\" \\
      -o \"$BUILD_DYN_OUTPUT_FILENAME_PARAM\" \\
      $BUILD_DYN_CC_OPTIONS_PARAM \\
      $BUILD_DYN_LD_OPTIONS_PRELUDE_PARAM \\
      \"$BUILD_DYN_INPUT_FILENAMES_PARAM\" \\
      $BUILD_DYN_LD_OPTIONS_PARAM \\
      2>&1
    ;;
  *)
    echo \"gambuild-C: unsupported operation: $1\" >&2
    exit 1
    ;;
esac
")
