;;; compiler.ss — Gerbil compiler integration for gsh
;;; Embeds the Gerbil compiler so users can compile and load .ss modules
;;; from within the shell, without needing a separate gxc installation.
;;;
;;; When gambitgsc is linked in, compile-file runs in-process (no external gsc).
;;; When gambitgsc is absent but external gsc exists, delegates to it.
;;; When neither is available, generates .scm/.ssi only (no native .o1).

(export ensure-gerbil-compiler!
        gsc-available?
        embedded-compile-file?
        gsh-compile
        gsh-load
        gsh-use
        gsh-show-exports)

(import :std/sugar
        :std/format
        :gerbil/runtime/init
        :gerbil/runtime/loader
        :gerbil/expander
        :gerbil/compiler
        :gsh/static-compat)

;;; --- Lazy initialization ---

(def *gerbil-compiler-initialized* #f)

(def (ensure-gerbil-compiler!)
  "Initialize the Gerbil expander and compiler on first use.
   Called lazily to avoid startup cost for normal shell operations."
  (unless *gerbil-compiler-initialized*
    (set! *gerbil-compiler-initialized* #t)
    (ensure-static-compat!)
    (__load-gxi)
    (when (scm-only-load-module-active?)
      (patch-loader-post-gxi!))))

;;; --- Compiler detection ---

(def *embedded-compile-file-cached* 'unchecked)

(def (embedded-compile-file?)
  "Check if compile-file is available in-process (gambitgsc linked in)."
  (when (eq? *embedded-compile-file-cached* 'unchecked)
    (set! *embedded-compile-file-cached*
      (with-catch (lambda (e) #f)
        (lambda () (procedure? (eval 'compile-file))))))
  *embedded-compile-file-cached*)

(def (gsc-available?)
  "Check if native compilation is available (embedded or external gsc)."
  (or (embedded-compile-file?)
      (let ((gsc-env (getenv "GERBIL_GSC" #f)))
        (if gsc-env
          (file-exists? gsc-env)
          (file-exists?
           (path-expand "gsc" (path-expand "bin" (path-expand "~~"))))))))

;;; --- In-process .scm → .o1 compilation ---

(def (compile-scm-files outdir modpath)
  "Compile generated .scm files to .o1 using in-process compile-file.
   modpath is the module path (e.g. \"gsh/bench\") — may include package prefix.
   Compiles modpath.scm and all modpath~N.scm parts."
  (let* ((cf (eval 'compile-file))
         ;; Module path uses / separator, maps to directory structure
         (scm-dir (path-expand (path-directory modpath) outdir))
         (basename (path-strip-directory modpath)))
    ;; Compile the main .scm
    (let ((main-scm (path-expand (string-append basename ".scm") scm-dir)))
      (when (file-exists? main-scm)
        (cf main-scm)))
    ;; Compile ~N parts
    (let loop ((n 0))
      (let ((part-scm (path-expand (string-append basename "~" (number->string n) ".scm") scm-dir)))
        (when (file-exists? part-scm)
          (cf part-scm)
          (loop (+ n 1)))))))

;;; --- Compile ---

(def (gsh-compile srcpath
                  output-dir: (output-dir #f)
                  invoke-gsc: (invoke-gsc 'auto)
                  verbose: (verbose #f))
  "Compile a Gerbil source file (.ss).
   Returns (values outdir native?)."
  (ensure-gerbil-compiler!)
  (let* ((srcpath (path-normalize srcpath))
         (outdir (or output-dir (path-directory srcpath)))
         (has-embedded? (embedded-compile-file?))
         (use-external-gsc?
           (if (eq? invoke-gsc 'auto)
             ;; If we have embedded compile-file, don't invoke external gsc
             (if has-embedded? #f
               (let ((gsc-env (getenv "GERBIL_GSC" #f)))
                 (if gsc-env
                   (file-exists? gsc-env)
                   (file-exists?
                    (path-expand "gsc" (path-expand "bin" (path-expand "~~")))))))
             invoke-gsc))
         (opts [output-dir: outdir
                invoke-gsc: use-external-gsc?
                keep-scm: #t
                verbose: verbose
                generate-ssxi: #f])
         (basename (path-strip-extension (path-strip-directory srcpath))))
    (compile-module srcpath opts)
    ;; If embedded compile-file and we didn't use external gsc, compile .scm → .o1
    (when (and has-embedded? (not use-external-gsc?))
      ;; Derive module path from expander context (includes package prefix)
      (let ((modpath (with-catch
                      (lambda (e) basename)
                      (lambda ()
                        (let ((ctx (import-module srcpath)))
                          (symbol->string (expander-context-id ctx)))))))
        (compile-scm-files outdir modpath)))
    (let ((native? (or has-embedded? use-external-gsc?)))
      (fprintf (current-error-port) "compiled: ~a~a~n"
               srcpath (if native? "" " (scm only)"))
      (values outdir native?))))

;;; --- Load ---

(def (gsh-load modpath libdir: (libdir #f))
  "Load a compiled Gerbil module and import into the eval context.
   modpath is like \"test-mod\" or \"mylib/foo\"."
  (ensure-gerbil-compiler!)
  (when libdir
    (let ((dir (path-normalize libdir)))
      (unless (member dir (load-path))
        (add-load-path! dir))))
  (let ((mod-sym (string->symbol (string-append ":" modpath))))
    (eval `(import ,mod-sym))
    (fprintf (current-error-port) "loaded: ~a~n" modpath)))

;;; --- Use (compile + load) ---

(def (gsh-use srcpath
              output-dir: (output-dir #f)
              verbose: (verbose #f))
  "Compile and load a Gerbil source file in one step.
   Returns the module path string."
  (ensure-gerbil-compiler!)
  (let* ((srcpath (path-normalize srcpath)))
    (let-values (((outdir native?) (gsh-compile srcpath
                                                  output-dir: output-dir
                                                  verbose: verbose)))
      (unless (member outdir (load-path))
        (add-load-path! outdir))
      (let ((modpath
             (with-catch
              (lambda (e)
                (path-strip-extension (path-strip-directory srcpath)))
              (lambda ()
                (let ((ctx (import-module srcpath)))
                  (symbol->string (expander-context-id ctx)))))))
        (gsh-load modpath)
        modpath))))

;;; --- Show exports ---

(def (gsh-show-exports modpath)
  "List the exports of a module."
  (ensure-gerbil-compiler!)
  (let ((ctx (with-catch (lambda (e) #f)
               (lambda ()
                 (import-module (string->symbol (string-append ":" modpath)))))))
    (if (not ctx)
      (begin
        (fprintf (current-error-port) "gsh: cannot resolve module ~a~n" modpath)
        1)
      (let ((exports (module-context-export ctx)))
        (for-each
         (lambda (exp)
           (when (and (module-export? exp) (eqv? (module-export-phi exp) 0))
             (let* ((name (module-export-name exp))
                    (binding (with-catch (lambda (e) #f)
                               (lambda () (core-resolve-module-export exp)))))
               (cond
                 ((and binding (runtime-binding? binding))
                  (let ((val (with-catch (lambda (e) #f)
                               (lambda () (eval name)))))
                    (if (procedure? val)
                      (fprintf (current-output-port) "  ~a\tprocedure~n" name)
                      (fprintf (current-output-port) "  ~a\tvalue~n" name))))
                 ((and binding (syntax-binding? binding))
                  (fprintf (current-output-port) "  ~a\tsyntax~n" name))
                 (else
                  (fprintf (current-output-port) "  ~a~n" name))))))
         exports)
        0))))
