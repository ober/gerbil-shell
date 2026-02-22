;;; compiler.ss — Gerbil compiler integration for gsh
;;; Embeds the Gerbil compiler so users can compile and load .ss modules
;;; from within the shell, without needing a separate gxc installation.
;;;
;;; When gsc (Gambit compiler) is available, full native compilation to .oN works.
;;; When gsc is absent, an interpreted fallback loads the generated .scm directly.

(export ensure-gerbil-compiler!
        gsc-available?
        gsh-compile
        gsh-load
        gsh-use
        gsh-show-exports)

(import :std/sugar
        :std/format
        :gerbil/runtime/init
        :gerbil/runtime/loader
        :gerbil/expander
        :gerbil/compiler)

;;; --- Lazy initialization ---

(def *gerbil-compiler-initialized* #f)

(def (ensure-gerbil-compiler!)
  "Initialize the Gerbil expander and compiler on first use.
   Called lazily to avoid startup cost for normal shell operations."
  (unless *gerbil-compiler-initialized*
    (set! *gerbil-compiler-initialized* #t)
    ;; __load-gxi sets up the expander, readtables, and eval hooks.
    ;; Safe to call even if script.ss already called it — guarded by our flag.
    (__load-gxi)))

;;; --- gsc detection ---

(def (gsc-available?)
  "Check if the Gambit compiler (gsc) is available for native compilation."
  (let ((gsc-env (getenv "GERBIL_GSC" #f)))
    (if gsc-env
      (file-exists? gsc-env)
      (file-exists?
       (path-expand "gsc" (path-expand "bin" (path-expand "~~")))))))

;;; --- Compile ---

(def (gsh-compile srcpath
                  output-dir: (output-dir #f)
                  invoke-gsc: (invoke-gsc 'auto)
                  verbose: (verbose #f))
  "Compile a Gerbil source file (.ss).
   Returns (values outdir use-gsc?)."
  (ensure-gerbil-compiler!)
  (let* ((srcpath (path-normalize srcpath))
         (outdir (or output-dir (path-directory srcpath)))
         (use-gsc? (if (eq? invoke-gsc 'auto) (gsc-available?) invoke-gsc))
         (opts [output-dir: outdir
                invoke-gsc: use-gsc?
                keep-scm: #t
                verbose: verbose
                generate-ssxi: #f]))
    (compile-module srcpath opts)
    (fprintf (current-error-port) "compiled: ~a~n" srcpath)
    (values outdir use-gsc?)))

;;; --- Interpreted .scm loader ---
;;; When gsc is unavailable, .scm/.ssi files are generated but can't be loaded
;;; normally because the Gerbil expander rewrites (declare ...) to (%#declare ...)
;;; which the evaluator rejects. This loader reads .scm with Gambit's read,
;;; skips declare forms, and eval's the rest.

(def (load-scm-interpreted filepath load-dir)
  "Load a generated .scm file by reading forms and eval-ing them,
   skipping (declare ...) forms and handling (load-module ...) calls."
  (let ((port (open-input-file filepath)))
    (let loop ()
      (let ((form (read port)))
        (if (eof-object? form)
          (close-port port)
          (begin
            (cond
              ;; Skip (declare ...) — optimizer hints for native compilation
              ((and (pair? form) (eq? (car form) 'declare))
               #f)
              ;; Handle (begin ...) which may contain (load-module "name~N")
              ((and (pair? form) (eq? (car form) 'begin))
               (for-each
                (lambda (subform)
                  (cond
                    ;; (begin) — empty, skip
                    ((and (pair? subform) (eq? (car subform) 'begin)
                          (null? (cdr subform)))
                     #f)
                    ;; (load-module "modname~N") — load the referenced file
                    ((and (pair? subform) (eq? (car subform) 'load-module)
                          (pair? (cdr subform)) (string? (cadr subform)))
                     (let ((mod-file (string-append load-dir "/"
                                                    (cadr subform) ".scm")))
                       (load-scm-interpreted mod-file load-dir)))
                    (else
                     (eval subform))))
                (cdr form)))
              (else
               (eval form)))
            (loop)))))))

(def (gsh-load-interpreted modpath)
  "Load a module's .scm files interpretively from the load path.
   Finds the main .scm and all ~N.scm files, loads them via eval."
  (let ((scm-file (find-scm-in-load-path modpath)))
    (if scm-file
      (let ((load-dir (path-directory scm-file)))
        (load-scm-interpreted scm-file load-dir)
        #t)
      #f)))

(def (find-scm-in-load-path modpath)
  "Search load-path for modpath.scm, return full path or #f."
  (let loop ((dirs (load-path)))
    (if (null? dirs)
      #f
      (let ((candidate (string-append (car dirs) "/" modpath ".scm")))
        (if (file-exists? candidate)
          candidate
          (loop (cdr dirs)))))))

;;; --- Load ---

(def (gsh-load modpath libdir: (libdir #f))
  "Load a compiled Gerbil module and import into the eval context.
   modpath is like \"test-mod\" or \"mylib/foo\".
   Tries native import first; falls back to interpreted .scm loading."
  (ensure-gerbil-compiler!)
  (when libdir
    (let ((dir (path-normalize libdir)))
      (unless (member dir (load-path))
        (add-load-path! dir))))
  ;; Try native import first (works when .o1 files exist)
  (let ((mod-sym (string->symbol (string-append ":" modpath))))
    (with-catch
     (lambda (e)
       ;; Native import failed — try interpreted .scm loading
       (if (gsh-load-interpreted modpath)
         (fprintf (current-error-port) "loaded: ~a (interpreted)~n" modpath)
         (raise e)))
     (lambda ()
       (eval `(import ,mod-sym))
       (fprintf (current-error-port) "loaded: ~a~n" modpath)))))

;;; --- Use (compile + load) ---

(def (gsh-use srcpath
              output-dir: (output-dir #f)
              verbose: (verbose #f))
  "Compile and load a Gerbil source file in one step.
   Returns the module path string."
  (ensure-gerbil-compiler!)
  (let* ((srcpath (path-normalize srcpath)))
    (let-values (((outdir use-gsc?) (gsh-compile srcpath
                                                  output-dir: output-dir
                                                  verbose: verbose)))
      ;; Add output dir to load path
      (unless (member outdir (load-path))
        (add-load-path! outdir))
      ;; Derive the module path from the compiled module context
      (let ((modpath
             (with-catch
              (lambda (e)
                ;; Fallback: derive from filename
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
