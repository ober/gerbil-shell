#!/usr/bin/env gxi
(import :std/build-script
        :std/misc/path
        :std/misc/string)

;; Collect .o files from a vendor directory for linking
(def (collect-vendor-objs subdir)
  (let ((dir (path-expand subdir (current-directory))))
    (if (file-exists? dir)
      (let* ((files (directory-files dir))
             (ofiles (filter (lambda (f) (string-suffix? ".o" f)) files)))
        (if (pair? ofiles)
          (string-join (map (lambda (f) (path-expand f dir)) ofiles) " ")
          ""))
      "")))

;; Collect gambitgsc .o files for linking (enables in-process compile-file)
(def gambitgsc-ld-opts (collect-vendor-objs "_vendor/gambitgsc"))

;; Collect gerbil-runtime .o files for linking (static builds only)
;; These embed missing runtime modules + .ssi data so the static binary
;; doesn't need -:~~=/path/to/gerbil
(def gerbil-runtime-ld-opts
  (if (getenv "GSH_STATIC" #f)
    (collect-vendor-objs "_vendor/gerbil-runtime")
    ""))

;; Collect gsh-dlopen .o files for linking (static builds only)
;; Provides dlopen/dlsym/dlclose/dlerror that override musl's weak stubs,
;; enabling .o1 loading in the static binary
(def gsh-dlopen-ld-opts
  (if (getenv "GSH_STATIC" #f)
    (collect-vendor-objs "_vendor/gsh-dlopen")
    ""))

;; GSH_STATIC=1 adds -static for static binary builds
(def static-ld-opts
  (if (getenv "GSH_STATIC" #f) "-static " ""))

(defbuild-script
  `("ffi"
    "util"
    "ast"
    "pregexp-compat"
    "environment"
    "lexer"
    "glob"
    "arithmetic"
    "expander"
    "parser"
    "redirect"
    "pipeline"
    "signals"
    "jobs"
    "registry"
    "macros"
    "builtins"
    "functions"
    "control"
    "executor"
    "history"
    "prompt"
    "fuzzy"
    "lineedit"
    "fzf"
    "completion"
    "static-compat"
    "script"
    "compiler"
    "startup"
    (exe: "main" bin: "gsh" optimize: #t debug: 'env
          "-ld-options" ,(string-append static-ld-opts "-lpcre2-8 " gsh-dlopen-ld-opts " " gambitgsc-ld-opts " " gerbil-runtime-ld-opts)))
  parallelize: (max 1 (quotient (##cpu-count) 2)))
