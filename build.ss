#!/usr/bin/env gxi
(import :std/build-script
        :std/misc/path
        :std/misc/string)

;; Collect gambitgsc .o files for linking (enables in-process compile-file)
(def gambitgsc-ld-opts
  (let ((dir (path-expand "_vendor/gambitgsc" (current-directory))))
    (if (file-exists? dir)
      (let* ((files (directory-files dir))
             (ofiles (filter (lambda (f) (string-suffix? ".o" f)) files)))
        (if (pair? ofiles)
          (string-join (map (lambda (f) (path-expand f dir)) ofiles) " ")
          ""))
      "")))

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
    "lineedit"
    "completion"
    "static-compat"
    "script"
    "compiler"
    "startup"
    (exe: "main" bin: "gsh" optimize: #t debug: 'env
          "-ld-options" ,(string-append static-ld-opts "-lpcre2-8 " gambitgsc-ld-opts)))
  parallelize: (max 1 (quotient (##cpu-count) 2)))
