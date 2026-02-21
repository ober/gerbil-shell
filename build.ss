#!/usr/bin/env gxi
(import :std/build-script)
(defbuild-script
  '("ffi"
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
    "script"
    "compiler"
    "startup"
    (exe: "main" bin: "gsh" optimize: #t debug: 'env
          "-ld-options" "-lpcre2-8"))
  parallelize: (max 1 (quotient (##cpu-count) 2)))
