#!/usr/bin/env gxi
(import :std/build-script)
(defbuild-script
  '("ffi"
    "util"
    "ast"
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
    "builtins"
    "functions"
    "control"
    "executor"
    "history"
    "prompt"
    "lineedit"
    "completion"
    "script"
    "startup"
    (exe: "main" bin: "gsh"))
  parallelize: (max 1 (quotient (##cpu-count) 2)))
