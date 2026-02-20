;;; pregexp-compat.ss — Drop-in replacement for :std/pregexp using PCRE2
;;;
;;; This module provides a pregexp-compatible API backed by PCRE2+JIT for
;;; massive performance improvements (10-500x faster on many operations).
;;;
;;; Import this instead of :std/pregexp for compatibility with existing code.
;;;
;;; CRITICAL OPTIMIZATION: Pattern caching happens automatically in pcre2-pregexp-match
;;; via ensure-regex -> pcre2-compile/cached (LRU cache, max 64 patterns).
;;; To enable caching, glob-pattern->pregexp returns pattern STRINGS not compiled regexes.

(export pregexp-match
        pregexp-match-positions
        pregexp-replace
        pregexp-replace*
        pregexp-quote)

(import :gerbil-pcre/pcre2/pcre2)

;; Define wrapper functions with pregexp-compatible names
(defalias pregexp-match pcre2-pregexp-match)
(defalias pregexp-match-positions pcre2-pregexp-match-positions)
(defalias pregexp-replace pcre2-pregexp-replace)
(defalias pregexp-replace* pcre2-pregexp-replace*)
(defalias pregexp-quote pcre2-pregexp-quote)
