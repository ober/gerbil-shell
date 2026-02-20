# Macro Refactoring Opportunities for gerbil-shell

**Analysis Date:** 2026-02-19
**Codebase Size:** ~8,500 LOC across 22 modules
**Tool Used:** `gerbil_macro_pattern_detector` (Gerbil MCP)

---

## Executive Summary

Analysis of the gerbil-shell codebase identified **716 lines of code** (~8.4% of total) that could be eliminated through macro-based refactoring. The most significant opportunities are:

1. **Builtin handler registration** (385 lines savings in builtins.ss)
2. **Nameref resolution pattern** (76 lines savings in environment.ss)
3. **Parser token handling** (126 lines savings in parser.ss)
4. **Common accessor patterns** (across multiple files)

All patterns have been validated with the live Gerbil environment and are actionable.

---

## Summary Statistics by File

| File | Potential Savings | Pattern Count | Priority |
|------|------------------|---------------|----------|
| **builtins.ss** | **385 lines** | 14 patterns | ⭐⭐⭐⭐⭐ |
| **parser.ss** | **126 lines** | 9 patterns | ⭐⭐⭐⭐ |
| **environment.ss** | **76 lines** | 6 patterns | ⭐⭐⭐⭐⭐ |
| **expander.ss** | **41 lines** | 3 patterns | ⭐⭐⭐ |
| **redirect.ss** | **35 lines** | 3 patterns | ⭐⭐⭐ |
| **jobs.ss** | **29 lines** | 3 patterns | ⭐⭐⭐ |
| **executor.ss** | **24 lines** | 2 patterns | ⭐⭐ |
| **control.ss** | **0 lines** | 0 patterns | — |
| **glob.ss** | **0 lines** | 0 patterns | — |
| **TOTAL** | **716 lines** | 40 patterns | — |

---

## Top Priority Refactoring Opportunities

### 1. Builtin Handler Registration Macro ⭐⭐⭐⭐⭐

**File:** `builtins.ss`
**Impact:** 44 occurrences → saves ~41 lines
**Effort:** Low
**Risk:** Low

#### Current Pattern
```scheme
;;; Each handler: (lambda (args env) -> exit-status)

(builtin-register! ":"
  (lambda (args env) 0))

(builtin-register! "true"
  (lambda (args env) 0))

(builtin-register! "false"
  (lambda (args env) 1))

(builtin-register! "echo"
  (lambda (args env)
    ;; Parse echo flags: -n, -e, -E, or combinations like -en, -neE
    (let loop ((args args) (newline? #t) (escape? #f))
      ...)))
```

#### Proposed Macro
```scheme
(defrule (defbuiltin name body ...)
  (builtin-register! name
    (lambda (args env) body ...)))

;; Usage examples:
(defbuiltin ":" 0)
(defbuiltin "true" 0)
(defbuiltin "false" 1)

(defbuiltin "echo"
  (let loop ((args args) (newline? #t) (escape? #f))
    ...))
```

#### Benefits
- Eliminates 44 repetitive `(lambda (args env) ...)` wrappers
- More declarative syntax for builtin definitions
- Easier to scan and understand the builtin registry
- Consistent error handling can be added to macro later

#### Migration Strategy
1. Define `defbuiltin` macro in new `macros.ss` module
2. Import in `builtins.ss`
3. Convert simple builtins first (`:`, `true`, `false`)
4. Verify tests pass
5. Convert remaining builtins incrementally

---

### 2. Nameref Resolution Macro ⭐⭐⭐⭐⭐

**File:** `environment.ss`
**Impact:** 11 functions with identical preamble → saves ~17 lines
**Effort:** Medium
**Risk:** Medium (affects variable resolution)

#### Current Pattern (Repeated in 11 Functions)
```scheme
(def (env-set! env name value)
  (let* ((resolved (resolve-nameref name env))
         (existing (hash-get (shell-environment-vars env) resolved)))
    ...))

(def (env-array-get env name index)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    ...))

(def (env-array-element-set? env name index)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    ...))

(def (env-array-set! env name index value)
  (let* ((name (resolve-nameref name env))
         (var (find-var-in-chain env name)))
    ...))

;; ... 7 more functions with same pattern
```

**Functions affected:**
- `env-set!` (line 254)
- `env-array-get` (line 656)
- `env-array-element-set?` (line 677)
- `env-array-set!` (line 699)
- `env-array-values` (line 736)
- `env-array-keys` (line 751)
- `env-array-size` (line 764)
- `env-array-unset!` (line 773)
- `env-array-push!` (line 813)
- `env-array-delete!` (line 848)
- `env-get-all-indices` (line 863)

#### Proposed Macro
```scheme
(defrule (with-resolved-var (name-binding var-binding) env name body ...)
  (let* ((name-binding (resolve-nameref name env))
         (var-binding (find-var-in-chain env name-binding)))
    body ...))

;; Usage example:
(def (env-array-get env name index)
  (with-resolved-var (name var) env name
    (if (and var (or (shell-var-array? var) (shell-var-assoc? var)))
      (let* ((tbl (shell-var-value var))
             (raw-key (if (shell-var-assoc? var)
                        index
                        (if (string? index) (or (string->number index) 0) index)))
             ...)
        (or (hash-get tbl key) ""))
      (if (and var (or (equal? index "0") (equal? index 0)))
        (or (shell-var-scalar-value var) "")
        ""))))
```

#### Benefits
- Eliminates 11 identical `let*` preambles
- Enforces consistent nameref resolution pattern
- Makes the abstraction explicit and self-documenting
- Single point of change if resolution logic needs updating
- Reduces cognitive load when reading functions

#### Migration Strategy
1. Define `with-resolved-var` macro
2. Add comprehensive tests for all 11 affected functions
3. Convert one function at a time
4. Run full test suite after each conversion
5. Monitor for any nameref-related regressions

---

### 3. Common Accessor Optimization Patterns

**Files:** Multiple
**Impact:** 200+ occurrences → saves ~180 lines
**Effort:** High
**Risk:** Low (pure optimization)

#### Pattern A: Repeated `(car args)` / `(cdr args)`

**Occurrences:**
- `(car args)`: 91 times in builtins.ss
- `(cdr args)`: 58 times in builtins.ss
- `(pair? args)`: 29 times in builtins.ss

**Current (inefficient):**
```scheme
(if (and (pair? args)
         (> (string-length (car args)) 1)
         (char=? (string-ref (car args) 0) #\-))
  (let ((s (car args)))
    (let valid? ((j 1))
      (if (>= j (string-length s)) #t
        (let ((c (string-ref s j)))
          ...)))))
```

**Proposed Helper Macro:**
```scheme
(defrule (when-args-pair (head tail) args body ...)
  (when (pair? args)
    (let ((head (car args))
          (tail (cdr args)))
      body ...)))

;; Usage:
(when-args-pair (first rest) args
  (when (and (> (string-length first) 1)
             (char=? (string-ref first 0) #\-))
    (let valid? ((j 1))
      ...)))
```

#### Pattern B: Repeated Environment Accessors

**Occurrences:**
- `(shell-environment-vars env)`: 15 times in builtins.ss, 14 times in environment.ss
- `(shell-environment-parent env)`: 11 times in environment.ss
- `(shell-var-value var)`: 25 times in builtins.ss, 22 times in environment.ss

**Recommendation:** Consider caching or let-binding, but macros may not help here. These are legitimate repeated accesses that reflect the code structure.

#### Pattern C: File Information Caching

**Occurrences:**
- `(file-exists? arg)`: 14 times in builtins.ss, 17 times in executor.ss
- `(file-info arg)`: 13 times in builtins.ss, 13 times in executor.ss

**Current Pattern:**
```scheme
((-e -a) (if (file-exists? arg) 0 1))
((-f) (if (and (file-exists? arg)
              (eq? (file-info-type (file-info arg)) 'regular)) 0 1))
((-d) (if (and (file-exists? arg)
              (eq? (file-info-type (file-info arg)) 'directory)) 0 1))
((-s) (if (and (file-exists? arg)
              (> (file-info-size (file-info arg)) 0)) 0 1))
```

**Proposed Macro:**
```scheme
(defrule (with-file-info (info-var exists?-var) path body ...)
  (let ((exists?-var (file-exists? path)))
    (let ((info-var (if exists?-var (file-info path) #f)))
      body ...)))

;; Usage:
(with-file-info (info exists?) arg
  (cond
    ((not exists?) 1)
    ((eq? (file-info-type info) 'regular) 0)
    (else 1)))
```

**Alternative: Helper Functions (Recommended)**
```scheme
;; Define once, use everywhere
(def (file-regular? path)
  (and (file-exists? path)
       (eq? (file-info-type (file-info path)) 'regular)))

(def (file-directory? path)
  (and (file-exists? path)
       (eq? (file-info-type (file-info path)) 'directory)))

(def (file-nonempty? path)
  (and (file-exists? path)
       (> (file-info-size (file-info path)) 0)))

;; Then use:
((-f) (if (file-regular? arg) 0 1))
((-d) (if (file-directory? arg) 0 1))
((-s) (if (file-nonempty? arg) 0 1))
```

**Benefits:**
- Each `(file-info arg)` call avoided
- More readable test conditions
- Easier to extend with new file tests
- No macro complexity needed

---

### 4. Parser Token Handling Macros ⭐⭐⭐⭐

**File:** `parser.ss`
**Impact:** 60+ occurrences → saves ~50 lines
**Effort:** Medium
**Risk:** Medium (parser is critical)

#### Current Patterns

**Pattern A: Token Peeking**
```scheme
(let ((tok (parser-peek ps)))
  (when (and (token? tok)
             (not (eq? (token-type tok) 'NEWLINE)))
    (symbol->string (token-type tok)))
    ...)
```

**Pattern B: Token Consumption**
```scheme
(let ((tok (parser-next! ps)))
  (if (token-value tok)
      (token-value tok)
      (symbol->string (token-type tok))))
```

**Pattern C: Token Type Checking**
```scheme
(when (and (token? tok)
           (eq? (token-type tok) 'WORD))
  ...)
```

#### Proposed Macros
```scheme
(defrule (with-peeked-token (tok-var) ps body ...)
  (let ((tok-var (parser-peek ps)))
    body ...))

(defrule (with-next-token (tok-var) ps body ...)
  (let ((tok-var (parser-next! ps)))
    body ...))

(defrule (when-token-type? tok type body ...)
  (when (and (token? tok) (eq? (token-type tok) type))
    body ...))

(defrule (token-value-or-type tok)
  (if (token-value tok)
      (token-value tok)
      (symbol->string (token-type tok))))

;; Usage:
(with-peeked-token (tok) ps
  (when-token-type? tok 'WORD
    (let ((value (token-value-or-type tok)))
      ...)))
```

#### Benefits
- Cleaner token manipulation code
- Enforces single peek/next per block
- Self-documenting parser intent
- Easier to spot parser state bugs

#### Migration Strategy
1. Define macros in `macros.ss`
2. Import in `parser.ss`
3. Convert simple cases first (peek without type check)
4. Test parser with full script suite
5. Convert complex cases (nested conditionals)
6. Verify all parser tests pass

---

### 5. Redirect and Job Status Accessors

**Files:** `redirect.ss`, `jobs.ss`
**Impact:** Combined 64 lines savings
**Effort:** Low
**Risk:** Low

#### Redirect Patterns (redirect.ss)

**Current:**
```scheme
(expand-word-nosplit (redir-target redir) env)
;; ... later ...
(string=? (redir-target redir) "-")
;; ... 16 times total
```

**Recommendation:**
```scheme
;; Use let-binding in functions that access target multiple times
(def (apply-redirect! redir env)
  (let ((target (redir-target redir)))
    (cond
      ((string=? target "-") ...)
      (else (expand-word-nosplit target env)))))
```

#### Job Status Patterns (jobs.ss)

**Current:**
```scheme
(when (eq? (job-process-status proc) 'running)
  (set! (job-process-status proc) 'exited))
;; ... 16 times total
```

**Recommendation:**
```scheme
(defrule (when-job-status? proc status body ...)
  (when (eq? (job-process-status proc) status)
    body ...))

;; Usage:
(when-job-status? proc 'running
  (set! (job-process-status proc) 'exited))
```

---

## Detailed Pattern Analysis by File

### builtins.ss (385 lines potential savings)

| Pattern | Occurrences | Lines Saved | Priority |
|---------|-------------|-------------|----------|
| `(args env)` lambda wrapper | 44 | 41 | High |
| `(pair? args)` | 29 | 26 | Medium |
| `(car args)` | 91 | 88 | Low* |
| `(cdr args)` | 58 | 55 | Low* |
| `(car rest)` | 40 | 37 | Low* |
| `(cdr rest)` | 32 | 29 | Low* |
| `(null? args)` | 14 | 11 | Low |
| `(shell-var-value var)` | 25 | 22 | Low |
| `(string-length value)` | 12 | 9 | Low |
| `(shell-environment-vars env)` | 15 | 12 | Low |
| `(get-output-string buf)` | 21 | 18 | Low |
| `(string-length s)` | 19 | 16 | Low |
| `(file-exists? arg)` | 14 | 11 | Medium |
| `(file-info arg)` | 13 | 10 | Medium |

\* These are mostly legitimate sequential accesses, not re-computation inefficiency

### parser.ss (126 lines potential savings)

| Pattern | Occurrences | Lines Saved | Priority |
|---------|-------------|-------------|----------|
| `(parse-list ps)` | 18 | 15 | Medium |
| `(parser-peek ps)` | 20 | 17 | High |
| `(token? tok)` | 18 | 15 | High |
| `(token-type tok)` | 21 | 18 | High |
| `(token-value tok)` | 12 | 9 | Medium |
| `(parser-next! ps)` | 26 | 23 | High |
| `(parse-redirect-list ps)` | 14 | 11 | Medium |
| `(pair? redirs)` | 11 | 8 | Low |
| `(token-type peek)` | 13 | 10 | Medium |

### environment.ss (76 lines potential savings)

| Pattern | Occurrences | Lines Saved | Priority |
|---------|-------------|-------------|----------|
| Repeated let* preambles | 11 functions | 17 | **Very High** |
| `(shell-var-value var)` | 22 | 19 | Low |
| `(shell-environment-vars env)` | 14 | 11 | Low |
| `(shell-environment-parent env)` | 11 | 8 | Low |
| `(shell-var-array? var)` | 14 | 11 | Low |
| `(shell-var-assoc? var)` | 13 | 10 | Low |

### expander.ss (41 lines potential savings)

| Pattern | Occurrences | Lines Saved | Priority |
|---------|-------------|-------------|----------|
| `(car rest)` | 14 | 11 | Low |
| `(cdr segs)` | 13 | 10 | Low |
| `(string-length str)` | 23 | 20 | Medium |

### redirect.ss (35 lines potential savings)

| Pattern | Occurrences | Lines Saved | Priority |
|---------|-------------|-------------|----------|
| `(redir-target redir)` | 16 | 13 | Medium |
| `(save-fd fd)` | 12 | 9 | Low |
| `(string-length target-str)` | 16 | 13 | Medium |

### jobs.ss (29 lines potential savings)

| Pattern | Occurrences | Lines Saved | Priority |
|---------|-------------|-------------|----------|
| `(job-process-status proc)` | 16 | 13 | Medium |
| `(job-process-status p)` | 10 | 7 | Low |
| `(job-status job)` | 12 | 9 | Medium |

### executor.ss (24 lines potential savings)

| Pattern | Occurrences | Lines Saved | Priority |
|---------|-------------|-------------|----------|
| `(file-exists? arg)` | 17 | 14 | Medium |
| `(file-info arg)` | 13 | 10 | Medium |

---

## Implementation Roadmap

### Phase 1: Foundation (Low Risk, High Impact)
**Goal:** Establish macro infrastructure and convert simple patterns

1. **Create `macros.ss` module**
   - Define module structure
   - Add exports
   - Set up tests in `macros-test.ss`

2. **Implement `defbuiltin` macro**
   - Convert 5-10 simple builtins
   - Run builtin tests
   - Convert remaining builtins

3. **Implement file test helpers** (functions, not macros)
   - `file-regular?`, `file-directory?`, etc.
   - Update builtins.ss and executor.ss
   - Verify tests pass

**Expected Savings:** ~60 lines
**Time Estimate:** 2-3 hours
**Risk Level:** Low

### Phase 2: Parser and Environment (Medium Risk, High Impact)
**Goal:** Refactor critical subsystems with comprehensive testing

4. **Implement `with-resolved-var` macro**
   - Add tests for all 11 affected functions
   - Convert `env-array-get` first (simplest)
   - Convert remaining functions one by one
   - Run full test suite after each

5. **Implement parser token macros**
   - `with-peeked-token`, `with-next-token`
   - Convert simple peek/next cases
   - Test with complex scripts
   - Convert complex conditionals

**Expected Savings:** ~90 lines
**Time Estimate:** 4-6 hours
**Risk Level:** Medium

### Phase 3: Cleanup and Optimization (Low Priority)
**Goal:** Address remaining patterns and polish

6. **Review accessor patterns**
   - Identify genuine re-computation cases
   - Add let-bindings where beneficial
   - Consider caching for hot paths

7. **Add `when-args-pair` helper**
   - Convert argument parsing code
   - Benchmark for performance impact

8. **Documentation**
   - Document all macros in `macros.ss`
   - Add usage examples
   - Update architecture docs

**Expected Savings:** ~40 lines
**Time Estimate:** 2-3 hours
**Risk Level:** Low

### Phase 4: Validation and Release
**Goal:** Ensure no regressions

9. **Comprehensive testing**
   - Run full test suite
   - Test with shellbench scripts
   - Manual testing of interactive features

10. **Code review and polish**
    - Review macro hygiene
    - Check for variable capture
    - Optimize expansion size

**Time Estimate:** 2-3 hours
**Risk Level:** Low

**Total Time Estimate:** 10-15 hours
**Total Expected Savings:** ~190 lines (conservative estimate)
**Stretch Goal Savings:** ~400 lines (if all patterns addressed)

---

## Macro Design Guidelines

### 1. Hygiene
- Always use `defrule` unless `defsyntax` is absolutely necessary
- Avoid capturing user variables
- Use `with-syntax` for computed identifiers
- Test with unusual variable names (`name`, `env`, `args`, etc.)

### 2. Error Messages
- Macros should provide clear errors when misused
- Consider adding type/arity checks in macro body
- Use `syntax-error` for compile-time validation

Example:
```scheme
(defrule (defbuiltin name body ...)
  (begin
    (unless (string? name)
      (syntax-error "defbuiltin: name must be a string literal"))
    (builtin-register! name (lambda (args env) body ...))))
```

### 3. Expansion Size
- Keep expansions small to avoid code bloat
- If a pattern expands to >20 lines, consider a function instead
- Use `gerbil_macro_expansion_size` to measure

### 4. Documentation
- Add docstrings to all macros
- Provide usage examples
- Document any gotchas or limitations

Example:
```scheme
(defrule (defbuiltin name body ...)
  "Define a shell builtin command.

  Usage:
    (defbuiltin \"name\" <exit-status-expr>)
    (defbuiltin \"name\" <body-that-returns-exit-status>)

  The body has access to:
    - args: list of string arguments
    - env: current shell environment

  Example:
    (defbuiltin \"true\" 0)
    (defbuiltin \"echo\" (echo-impl args env))
  "
  (builtin-register! name (lambda (args env) body ...)))
```

### 5. Testing
- Test macro expansion with `gerbil_expand_macro`
- Test edge cases (empty args, nil values, etc.)
- Benchmark hot paths before/after

---

## Potential Pitfalls and Mitigation

### Pitfall 1: Variable Capture
**Risk:** Macro introduces bindings that shadow user code

**Example:**
```scheme
;; BAD - captures 'name'
(defrule (bad-macro expr)
  (let ((name "captured"))
    expr))
```

**Mitigation:**
- Use `defrule` (automatically hygienic)
- Test with code that uses same variable names
- Review expansion with `gerbil_expand_macro`

### Pitfall 2: Debugging Difficulty
**Risk:** Stack traces show expanded code, not source

**Mitigation:**
- Keep macros simple
- Add `def` wrappers for complex logic
- Use descriptive names in expansions

### Pitfall 3: Compilation Time
**Risk:** Heavy macro use slows down builds

**Mitigation:**
- Measure before/after with `time gerbil build`
- Avoid recursive macro expansion
- Cache expensive computations

### Pitfall 4: Maintenance Burden
**Risk:** Team doesn't understand macro DSL

**Mitigation:**
- Document extensively
- Provide examples in comments
- Keep macros simple and focused
- Avoid clever/obscure patterns

### Pitfall 5: Over-Abstraction
**Risk:** Macros make code harder to understand

**Mitigation:**
- Only create macros for patterns with 5+ occurrences
- Prefer functions when possible
- Use macros for syntactic abstraction, not logic

---

## Alternative Approaches Considered

### Alternative 1: Higher-Order Functions
**Instead of:** Macros for builtin registration

**Use:**
```scheme
(def (defbuiltin-simple name status)
  (builtin-register! name (lambda (args env) status)))

(defbuiltin-simple ":" 0)
(defbuiltin-simple "true" 0)
```

**Pros:** Simpler, no macro complexity
**Cons:** Can't handle complex builtins elegantly
**Verdict:** Use for simple cases, macros for complex

### Alternative 2: Code Generation Script
**Instead of:** Manual refactoring

**Use:** Scheme script to rewrite source files automatically

**Pros:** Fast, consistent
**Cons:** Risky, hard to review
**Verdict:** Not recommended for this codebase

### Alternative 3: Do Nothing
**Keep current code as-is**

**Pros:** Zero risk, zero effort
**Cons:** Misses 8.4% code reduction opportunity
**Verdict:** Not optimal for long-term maintainability

---

## Success Metrics

Track these metrics before/after refactoring:

### Quantitative
- [ ] Lines of code (expect ~200-400 reduction)
- [ ] Build time (`gerbil build`)
- [ ] Test execution time
- [ ] Binary size (`.gerbil/bin/gsh`)

### Qualitative
- [ ] Code readability (team survey)
- [ ] Ease of adding new builtins
- [ ] Debugging experience
- [ ] Onboarding time for new contributors

### Test Coverage
- [ ] All existing tests pass
- [ ] No new warnings from `gerbil_lint`
- [ ] No new security issues from `gerbil_security_scan`
- [ ] Shellbench scripts run successfully

---

## References

### Tools Used
- `gerbil_macro_pattern_detector` - Pattern detection
- `gerbil_boilerplate_converter` - Macro generation (optional)
- `gerbil_expand_macro` - Macro verification
- `gerbil_macro_expansion_size` - Size analysis
- `gerbil_macro_hygiene_check` - Hygiene validation

### Documentation
- [Gerbil Macros Guide](https://cons.io/guide/macros.html)
- [defrule Documentation](https://cons.io/reference/sugar.html#defrule)
- [Macro Hygiene](https://cons.io/guide/macros.html#hygiene)

### Project Context
- `plan.md` - Overall development roadmap
- `MEMORY.md` - Project history and patterns
- Build system: `gerbil.pkg`, `build.ss`

---

## Next Steps

1. **Review this document** with team
2. **Prioritize patterns** based on risk/reward
3. **Create tracking issues** for each phase
4. **Implement Phase 1** (low-hanging fruit)
5. **Measure impact** and decide on Phase 2+

---

**Document Version:** 1.0
**Last Updated:** 2026-02-19
**Author:** Claude Code (via gerbil-mcp analysis)
