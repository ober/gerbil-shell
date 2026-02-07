;;; parser.ss — Recursive-descent AST builder for gsh
;;; Consumes tokens from the lexer and produces AST nodes.

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/lexer)

;;; --- Parser state ---

(defstruct parser-state
  (lexer         ;; lexer instance
   peeked        ;; peeked token or #f
   needs-more?   ;; #t if input is incomplete
   heredoc-queue ;; list of heredoc tokens waiting to be consumed
   )
  transparent: #t)

;;; --- Public interface ---

;; Parse a complete command from input string
;; Returns an AST node (command-list, and-or-list, etc.) or #f for empty input
(def (parse-complete-command input)
  (let* ((lex (if (string? input) (make-shell-lexer input) input))
         (ps (make-parser-state lex #f #f [])))
    (let ((result (parse-list ps)))
      (when (lexer-needs-more? lex)
        (set! (parser-state-needs-more? ps) #t))
      result)))

;; Check if parser needs more input (incomplete command)
(def (parser-needs-more? ps)
  (parser-state-needs-more? ps))

;;; --- Token access ---

(def (parser-peek ps)
  (unless (parser-state-peeked ps)
    (set! (parser-state-peeked ps) (lexer-next! (parser-state-lexer ps))))
  (parser-state-peeked ps))

(def (parser-next! ps)
  (if (parser-state-peeked ps)
    (let ((tok (parser-state-peeked ps)))
      (set! (parser-state-peeked ps) #f)
      tok)
    (lexer-next! (parser-state-lexer ps))))

(def (parser-consume! ps)
  (parser-next! ps))

;; Check if next token is a specific type
(def (parser-check? ps type)
  (let ((tok (parser-peek ps)))
    (and (token? tok) (eq? (token-type tok) type))))

;; Check if next token is a word with specific value
(def (parser-check-word? ps word)
  (let ((tok (parser-peek ps)))
    (and (token? tok)
         (eq? (token-type tok) 'WORD)
         (string=? (token-value tok) word))))

;; Expect a specific word, error if not found
(def (parser-expect-word! ps word)
  (let ((tok (parser-next! ps)))
    (unless (and (token? tok)
                 (eq? (token-type tok) 'WORD)
                 (string=? (token-value tok) word))
      (error (format "parse error: expected '~a', got ~a"
                     word (if (token? tok) (token-value tok) tok))))))

;; Skip newline tokens
(def (skip-newlines! ps)
  (let loop ()
    (when (parser-check? ps 'NEWLINE)
      (parser-consume! ps)
      (loop))))

;; Check if at end of input or separator
(def (at-end-or-sep? ps)
  (let ((tok (parser-peek ps)))
    (or (eq? tok 'eof)
        (not (token? tok)))))

;;; --- Grammar productions ---

;; list : and_or ((';' | '&') and_or)* [';' | '&']
(def (parse-list ps)
  (skip-newlines! ps)
  (let ((first (parse-and-or ps)))
    (if (not first)
      #f
      (let loop ((items [(cons 'sequential first)]))
        (let ((tok (parser-peek ps)))
          (cond
            ;; Semicolon — sequential
            ((parser-check? ps 'SEMI)
             (parser-consume! ps)
             (skip-newlines! ps)
             (let ((next (parse-and-or ps)))
               (if next
                 (loop (cons (cons 'sequential next) items))
                 (make-command-list (reverse items)))))
            ;; Ampersand — background
            ((parser-check? ps 'AMP)
             (parser-consume! ps)
             ;; Mark the last item as background
             (let ((updated (cons (cons 'background (cdar items)) (cdr items))))
               (skip-newlines! ps)
               (let ((next (parse-and-or ps)))
                 (if next
                   (loop (cons (cons 'sequential next) updated))
                   (make-command-list (reverse updated))))))
            ;; Newline — continue looking
            ((parser-check? ps 'NEWLINE)
             (parser-consume! ps)
             (skip-newlines! ps)
             (let ((next (parse-and-or ps)))
               (if next
                 (loop (cons (cons 'sequential next) items))
                 (make-command-list (reverse items)))))
            (else
             (if (= (length items) 1)
               ;; Single command — unwrap
               (cdar items)
               (make-command-list (reverse items))))))))))

;; and_or : pipeline (('&&' | '||') newline* pipeline)*
(def (parse-and-or ps)
  (let ((first (parse-pipeline ps)))
    (if (not first)
      #f
      (let loop ((rest []))
        (cond
          ((parser-check? ps 'AND_IF)
           (parser-consume! ps)
           (skip-newlines! ps)
           (let ((next (parse-pipeline ps)))
             (if next
               (loop (cons (cons 'and next) rest))
               (error "parse error: expected command after &&"))))
          ((parser-check? ps 'OR_IF)
           (parser-consume! ps)
           (skip-newlines! ps)
           (let ((next (parse-pipeline ps)))
             (if next
               (loop (cons (cons 'or next) rest))
               (error "parse error: expected command after ||"))))
          (else
           (if (null? rest)
             first
             (make-and-or-list first (reverse rest)))))))))

;; pipeline : ['!'] command ('|' newline* command)*
(def (parse-pipeline ps)
  (let* ((bang? (and (or (parser-check-word? ps "!")
                         (parser-check? ps 'BANG))
                     (begin (parser-consume! ps) #t)))
         (first (parse-command ps)))
    (if (not first)
      (if bang? (error "parse error: expected command after !") #f)
      (let loop ((cmds [first]))
        (cond
          ((or (parser-check? ps 'PIPE) (parser-check? ps 'PIPEAMP))
           (let ((pipe-type (token-type (parser-next! ps))))
             (skip-newlines! ps)
             (let ((next (parse-command ps)))
               (if next
                 (loop (cons next cmds))
                 (error "parse error: expected command after |")))))
          (else
           (let ((commands (reverse cmds)))
             (if (and (= (length commands) 1) (not bang?))
               (car commands)
               (make-ast-pipeline commands bang?)))))))))

;; command : compound_command redirect*
;;         | function_def
;;         | simple_command
(def (parse-command ps)
  (let ((tok (parser-peek ps)))
    (cond
      ((not (token? tok)) #f)
      ((eq? tok 'eof) #f)
      ;; Compound commands
      ((parser-check-word? ps "{")
       (parse-brace-group ps))
      ((parser-check? ps 'LPAREN)
       (parse-subshell ps))
      ((parser-check-word? ps "if")
       (parse-if ps))
      ((parser-check-word? ps "while")
       (parse-while ps))
      ((parser-check-word? ps "until")
       (parse-until ps))
      ((parser-check-word? ps "for")
       (parse-for ps))
      ((parser-check-word? ps "case")
       (parse-case ps))
      ((parser-check-word? ps "select")
       (parse-select ps))
      ((parser-check-word? ps "function")
       (parse-function-def-keyword ps))
      ;; Check for function def: word ( )
      ;; (handled in simple-command when we see WORD LPAREN RPAREN)
      (else
       (parse-simple-command ps)))))

;;; --- Simple command ---

;; simple_command : cmd_prefix? WORD cmd_suffix?
;; cmd_prefix : (ASSIGNMENT_WORD | redirect)+
;; cmd_suffix : (WORD | redirect)+
(def (parse-simple-command ps)
  (let/cc return  ;; early return for function definitions
  (let ((assignments [])
        (words [])
        (redirections []))
    ;; Parse prefix: assignments and redirections before command word
    (let prefix-loop ()
      (let ((tok (parser-peek ps)))
        (cond
          ((parser-check? ps 'ASSIGNMENT_WORD)
           (let ((tok (parser-next! ps)))
             (set! assignments (cons (parse-assignment-token tok) assignments))
             (prefix-loop)))
          ((redirect-token? tok)
           (set! redirections (cons (parse-redirect! ps) redirections))
           (prefix-loop))
          (else #!void))))
    ;; Parse command word and suffix
    (let ((tok (parser-peek ps)))
      (when (and (token? tok)
                 (or (eq? (token-type tok) 'WORD)
                     (eq? (token-type tok) 'IO_NUMBER)))
        ;; Check for function definition: name ( )
        (when (and (eq? (token-type tok) 'WORD)
                   (not (reserved-word? (token-value tok))))
          (let ((word-tok (parser-next! ps)))
            (set! words (cons (token-value word-tok) words))
            ;; Check for function def
            (when (parser-check? ps 'LPAREN)
              (parser-consume! ps)  ;; (
              (when (parser-check? ps 'RPAREN)
                (parser-consume! ps)  ;; )
                ;; This is a function definition
                (skip-newlines! ps)
                (let ((body (parse-command ps))
                      (redirs (parse-redirect-list ps)))
                  (return
                   (make-function-def
                    (token-value word-tok)
                    body
                    redirs)))))
            ;; Parse suffix: more words and redirections
            (let suffix-loop ()
              (let ((tok (parser-peek ps)))
                (cond
                  ((and (token? tok)
                        (or (and (eq? (token-type tok) 'WORD)
                                 (not (reserved-word? (token-value tok))))
                            ;; ASSIGNMENT_WORD as argument to builtins like
                            ;; local, export, readonly, declare
                            (eq? (token-type tok) 'ASSIGNMENT_WORD)))
                   (set! words (cons (token-value (parser-next! ps)) words))
                   (suffix-loop))
                  ((redirect-token? tok)
                   (set! redirections (cons (parse-redirect! ps) redirections))
                   (suffix-loop))
                  (else #!void))))))))
    ;; Build result
    (if (and (null? words) (null? assignments) (null? redirections))
      #f
      (make-simple-command
       (reverse assignments)
       (reverse words)
       (reverse redirections))))))

;; Parse an assignment token "NAME=VALUE" or "NAME+=VALUE"
(def (parse-assignment-token tok)
  (let* ((word (token-value tok))
         (eq-pos (string-find-eq word)))
    (if (and eq-pos (> eq-pos 0)
             (char=? (string-ref word (- eq-pos 1)) #\+))
      ;; NAME+=VALUE
      (make-assignment
       (substring word 0 (- eq-pos 1))
       (substring word (+ eq-pos 1) (string-length word))
       '+=)
      ;; NAME=VALUE
      (make-assignment
       (substring word 0 eq-pos)
       (substring word (+ eq-pos 1) (string-length word))
       '=))))

(def (string-find-eq str)
  (let loop ((i 0))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) #\=) i)
      (else (loop (+ i 1))))))

;;; --- Redirections ---

(def (redirect-token? tok)
  (and (token? tok)
       (memq (token-type tok)
             '(LESS GREAT DGREAT DLESS DLESSDASH TLESS
               LESSAND GREATAND LESSGREAT CLOBBER
               AMPGREAT AMPGREAT_GREAT IO_NUMBER))))

(def (parse-redirect! ps)
  (let ((fd #f))
    ;; Optional IO_NUMBER prefix
    (when (parser-check? ps 'IO_NUMBER)
      (set! fd (string->number (token-value (parser-next! ps)))))
    (let ((op-tok (parser-next! ps)))
      (let ((op-type (token-type op-tok)))
        (let ((target-tok (parser-next! ps)))
          (make-redir
           (case op-type
             ((LESS) '<)
             ((GREAT) '>)
             ((DGREAT) '>>)
             ((CLOBBER) 'clobber)
             ((LESSAND) '<&)
             ((GREATAND) '>&)
             ((LESSGREAT) '<>)
             ((DLESS) '<<)
             ((DLESSDASH) '<<-)
             ((TLESS) '<<<)
             ((AMPGREAT) '&>)
             ((AMPGREAT_GREAT) '&>>)
             (else '>))
           fd
           (if (token? target-tok) (token-value target-tok) "")))))))

(def (parse-redirect-list ps)
  (let loop ((redirs []))
    (if (redirect-token? (parser-peek ps))
      (loop (cons (parse-redirect! ps) redirs))
      (reverse redirs))))

;;; --- Compound commands ---

;; { list ; }
(def (parse-brace-group ps)
  (parser-expect-word! ps "{")
  (let ((body (parse-list ps)))
    (skip-newlines! ps)
    (parser-expect-word! ps "}")
    (let ((redirs (parse-redirect-list ps)))
      (make-brace-group body))))

;; ( list )
(def (parse-subshell ps)
  (parser-consume! ps)  ;; skip (
  (let ((body (parse-list ps)))
    (skip-newlines! ps)
    (unless (parser-check? ps 'RPAREN)
      (error "parse error: expected ')'"))
    (parser-consume! ps)
    (let ((redirs (parse-redirect-list ps)))
      (make-subshell body))))

;; if list ; then list [elif list ; then list]* [else list] fi
(def (parse-if ps)
  (parser-expect-word! ps "if")
  (let loop ((clauses []))
    (let ((condition (parse-list ps)))
      (skip-newlines! ps)
      (parser-expect-word! ps "then")
      (let ((body (parse-list ps)))
        (skip-newlines! ps)
        (let ((new-clauses (cons (cons condition body) clauses)))
          (cond
            ((parser-check-word? ps "elif")
             (parser-consume! ps)
             (loop new-clauses))
            ((parser-check-word? ps "else")
             (parser-consume! ps)
             (let ((else-body (parse-list ps)))
               (skip-newlines! ps)
               (parser-expect-word! ps "fi")
               (make-if-command (reverse new-clauses) else-body)))
            (else
             (parser-expect-word! ps "fi")
             (make-if-command (reverse new-clauses) #f))))))))

;; while list ; do list ; done
(def (parse-while ps)
  (parser-expect-word! ps "while")
  (let ((test (parse-list ps)))
    (skip-newlines! ps)
    (parser-expect-word! ps "do")
    (let ((body (parse-list ps)))
      (skip-newlines! ps)
      (parser-expect-word! ps "done")
      (make-while-command test body))))

;; until list ; do list ; done
(def (parse-until ps)
  (parser-expect-word! ps "until")
  (let ((test (parse-list ps)))
    (skip-newlines! ps)
    (parser-expect-word! ps "do")
    (let ((body (parse-list ps)))
      (skip-newlines! ps)
      (parser-expect-word! ps "done")
      (make-until-command test body))))

;; for name [in word ...] ; do list ; done
(def (parse-for ps)
  (parser-expect-word! ps "for")
  (let ((name-tok (parser-next! ps)))
    (unless (and (token? name-tok) (eq? (token-type name-tok) 'WORD))
      (error "parse error: expected variable name after 'for'"))
    (let ((var-name (token-value name-tok)))
      (skip-newlines! ps)
      ;; Optional "in word..."
      (let ((words
             (if (parser-check-word? ps "in")
               (begin
                 (parser-consume! ps)
                 (let loop ((words []))
                   (let ((tok (parser-peek ps)))
                     (cond
                       ((or (parser-check? ps 'SEMI) (parser-check? ps 'NEWLINE))
                        (parser-consume! ps)
                        (reverse words))
                       ((and (token? tok) (eq? (token-type tok) 'WORD)
                             (not (string=? (token-value tok) "do")))
                        (loop (cons (token-value (parser-next! ps)) words)))
                       (else (reverse words))))))
               ;; No "in" clause — iterate over "$@"
               (begin
                 (when (parser-check? ps 'SEMI) (parser-consume! ps))
                 #f))))
        (skip-newlines! ps)
        (parser-expect-word! ps "do")
        (let ((body (parse-list ps)))
          (skip-newlines! ps)
          (parser-expect-word! ps "done")
          (make-for-command var-name words body))))))

;; case word in [[(] pattern [| pattern]* ) list ;;]* esac
(def (parse-case ps)
  (parser-expect-word! ps "case")
  (let ((word-tok (parser-next! ps)))
    (unless (and (token? word-tok) (eq? (token-type word-tok) 'WORD))
      (error "parse error: expected word after 'case'"))
    (let ((word (token-value word-tok)))
      (skip-newlines! ps)
      (parser-expect-word! ps "in")
      (skip-newlines! ps)
      (let loop ((clauses []))
        (cond
          ((parser-check-word? ps "esac")
           (parser-consume! ps)
           (make-case-command word (reverse clauses)))
          (else
           ;; Parse pattern list
           (when (parser-check? ps 'LPAREN) (parser-consume! ps))  ;; optional (
           (let pattern-loop ((patterns []))
             (let ((pat-tok (parser-next! ps)))
               (unless (and (token? pat-tok) (eq? (token-type pat-tok) 'WORD))
                 (error "parse error: expected pattern in case clause"))
               (let ((new-patterns (cons (token-value pat-tok) patterns)))
                 (if (parser-check? ps 'PIPE)
                   (begin (parser-consume! ps)
                          (pattern-loop new-patterns))
                   ;; Expect )
                   (begin
                     (unless (parser-check? ps 'RPAREN)
                       (error "parse error: expected ')' in case clause"))
                     (parser-consume! ps)
                     (skip-newlines! ps)
                     (let ((body (parse-list ps)))
                       (skip-newlines! ps)
                       ;; Expect ;; or ;& or ;;&
                       (let ((terminator
                              (cond
                                ((parser-check? ps 'DSEMI)
                                 (parser-consume! ps) 'break)
                                (else 'break))))
                         (skip-newlines! ps)
                         (loop (cons (make-case-clause
                                      (reverse new-patterns)
                                      body
                                      terminator)
                                     clauses)))))))))))))))

;; select name in word ... ; do list ; done
(def (parse-select ps)
  (parser-expect-word! ps "select")
  (let ((name-tok (parser-next! ps)))
    (unless (and (token? name-tok) (eq? (token-type name-tok) 'WORD))
      (error "parse error: expected variable name after 'select'"))
    (let ((var-name (token-value name-tok)))
      (skip-newlines! ps)
      (parser-expect-word! ps "in")
      (let loop ((words []))
        (let ((tok (parser-peek ps)))
          (cond
            ((or (parser-check? ps 'SEMI) (parser-check? ps 'NEWLINE))
             (parser-consume! ps)
             (skip-newlines! ps)
             (parser-expect-word! ps "do")
             (let ((body (parse-list ps)))
               (skip-newlines! ps)
               (parser-expect-word! ps "done")
               (make-select-command var-name (reverse words) body)))
            ((and (token? tok) (eq? (token-type tok) 'WORD)
                  (not (string=? (token-value tok) "do")))
             (loop (cons (token-value (parser-next! ps)) words)))
            (else
             (parser-expect-word! ps "do")
             (let ((body (parse-list ps)))
               (skip-newlines! ps)
               (parser-expect-word! ps "done")
               (make-select-command var-name (reverse words) body)))))))))

;; function name { body }
(def (parse-function-def-keyword ps)
  (parser-consume! ps)  ;; skip "function"
  (let ((name-tok (parser-next! ps)))
    (unless (and (token? name-tok) (eq? (token-type name-tok) 'WORD))
      (error "parse error: expected function name"))
    ;; Optional ()
    (when (parser-check? ps 'LPAREN)
      (parser-consume! ps)
      (unless (parser-check? ps 'RPAREN)
        (error "parse error: expected ')' after '(' in function definition"))
      (parser-consume! ps))
    (skip-newlines! ps)
    (let ((body (parse-command ps))
          (redirs (parse-redirect-list ps)))
      (make-function-def (token-value name-tok) body redirs))))

;;; --- Helpers ---
