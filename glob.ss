;;; glob.ss — Pathname expansion (globbing) for gsh

(export #t)
(import :std/pregexp
        :std/sort
        :std/sugar
        :std/iter)

;;; --- Public interface ---

;; Check if a string contains unquoted glob metacharacters
(def (glob-pattern? str)
  (let loop ((i 0) (escaped? #f))
    (if (>= i (string-length str))
      #f
      (let ((ch (string-ref str i)))
        (cond
          (escaped? (loop (+ i 1) #f))
          ((char=? ch #\\) (loop (+ i 1) #t))
          ((or (char=? ch #\*) (char=? ch #\?) (char=? ch #\[))
           #t)
          (else (loop (+ i 1) #f)))))))

;; Match a glob pattern against a string
;; Returns #t if the string matches the pattern
(def (glob-match? pattern string)
  (let ((rx (glob-pattern->pregexp pattern)))
    (and (pregexp-match rx string) #t)))

;; Expand a glob pattern to a sorted list of matching file paths
;; Returns the original pattern (as a list) if no matches
(def (glob-expand pattern)
  (let ((matches (glob-expand-path pattern)))
    (if (null? matches)
      [pattern]  ;; no match: return pattern literally
      (sort matches string<?))))

;;; --- Glob-to-regex conversion ---

;; Convert a glob pattern to a pregexp string
(def (glob-pattern->pregexp pattern)
  (let ((rx (open-output-string)))
    (display "^" rx)
    (let loop ((i 0) (in-bracket? #f))
      (if (>= i (string-length pattern))
        (begin (display "$" rx)
               (pregexp (get-output-string rx)))
        (let ((ch (string-ref pattern i)))
          (cond
            ;; Inside bracket expression
            (in-bracket?
             (cond
               ((char=? ch #\])
                (display "]" rx)
                (loop (+ i 1) #f))
               ((char=? ch #\!)
                ;; [! at start of bracket = negation
                (display "^" rx)
                (loop (+ i 1) #t))
               (else
                (display (pregexp-quote-char ch) rx)
                (loop (+ i 1) #t))))
            ;; Backslash escape
            ((char=? ch #\\)
             (if (< (+ i 1) (string-length pattern))
               (begin
                 (display (pregexp-quote-char (string-ref pattern (+ i 1))) rx)
                 (loop (+ i 2) #f))
               (begin
                 (display "\\\\" rx)
                 (loop (+ i 1) #f))))
            ;; Glob metacharacters
            ((char=? ch #\*)
             ;; Check for **
             (if (and (< (+ i 1) (string-length pattern))
                      (char=? (string-ref pattern (+ i 1)) #\*))
               (begin
                 (display ".*" rx)  ;; ** matches across /
                 (loop (+ i 2) #f))
               (begin
                 (display "[^/]*" rx)  ;; * matches anything except /
                 (loop (+ i 1) #f))))
            ((char=? ch #\?)
             (display "[^/]" rx)
             (loop (+ i 1) #f))
            ((char=? ch #\[)
             (display "[" rx)
             (loop (+ i 1) #t))
            ;; Regex special chars that need escaping
            (else
             (display (pregexp-quote-char ch) rx)
             (loop (+ i 1) #f))))))))

;; Quote a single char for pregexp
(def (pregexp-quote-char ch)
  (let ((s (string ch)))
    (if (pregexp-match "[\\\\\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\{\\}]" s)
      (string-append "\\" s)
      s)))

;;; --- Path expansion ---

;; Expand a glob pattern against the filesystem
;; Handles path components separately: dir/pattern
(def (glob-expand-path pattern)
  (let ((parts (split-glob-path pattern)))
    (if (null? parts)
      []
      (let ((start (if (and (> (string-length pattern) 0)
                            (char=? (string-ref pattern 0) #\/))
                     "/"
                     ".")))
        (glob-expand-parts parts start (char=? (string-ref pattern 0) #\/))))))

;; Split a glob path into components
(def (split-glob-path path)
  (let loop ((i 0) (start 0) (parts []))
    (cond
      ((>= i (string-length path))
       (reverse (if (> i start)
                  (cons (substring path start i) parts)
                  parts)))
      ((char=? (string-ref path i) #\/)
       (loop (+ i 1) (+ i 1)
             (if (> i start)
               (cons (substring path start i) parts)
               parts)))
      (else
       (loop (+ i 1) start parts)))))

;; Expand a list of path components against the filesystem
(def (glob-expand-parts parts base absolute?)
  (if (null? parts)
    [(if absolute? base
         (if (string=? base ".") "" base))]
    (let* ((pattern (car parts))
           (rest (cdr parts))
           (entries (glob-match-dir base pattern)))
      (let loop ((entries entries) (results []))
        (if (null? entries)
          results
          (let* ((entry (car entries))
                 (full (if (string=? base ".")
                         entry
                         (if (string=? base "/")
                           (string-append "/" entry)
                           (string-append base "/" entry)))))
            (if (null? rest)
              (loop (cdr entries) (cons full results))
              ;; More components: recurse only if this is a directory
              (if (directory-exists? full)
                (loop (cdr entries)
                      (append (glob-expand-parts rest full absolute?) results))
                (loop (cdr entries) results)))))))))

;; Match entries in a directory against a glob pattern
(def (glob-match-dir dir pattern)
  (with-catch
   (lambda (e) [])
   (lambda ()
     (let* ((rx (glob-pattern->pregexp pattern))
            (entries (directory-files dir))
            (show-dots? (and (> (string-length pattern) 0)
                            (char=? (string-ref pattern 0) #\.))))
       (filter
        (lambda (entry)
          (and (or show-dots?
                   (not (char=? (string-ref entry 0) #\.)))
               (pregexp-match rx entry)))
        entries)))))

;; Check if path is a directory
(def (directory-exists? path)
  (with-catch
   (lambda (e) #f)
   (lambda ()
     (eq? (file-info-type (file-info path)) 'directory))))

;;; --- Extended glob patterns (extglob) ---
;; ?(pat) *(pat) +(pat) @(pat) !(pat)
;; These are deferred to Phase 14 (advanced features)

;;; --- Utility ---

;; Check if a string has any glob metacharacters (unescaped)
(def (needs-globbing? word)
  (glob-pattern? word))
