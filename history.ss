;;; history.ss — Command history for gsh

(export #t)
(import :std/sugar
        :std/format
        :std/pregexp
        :gsh/util)

;;; --- History state ---

(defstruct history-state
  (entries       ;; vector of strings (ring buffer)
   count         ;; number of entries added
   max-size      ;; $HISTSIZE
   file          ;; $HISTFILE path
   file-max      ;; $HISTFILESIZE
   control       ;; list of symbols: ignorespace ignoredups erasedups
   ignore-pats   ;; list of glob patterns (HISTIGNORE)
   )
  transparent: #t)

(def *history* #f)

;;; --- Public interface ---

(def (history-init! histfile histsize (filesize #f))
  (set! *history*
    (make-history-state
      (make-vector (or histsize 1000) #f)
      0
      (or histsize 1000)
      (or histfile "~/.gsh_history")
      (or filesize (* 2 (or histsize 1000)))
      []    ;; control
      []))  ;; ignore patterns
  (history-load!))

;; Add a line to history
(def (history-add! line)
  (when (and *history* (> (string-length (string-trim line)) 0))
    (let ((trimmed (string-trim line)))
      ;; Check HISTCONTROL
      (unless (history-should-ignore? trimmed)
        (let* ((h *history*)
               (idx (modulo (history-state-count h)
                            (history-state-max-size h))))
          (vector-set! (history-state-entries h) idx trimmed)
          (set! (history-state-count h) (+ 1 (history-state-count h))))))))

;; Get entry by absolute number (1-based)
(def (history-get n)
  (when *history*
    (let* ((h *history*)
           (total (history-state-count h))
           (max-sz (history-state-max-size h)))
      (if (and (> n 0) (<= n total))
        (let ((idx (modulo (- n 1) max-sz)))
          (vector-ref (history-state-entries h) idx))
        #f))))

;; Get entry by offset from end (0 = most recent)
(def (history-get-relative offset)
  (when *history*
    (let* ((h *history*)
           (total (history-state-count h)))
      (history-get (- total offset)))))

;; Search history for entries starting with prefix
(def (history-search prefix)
  (if (not *history*) []
      (let* ((h *history*)
             (entries (history-list)))
        (filter (lambda (entry)
                  (and (>= (string-length entry) (string-length prefix))
                       (string=? (substring entry 0 (string-length prefix)) prefix)))
                entries))))

;; Reverse search for entry containing substring
(def (history-search-reverse substr)
  (if (not *history*) #f
      (let* ((h *history*)
             (total (history-state-count h))
             (max-sz (history-state-max-size h))
             (start (min total max-sz)))
        (let loop ((i (- start 1)))
          (if (< i 0) #f
              (let* ((idx (modulo (- total 1 (- (- start 1) i)) max-sz))
                     (entry (vector-ref (history-state-entries h) idx)))
                (if (and entry (string-contains? entry substr))
                  entry
                  (loop (- i 1)))))))))

;; Get all history entries as a list (oldest first)
(def (history-list)
  (if (not *history*) []
      (let* ((h *history*)
             (total (history-state-count h))
             (max-sz (history-state-max-size h))
             (count (min total max-sz)))
        (let loop ((i 0) (result []))
          (if (>= i count)
            (reverse result)
            (let* ((abs-num (+ (max 1 (- total max-sz -1)) i))
                   (idx (modulo (- abs-num 1) max-sz))
                   (entry (vector-ref (history-state-entries h) idx)))
              (loop (+ i 1) (if entry (cons entry result) result))))))))

;; Total number of history entries
(def (history-count)
  (if *history* (min (history-state-count *history*)
                      (history-state-max-size *history*))
      0))

;; Clear all history
(def (history-clear!)
  (when *history*
    (let ((h *history*))
      (set! (history-state-entries h)
            (make-vector (history-state-max-size h) #f))
      (set! (history-state-count h) 0))))

;;; --- History expansion ---

;; Expand history references in a line: !!, !n, !-n, !str, !?str?
;; Returns expanded string or the original if no expansion
(def (history-expand line)
  (if (not *history*)
    line
    (let ((len (string-length line)))
      (let loop ((i 0) (result (open-output-string)) (changed? #f))
        (cond
          ((>= i len)
           (if changed?
             (get-output-string result)
             line))
          ;; Escaped !
          ((and (< (+ i 1) len)
                (char=? (string-ref line i) #\\)
                (char=? (string-ref line (+ i 1)) #\!))
           (display "!" result)
           (loop (+ i 2) result #t))
          ;; ! history expansion
          ((char=? (string-ref line i) #\!)
           (cond
             ;; !! = last command
             ((and (< (+ i 1) len) (char=? (string-ref line (+ i 1)) #\!))
              (let ((prev (history-get-relative 0)))
                (display (or prev "") result)
                (loop (+ i 2) result #t)))
             ;; !-N = N commands ago
             ((and (< (+ i 1) len) (char=? (string-ref line (+ i 1)) #\-))
              (let* ((num-end (find-number-end line (+ i 2)))
                     (num-str (substring line (+ i 2) num-end))
                     (n (string->number num-str)))
                (if n
                  (let ((entry (history-get-relative (- n 1))))
                    (display (or entry "") result)
                    (loop num-end result #t))
                  (begin (display "!" result)
                         (loop (+ i 1) result changed?)))))
             ;; !N = command number N
             ((and (< (+ i 1) len) (char-numeric? (string-ref line (+ i 1))))
              (let* ((num-end (find-number-end line (+ i 1)))
                     (num-str (substring line (+ i 1) num-end))
                     (n (string->number num-str)))
                (if n
                  (let ((entry (history-get n)))
                    (display (or entry "") result)
                    (loop num-end result #t))
                  (begin (display "!" result)
                         (loop (+ i 1) result changed?)))))
             ;; !string = most recent command starting with string
             ((and (< (+ i 1) len)
                   (not (char-whitespace? (string-ref line (+ i 1))))
                   (not (char=? (string-ref line (+ i 1)) #\=)))
              (let* ((word-end (find-word-end line (+ i 1)))
                     (prefix (substring line (+ i 1) word-end))
                     (matches (history-search prefix)))
                (if (pair? matches)
                  (begin (display (last matches) result)
                         (loop word-end result #t))
                  (error (format "!~a: event not found" prefix)))))
             (else
              (display "!" result)
              (loop (+ i 1) result changed?))))
          ;; ^old^new^ quick substitution
          ((and (= i 0) (char=? (string-ref line 0) #\^))
           (let* ((second (string-index-of line #\^ 1))
                  (third (if second (string-index-of line #\^ (+ second 1)) #f))
                  (old (and second (substring line 1 second)))
                  (new (and second
                            (if third
                              (substring line (+ second 1) third)
                              (substring line (+ second 1) len))))
                  (prev (history-get-relative 0)))
             (if (and old new prev)
               (let ((expanded (string-replace-first prev old new)))
                 (display expanded result)
                 (if third
                   (begin (display (substring line (+ third 1) len) result)
                          (get-output-string result))
                   (get-output-string result)))
               line)))
          ;; Regular character
          (else
           (display (string-ref line i) result)
           (loop (+ i 1) result changed?)))))))

;;; --- File I/O ---

(def (history-save!)
  (when *history*
    (let ((entries (history-list))
          (file (history-state-file *history*)))
      (with-catch
       (lambda (e) #!void)  ;; silently fail
       (lambda ()
         (call-with-output-file file
           (lambda (port)
             (for-each (lambda (entry) (display entry port) (newline port))
                       entries))))))))

(def (history-load!)
  (when *history*
    (let ((file (history-state-file *history*)))
      (with-catch
       (lambda (e) #!void)
       (lambda ()
         (when (file-exists? file)
           (let ((lines (read-file-lines file)))
             (for-each history-add! lines))))))))

;;; --- HISTCONTROL ---

(def (history-set-control! controls)
  (when *history*
    (set! (history-state-control *history*) controls)))

(def (history-should-ignore? line)
  (let ((controls (if *history* (history-state-control *history*) [])))
    (or
     ;; ignorespace: lines starting with space
     (and (memq 'ignorespace controls)
          (> (string-length line) 0)
          (char=? (string-ref line 0) #\space))
     ;; ignoredups: same as previous
     (and (memq 'ignoredups controls)
          (let ((prev (history-get-relative 0)))
            (and prev (string=? prev line)))))))

;;; --- Helpers ---

(def (find-number-end str start)
  (let loop ((i start))
    (if (and (< i (string-length str))
             (char-numeric? (string-ref str i)))
      (loop (+ i 1))
      i)))

(def (find-word-end str start)
  (let loop ((i start))
    (if (and (< i (string-length str))
             (not (char-whitespace? (string-ref str i))))
      (loop (+ i 1))
      i)))

(def (string-index-of str ch start)
  (let loop ((i start))
    (cond
      ((>= i (string-length str)) #f)
      ((char=? (string-ref str i) ch) i)
      (else (loop (+ i 1))))))

(def (string-replace-first str old new)
  (let ((pos (string-search str old)))
    (if pos
      (string-append (substring str 0 pos) new
                     (substring str (+ pos (string-length old)) (string-length str)))
      str)))

(def (string-search haystack needle)
  (let ((hlen (string-length haystack))
        (nlen (string-length needle)))
    (let loop ((i 0))
      (cond
        ((> (+ i nlen) hlen) #f)
        ((string=? (substring haystack i (+ i nlen)) needle) i)
        (else (loop (+ i 1)))))))

(def (string-trim str)
  (let* ((len (string-length str))
         (start (let loop ((i 0))
                  (if (and (< i len) (char-whitespace? (string-ref str i)))
                    (loop (+ i 1)) i)))
         (end (let loop ((i (- len 1)))
                (if (and (>= i start) (char-whitespace? (string-ref str i)))
                  (loop (- i 1)) (+ i 1)))))
    (substring str start end)))

(def (last lst)
  (if (null? (cdr lst)) (car lst) (last (cdr lst))))
