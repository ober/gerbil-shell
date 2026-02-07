;;; redirect.ss — File descriptor redirection for gsh
;;;
;;; Key insight: external commands launched via open-process with
;;; stdin/stdout/stderr-redirection: #f inherit REAL file descriptors (0,1,2),
;;; not Gambit port parameters. So we must manipulate both:
;;;   1. Real fds via ffi-dup/ffi-dup2/ffi-open-raw
;;;   2. Gambit port parameters for builtins

(export #t)
(import :std/sugar
        :std/format
        :gsh/ast
        :gsh/ffi
        :gsh/environment
        :gsh/expander)

;;; --- O_* flags for ffi-open-raw ---
(def O_RDONLY   0)
(def O_WRONLY   1)
(def O_RDWR     2)
(def O_CREAT   64)
(def O_TRUNC  512)
(def O_APPEND 1024)

;;; --- Saved state structure ---
;;; Each entry: (fd saved-real-fd saved-port)
;;;   fd           — which fd we redirected (0, 1, 2)
;;;   saved-real-fd — dup'd copy of the original real fd (for restore)
;;;   saved-port    — original Gambit port parameter value (for restore)

;;; --- Public interface ---

;; Apply a list of redirections, return saved state for restoration
;; Saved state is a flat list of (fd saved-real-fd saved-port) entries
(def (apply-redirections redirs env)
  (let loop ((redirs redirs) (saved []))
    (if (null? redirs)
      (reverse saved)
      (let* ((redir (car redirs))
             (result (apply-single-redirect! redir env)))
        (cond
          ((not result) (loop (cdr redirs) saved))
          ;; Multi-save: &> and &>> return a list of save entries
          ;; A save entry starts with an integer fd, a list-of-saves
          ;; starts with a list. Distinguish by checking (car result).
          ((integer? (car result))
           ;; Single save entry: (fd saved-real-fd saved-port)
           (loop (cdr redirs) (cons result saved)))
          (else
           ;; List of save entries: ((fd1 ...) (fd2 ...))
           (loop (cdr redirs) (append (reverse result) saved))))))))

;; Restore redirections from saved state
(def (restore-redirections saved)
  (for-each
   (lambda (entry)
     (let ((fd (car entry))
           (saved-real-fd (cadr entry))
           (saved-port (caddr entry)))
       ;; Restore the real fd
       (when (>= saved-real-fd 0)
         (ffi-dup2 saved-real-fd fd)
         (ffi-close-fd saved-real-fd))
       ;; Restore the Gambit port parameter
       (when saved-port
         (case fd
           ((0) (current-input-port saved-port))
           ((1) (current-output-port saved-port))
           ((2) (current-error-port saved-port))))))
   (reverse saved)))

;;; --- Single redirection ---

(def (apply-single-redirect! redir env)
  (let* ((op (redir-op redir))
         (fd (or (redir-fd redir) (default-fd-for-op op)))
         (target-str (expand-word-nosplit (redir-target redir) env)))
    (case op
      ;; < file — input from file
      ((<)
       (let ((save (save-fd fd)))
         (redirect-fd-to-file! fd target-str O_RDONLY 0)
         ;; Create Gambit port for builtins
         (when (= fd 0)
           (current-input-port (open-input-file target-str)))
         save))
      ;; > file — output to file (truncate)
      ((>)
       (let ((save (save-fd fd)))
         ;; Check noclobber
         (when (and (env-option? env "noclobber")
                    (file-exists? target-str))
           (fprintf (current-error-port) "gsh: ~a: cannot overwrite existing file~n" target-str)
           (restore-single! save)
           (error "cannot overwrite existing file"))
         (redirect-fd-to-file! fd target-str
                               (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                               #o666)
         (when (or (= fd 1) (= fd 2))
           (set-port-for-fd! fd target-str 'truncate))
         save))
      ;; >> file — output to file (append)
      ((>>)
       (let ((save (save-fd fd)))
         (redirect-fd-to-file! fd target-str
                               (bitwise-ior O_WRONLY O_CREAT O_APPEND)
                               #o666)
         (when (or (= fd 1) (= fd 2))
           (set-port-for-fd! fd target-str 'append))
         save))
      ;; clobber (>|) — output to file ignoring noclobber
      ((clobber)
       (let ((save (save-fd fd)))
         (redirect-fd-to-file! fd target-str
                               (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                               #o666)
         (when (or (= fd 1) (= fd 2))
           (set-port-for-fd! fd target-str 'truncate))
         save))
      ;; &> file — stdout+stderr to file
      ((&>)
       (let ((save1 (save-fd 1))
             (save2 (save-fd 2)))
         (redirect-fd-to-file! 1 target-str
                               (bitwise-ior O_WRONLY O_CREAT O_TRUNC)
                               #o666)
         ;; stderr → same as stdout
         (ffi-dup2 1 2)
         (let ((port (open-output-file [path: target-str truncate: #t])))
           (current-output-port port)
           (current-error-port port))
         ;; Return both saves
         (list save1 save2)))
      ;; &>> file — stdout+stderr append
      ((&>>)
       (let ((save1 (save-fd 1))
             (save2 (save-fd 2)))
         (redirect-fd-to-file! 1 target-str
                               (bitwise-ior O_WRONLY O_CREAT O_APPEND)
                               #o666)
         (ffi-dup2 1 2)
         (let ((port (open-output-file [path: target-str append: #t])))
           (current-output-port port)
           (current-error-port port))
         (list save1 save2)))
      ;; << heredoc — here-document
      ((<< <<-)
       (let ((save (save-fd 0)))
         ;; For heredoc, we use a pipe: write content, read from pipe
         (let-values (((read-fd write-fd) (ffi-pipe-raw)))
           ;; Write the heredoc content to the write end via /dev/fd
           (let ((write-port (open-output-file
                              (string-append "/dev/fd/" (number->string write-fd)))))
             (display target-str write-port)
             (force-output write-port)
             (close-port write-port))
           (ffi-close-fd write-fd)
           ;; Redirect stdin to read end
           (ffi-dup2 read-fd 0)
           (ffi-close-fd read-fd))
         (current-input-port (open-input-string target-str))
         save))
      ;; <<< word — here-string
      ((<<<)
       (let ((save (save-fd 0)))
         (let-values (((read-fd write-fd) (ffi-pipe-raw)))
           (let ((write-port (open-output-file
                              (string-append "/dev/fd/" (number->string write-fd)))))
             (display target-str write-port)
             (newline write-port)
             (force-output write-port)
             (close-port write-port))
           (ffi-close-fd write-fd)
           (ffi-dup2 read-fd 0)
           (ffi-close-fd read-fd))
         (current-input-port (open-input-string (string-append target-str "\n")))
         save))
      ;; >& n — dup fd (or close with >& -)
      ((>&)
       (cond
         ((string=? target-str "-")
          ;; Close fd
          (let ((save (save-fd fd)))
            (ffi-close-fd fd)
            save))
         (else
          (let ((target-fd (string->number target-str)))
            (if target-fd
              (let ((save (save-fd fd)))
                (ffi-dup2 target-fd fd)
                ;; Also update Gambit port
                (dup-gambit-port! target-fd fd)
                save)
              (begin
                (fprintf (current-error-port) "gsh: ~a: bad file descriptor~n" target-str)
                #f))))))
      ;; <& n — dup fd for input
      ((<&)
       (cond
         ((string=? target-str "-")
          (let ((save (save-fd fd)))
            (ffi-close-fd fd)
            save))
         (else
          (let ((target-fd (string->number target-str)))
            (if target-fd
              (let ((save (save-fd fd)))
                (ffi-dup2 target-fd fd)
                (dup-gambit-port! target-fd fd)
                save)
              (begin
                (fprintf (current-error-port) "gsh: ~a: bad file descriptor~n" target-str)
                #f))))))
      ;; <> file — open read-write on fd
      ((<>)
       (let ((save (save-fd fd)))
         (redirect-fd-to-file! fd target-str O_RDWR #o666)
         save))
      (else
       (fprintf (current-error-port) "gsh: unsupported redirect operator ~a~n" op)
       #f))))

;;; --- Helpers ---

(def (default-fd-for-op op)
  (case op
    ((< << <<- <<< <& <>) 0)
    ((> >> clobber >& &> &>>) 1)
    (else 0)))

;; Save the real fd + Gambit port for later restoration
;; Returns (fd saved-real-fd saved-port)
(def (save-fd fd)
  (let ((saved-real-fd (ffi-dup fd))
        (saved-port (case fd
                      ((0) (current-input-port))
                      ((1) (current-output-port))
                      ((2) (current-error-port))
                      (else #f))))
    (list fd saved-real-fd saved-port)))

;; Restore a single save entry
(def (restore-single! save)
  (let ((fd (car save))
        (saved-real-fd (cadr save))
        (saved-port (caddr save)))
    (when (>= saved-real-fd 0)
      (ffi-dup2 saved-real-fd fd)
      (ffi-close-fd saved-real-fd))
    (when saved-port
      (case fd
        ((0) (current-input-port saved-port))
        ((1) (current-output-port saved-port))
        ((2) (current-error-port saved-port))))))

;; Open a file and dup2 onto target fd
(def (redirect-fd-to-file! fd filename flags mode)
  (let ((raw-fd (ffi-open-raw filename flags mode)))
    (when (< raw-fd 0)
      (fprintf (current-error-port) "gsh: ~a: No such file or directory~n" filename)
      (error "cannot open file" filename))
    (unless (= raw-fd fd)
      (ffi-dup2 raw-fd fd)
      (ffi-close-fd raw-fd))))

;; Set Gambit port parameter for an output fd
(def (set-port-for-fd! fd filename mode)
  (let ((port (case mode
                ((truncate) (open-output-file [path: filename truncate: #t]))
                ((append) (open-output-file [path: filename append: #t])))))
    (case fd
      ((1) (current-output-port port))
      ((2) (current-error-port port)))))

;; Duplicate Gambit port from source-fd to dest-fd
(def (dup-gambit-port! source-fd dest-fd)
  (let ((source-port (case source-fd
                       ((0) (current-input-port))
                       ((1) (current-output-port))
                       ((2) (current-error-port))
                       (else #f))))
    (when source-port
      (case dest-fd
        ((0) (current-input-port source-port))
        ((1) (current-output-port source-port))
        ((2) (current-error-port source-port))))))

;; Convert redirections to open-process settings (kept for compatibility)
(def (redirections->process-settings redirs env)
  ;; With real fd redirection, we always use #f for stdin/stdout/stderr-redirection
  ;; to let the child inherit real fds. This function is kept for cases
  ;; where we need to know if redirections exist.
  [])
