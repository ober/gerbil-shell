;;; ffi.ss — POSIX FFI bindings for gsh
;;; Provides: waitpid, dup2, setpgid, tcsetpgrp, umask, etc.
;;; All functions are standard POSIX libc — no external libraries needed.

(export #t)
(import :std/foreign)

(begin-ffi (ffi-waitpid-pid ffi-waitpid-status
            ffi-dup ffi-dup2 ffi-setpgid ffi-getpgid
            ffi-tcsetpgrp ffi-tcgetpgrp ffi-umask ffi-getuid ffi-geteuid
            ffi-isatty ffi-setsid ffi-pipe-raw ffi-close-fd
            ffi-open-raw
            WNOHANG WUNTRACED WCONTINUED
            WIFEXITED WEXITSTATUS WIFSIGNALED WTERMSIG WIFSTOPPED WSTOPSIG)

  (c-declare #<<END-C
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

/* We need to store the last waitpid result since we can't easily
   return multiple values from a single C call.
   Thread-local would be ideal but static is fine for a shell. */
static int _waitpid_last_status = 0;
static int _waitpid_last_pid = 0;

static int ffi_do_waitpid(int pid, int options) {
    int status = 0;
    int result = waitpid(pid, &status, options);
    if (result > 0) {
        _waitpid_last_pid = result;
        _waitpid_last_status = status;
    } else {
        _waitpid_last_pid = result;
        _waitpid_last_status = 0;
    }
    return result;
}

static int ffi_get_waitpid_status(void) {
    return _waitpid_last_status;
}

/* Pipe wrapper that stores both fds */
static int _pipe_fds[2] = {-1, -1};

static int ffi_do_pipe(void) {
    return pipe(_pipe_fds);
}

static int ffi_pipe_read_fd(void) { return _pipe_fds[0]; }
static int ffi_pipe_write_fd(void) { return _pipe_fds[1]; }

END-C
  )

  ;; Wait option flags
  (define WNOHANG 1)
  (define WUNTRACED 2)
  (define WCONTINUED 8)

  ;; Status decoding macros (matching POSIX W* macros)
  (define (WIFEXITED s) (= (bitwise-and s #x7f) 0))
  (define (WEXITSTATUS s) (arithmetic-shift (bitwise-and s #xff00) -8))
  (define (WIFSIGNALED s)
    (let ((lo (bitwise-and s #x7f)))
      (and (not (= lo 0)) (not (= lo #x7f)))))
  (define (WTERMSIG s) (bitwise-and s #x7f))
  (define (WIFSTOPPED s) (= (bitwise-and s #xff) #x7f))
  (define (WSTOPSIG s) (arithmetic-shift (bitwise-and s #xff00) -8))

  ;; waitpid — call then retrieve pid and status separately
  (define-c-lambda _ffi_do_waitpid (int int) int "ffi_do_waitpid")
  (define-c-lambda _ffi_get_waitpid_status () int "ffi_get_waitpid_status")

  ;; High-level: call waitpid and get (values pid raw-status)
  (define (ffi-waitpid-pid pid options)
    (_ffi_do_waitpid pid options))

  (define (ffi-waitpid-status)
    (_ffi_get_waitpid_status))

  ;; dup — duplicate fd, returns new fd (next available)
  (define-c-lambda ffi-dup (int) int "dup")

  ;; dup2 — duplicate fd onto target fd
  (define-c-lambda ffi-dup2 (int int) int "dup2")

  ;; close — close a raw file descriptor
  (define-c-lambda ffi-close-fd (int) int "close")

  ;; Process group management
  (define-c-lambda ffi-setpgid (int int) int "setpgid")
  (define-c-lambda ffi-getpgid (int) int "getpgid")

  ;; Terminal foreground group
  (define-c-lambda ffi-tcsetpgrp (int int) int "tcsetpgrp")
  (define-c-lambda ffi-tcgetpgrp (int) int "tcgetpgrp")

  ;; File creation mask
  (define-c-lambda ffi-umask (int) int "umask")

  ;; User IDs
  (define-c-lambda ffi-getuid () int "getuid")
  (define-c-lambda ffi-geteuid () int "geteuid")

  ;; Terminal check
  (define-c-lambda ffi-isatty (int) int "isatty")

  ;; Session management
  (define-c-lambda ffi-setsid () int "setsid")

  ;; Raw open — open a file and return raw fd
  ;; flags: O_RDONLY=0, O_WRONLY=1, O_RDWR=2, O_CREAT=64, O_TRUNC=512, O_APPEND=1024
  (define-c-lambda ffi-open-raw (char-string int int) int "open")

  ;; Raw pipe — returns 0 on success, -1 on error
  ;; After calling, retrieve fds with ffi-pipe-read-fd / ffi-pipe-write-fd
  (define-c-lambda _ffi_do_pipe () int "ffi_do_pipe")
  (define-c-lambda _ffi_pipe_read_fd () int "ffi_pipe_read_fd")
  (define-c-lambda _ffi_pipe_write_fd () int "ffi_pipe_write_fd")

  (define (ffi-pipe-raw)
    (let ((rc (_ffi_do_pipe)))
      (if (= rc 0)
        (values (_ffi_pipe_read_fd) (_ffi_pipe_write_fd))
        (error "pipe failed" rc))))
)
