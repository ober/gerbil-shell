;;; ffi.ss — POSIX FFI bindings for gsh
;;; Provides: waitpid, dup2, setpgid, tcsetpgrp, umask, etc.
;;; All functions are standard POSIX libc — no external libraries needed.

(export #t)
(import :std/foreign)

(begin-ffi (ffi-waitpid-pid ffi-waitpid-status
            ffi-dup ffi-dup-above ffi-dup2 ffi-move-gambit-fds ffi-setpgid ffi-getpgid
            ffi-tcsetpgrp ffi-tcgetpgrp ffi-umask ffi-getuid ffi-geteuid
            ffi-getegid ffi-access
            ffi-isatty ffi-setsid ffi-pipe-raw ffi-close-fd
            ffi-open-raw ffi-mkfifo ffi-unlink ffi-getpid
            ffi-read-all-from-fd ffi-unsetenv ffi-strftime
            WNOHANG WUNTRACED WCONTINUED
            WIFEXITED WEXITSTATUS WIFSIGNALED WTERMSIG WIFSTOPPED WSTOPSIG)

  (c-declare #<<END-C
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>

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

/* Move Gambit's scheduler select_abort pipe fds to high fds (>= min_fd).
   This modifies the ___pstate_os structure directly so Gambit's scheduler
   continues to work but frees low fds for shell use. */
static int ___gambit_move_select_abort_fds(int min_fd) {
    ___processor_state ___ps = ___PSTATE;
    int old_rfd = ___ps->os.select_abort.reading_fd;
    int old_wfd = ___ps->os.select_abort.writing_fd;
    int new_rfd, new_wfd;

    /* Only move if they are below min_fd */
    if (old_rfd >= min_fd && old_wfd >= min_fd) return 0;

    new_rfd = fcntl(old_rfd, F_DUPFD, min_fd);
    if (new_rfd < 0) return -1;

    new_wfd = fcntl(old_wfd, F_DUPFD, min_fd);
    if (new_wfd < 0) { close(new_rfd); return -1; }

    /* Update Gambit's state */
    ___ps->os.select_abort.reading_fd = new_rfd;
    ___ps->os.select_abort.writing_fd = new_wfd;

    /* Close the old low fds */
    close(old_rfd);
    close(old_wfd);

    return 0;
}

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

  ;; dup-above — duplicate fd to fd >= min_fd using fcntl(F_DUPFD)
  ;; Used by save-fd to avoid conflicting with user-visible fds
  (define-c-lambda ffi-dup-above (int int) int
    "___return(fcntl(___arg1, F_DUPFD, ___arg2));")

  ;; dup2 — duplicate fd onto target fd
  (define-c-lambda ffi-dup2 (int int) int "dup2")

  ;; Move Gambit's internal scheduler pipe fds (select_abort) to high fds.
  ;; This frees fds 3-9 for user shell redirects (exec 3>, etc.)
  ;; Accesses ___PSTATE->os.select_abort.reading_fd and writing_fd
  ;; Returns 0 on success, -1 on error.
  (define-c-lambda ffi-move-gambit-fds (int) int
    "___return(___gambit_move_select_abort_fds(___arg1));")

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
  (define-c-lambda ffi-getegid () int "getegid")

  ;; File access check — access(path, mode) returns 0 on success
  ;; mode: R_OK=4, W_OK=2, X_OK=1, F_OK=0
  (define-c-lambda ffi-access (char-string int) int "access")

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

  ;; mkfifo — create a named pipe (FIFO)
  ;; mode: e.g. #o600
  (define-c-lambda ffi-mkfifo (char-string int) int "mkfifo")

  ;; unlink — remove a file/FIFO
  (define-c-lambda ffi-unlink (char-string) int "unlink")

  ;; getpid — current process ID
  (define-c-lambda ffi-getpid () int "getpid")

  ;; unsetenv — remove variable from OS environment
  (define-c-lambda ffi-unsetenv (char-string) int "unsetenv")

  ;; Read all bytes from a raw fd into a static buffer.
  ;; Returns the number of bytes read. Use ffi-get-read-buf to retrieve the string.
  ;; This avoids open-input-file "/dev/fd/N" which blocks on empty pipes in Gambit.
  (c-declare #<<END-C2
#define FFI_READ_BUF_SIZE (1024*1024)
static char _ffi_read_buf[FFI_READ_BUF_SIZE];
static size_t _ffi_read_buf_len = 0;

static int ffi_do_read_all(int fd) {
    _ffi_read_buf_len = 0;
    char chunk[4096];
    ssize_t n;
    while ((n = read(fd, chunk, sizeof(chunk))) > 0) {
        size_t avail = FFI_READ_BUF_SIZE - 1 - _ffi_read_buf_len;
        size_t copy = (size_t)n < avail ? (size_t)n : avail;
        memcpy(_ffi_read_buf + _ffi_read_buf_len, chunk, copy);
        _ffi_read_buf_len += copy;
        if (copy < (size_t)n) break;
    }
    _ffi_read_buf[_ffi_read_buf_len] = '\0';
    return (int)_ffi_read_buf_len;
}
END-C2
  )

  (define-c-lambda _ffi_read_all (int) int "ffi_do_read_all")
  (define-c-lambda _ffi_get_read_buf () char-string "___return(_ffi_read_buf);")

  (define (ffi-read-all-from-fd fd)
    (_ffi_read_all fd)
    (_ffi_get_read_buf))

  ;; strftime — format a time value
  (c-declare #<<END-STRFTIME
static char _strftime_buf[4096];
static char* ffi_do_strftime(const char* fmt, int epoch) {
    time_t t = (time_t)epoch;
    struct tm *tm = localtime(&t);
    if (!tm) { _strftime_buf[0] = '\0'; return _strftime_buf; }
    strftime(_strftime_buf, sizeof(_strftime_buf), fmt, tm);
    return _strftime_buf;
}
END-STRFTIME
  )
  (define-c-lambda _ffi_strftime (char-string int) char-string "ffi_do_strftime")
  (define (ffi-strftime fmt epoch) (_ffi_strftime fmt epoch))
)
