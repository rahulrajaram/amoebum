#define _DEFAULT_SOURCE

#include "ptui_native.h"

#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <sys/ioctl.h>
#include <unistd.h>

static int ptui_signal_pipe[2] = {-1, -1};
static int ptui_signals_initialized = 0;

static void ptui_signal_handler(int signo) {
  int saved_errno = errno;
  if (ptui_signal_pipe[1] >= 0) {
    (void)write(ptui_signal_pipe[1], &signo, sizeof(signo));
  }
  errno = saved_errno;
}

int ptui_tty_enable_raw(int fd, ptui_tty_state* out) {
  struct termios raw;

  if (out == NULL) {
    errno = EINVAL;
    return -1;
  }

  out->valid = 0;
  memset(&out->saved, 0, sizeof(out->saved));

  if (tcgetattr(fd, &out->saved) != 0) {
    return -1;
  }

  raw = out->saved;
  cfmakeraw(&raw);

  if (tcsetattr(fd, TCSAFLUSH, &raw) != 0) {
    return -1;
  }

  out->valid = 1;
  return 0;
}

int ptui_tty_restore(int fd, const ptui_tty_state* st) {
  if (st == NULL || st->valid == 0) {
    return 0;
  }

  return tcsetattr(fd, TCSAFLUSH, &st->saved);
}

int ptui_tty_get_winsz(int fd, int* rows, int* cols) {
  struct winsize ws;

  if (rows == NULL || cols == NULL) {
    errno = EINVAL;
    return -1;
  }

  if (ioctl(fd, TIOCGWINSZ, &ws) != 0) {
    return -1;
  }

  *rows = (int)ws.ws_row;
  *cols = (int)ws.ws_col;
  return 0;
}

int ptui_fd_set_nonblocking(int fd, int nonblocking) {
  int flags = fcntl(fd, F_GETFL);
  if (flags < 0) {
    return -1;
  }

  if (nonblocking) {
    flags |= O_NONBLOCK;
  } else {
    flags &= ~O_NONBLOCK;
  }

  return fcntl(fd, F_SETFL, flags);
}

ssize_t ptui_fd_read(int fd, unsigned char* buf, size_t cap) {
  if (buf == NULL && cap > 0) {
    errno = EINVAL;
    return -1;
  }

  return read(fd, buf, cap);
}

int ptui_signals_init(void) {
  struct sigaction sa;

  if (ptui_signals_initialized) {
    return 0;
  }

  if (pipe(ptui_signal_pipe) != 0) {
    return -1;
  }
  if (fcntl(ptui_signal_pipe[0], F_SETFL, O_NONBLOCK) != 0 ||
      fcntl(ptui_signal_pipe[1], F_SETFL, O_NONBLOCK) != 0 ||
      fcntl(ptui_signal_pipe[0], F_SETFD, FD_CLOEXEC) != 0 ||
      fcntl(ptui_signal_pipe[1], F_SETFD, FD_CLOEXEC) != 0) {
    close(ptui_signal_pipe[0]);
    close(ptui_signal_pipe[1]);
    ptui_signal_pipe[0] = -1;
    ptui_signal_pipe[1] = -1;
    return -1;
  }

  memset(&sa, 0, sizeof(sa));
  sa.sa_handler = ptui_signal_handler;
  sa.sa_flags = SA_RESTART;
  if (sigemptyset(&sa.sa_mask) != 0) {
    close(ptui_signal_pipe[0]);
    close(ptui_signal_pipe[1]);
    ptui_signal_pipe[0] = -1;
    ptui_signal_pipe[1] = -1;
    return -1;
  }
  if (sigaction(SIGWINCH, &sa, NULL) != 0) {
    close(ptui_signal_pipe[0]);
    close(ptui_signal_pipe[1]);
    ptui_signal_pipe[0] = -1;
    ptui_signal_pipe[1] = -1;
    return -1;
  }
  if (sigaction(SIGINT, &sa, NULL) != 0) {
    close(ptui_signal_pipe[0]);
    close(ptui_signal_pipe[1]);
    ptui_signal_pipe[0] = -1;
    ptui_signal_pipe[1] = -1;
    return -1;
  }
  if (sigaction(SIGTERM, &sa, NULL) != 0) {
    close(ptui_signal_pipe[0]);
    close(ptui_signal_pipe[1]);
    ptui_signal_pipe[0] = -1;
    ptui_signal_pipe[1] = -1;
    return -1;
  }

  ptui_signals_initialized = 1;
  return 0;
}

int ptui_signals_fd(void) {
  return ptui_signal_pipe[0];
}

int ptui_signals_read(int* signo) {
  ssize_t n;

  if (signo == NULL) {
    errno = EINVAL;
    return -1;
  }
  if (ptui_signal_pipe[0] < 0) {
    errno = EINVAL;
    return -1;
  }

  n = read(ptui_signal_pipe[0], signo, sizeof(*signo));
  if (n == (ssize_t)sizeof(*signo)) {
    return 1;
  }
  if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK || errno == EINTR)) {
    return 0;
  }
  if (n == 0) {
    return 0;
  }
  if (n >= 0 && n < (ssize_t)sizeof(*signo)) {
    errno = EIO;
  }
  return -1;
}
