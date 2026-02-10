#ifndef PTUI_NATIVE_H
#define PTUI_NATIVE_H

#include <stddef.h>
#include <sys/types.h>
#include <termios.h>

typedef struct {
  int valid;
  struct termios saved;
} ptui_tty_state;

int ptui_tty_enable_raw(int fd, ptui_tty_state* out);
int ptui_tty_restore(int fd, const ptui_tty_state* st);
int ptui_tty_get_winsz(int fd, int* rows, int* cols);
int ptui_fd_set_nonblocking(int fd, int nonblocking);
ssize_t ptui_fd_read(int fd, unsigned char* buf, size_t cap);
int ptui_signals_init(void);
int ptui_signals_fd(void);
int ptui_signals_read(int* signo);

#endif
