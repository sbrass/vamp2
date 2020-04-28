#include <time.h>
#include <errno.h>
#include <string.h>

#include <sys/types.h>
#include <signal.h>
#include <unistd.h>

#include "c_signal.h"

#include <stdio.h>

// Convert our signal definition back to the standard signal definition.
// We cannot directly implement the standard signals into Fortran
// as the include "signal.h" in a Fortran file would lead to an compiler error.
int convert_c_signal(int sig) {
  int c_sig = 0;
  switch (sig) {
  case C_SIGINT:
    c_sig = SIGINT;
    break;
  }
  return c_sig;
}

void c_signal(int sig, void handler (int)) {
  signal(convert_c_signal(sig), handler);
}

int c_raise(int sig) {
  return raise(convert_c_signal(sig));
}

// Wrap hostname in order to handle errno with C
int c_gethostname(char * name, size_t len) {
  int ret_val = gethostname(name, len);
  if (ret_val == 0) return ret_val;
  printf("%s", strerror(errno));
  return errno;
}

void c_nanosleep(const int long seconds, const int long nanoseconds) {
  const struct timespec req = {
                               .tv_sec = seconds,
                               .tv_nsec = nanoseconds
  };
  struct timespec rem;
  int ret_val = nanosleep(&req, &rem);
  if (ret_val == 0) return;
  /* EFAULT Problem with copying information from user space. */

  /* EINTR  The pause has been interrupted by a signal that was delivered */
  /*        to the thread (see signal(7)).  The remaining sleep time has */
  /*        been written into *rem so that the thread can easily call */
  /*        nanosleep() again and continue with the pause. */

  /* EINVAL The value in the tv_nsec field was not in the range 0 to */
  /*        999999999 or tv_sec was negative. */
  switch (ret_val) {
  case EFAULT:
    printf("errno: EFAULT\n");
    break;
  case EINTR:
    printf("errno: EINTR\n");
    break;
  case EINVAL:
    printf("errno: EINVAL\n");
    break;
  }
}
