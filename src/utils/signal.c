#include <sys/types.h>
#include <signal.h>

#include "c_signal.h"

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
