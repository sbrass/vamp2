program main
  use iso_fortran_env, only: ERROR_UNIT
  use signal

  implicit none

  character(:), allocatable :: hostname

  call signal_get_hostname (hostname)

  call signal_signal (SIGINT, c_signal_handler_interrupt)
  write (ERROR_UNIT, "(A,1X,I6,1X,A,1X,A)") "PID", signal_getpid (), "ON", hostname
  call sleep (2)
  call signal_raise (SIGINT)
end program main
