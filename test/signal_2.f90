program main
  use iso_fortran_env, only: ERROR_UNIT
  use signal

  implicit none

  call signal_signal (SIGINT, c_signal_handler_interrupt)
  call signal_raise (SIGINT)
end program main
