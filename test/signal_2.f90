program main
  use iso_fortran_env, only: ERROR_UNIT
  use signal

  implicit none

  write (ERROR_UNIT, "(A)") "Sleep for 2.5 seconds."
  call signal_nanosleep (seconds = 2, nseconds = 500000000)
end program main
