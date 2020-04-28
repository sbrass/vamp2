program main
  use iso_fortran_env, only: ERROR_UNIT
  use signal, only: signal_get_hostname

  implicit none

  character(:), allocatable :: hostname
  call signal_get_hostname (hostname)
  write(ERROR_UNIT, "(A)") hostname
end program main
