program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use vamp2_uti

  use signal
  use test_utils

  implicit none

  call vamp2_1 (ERROR_UNIT)
  call vamp2_2 (ERROR_UNIT)
  call vamp2_3 (ERROR_UNIT)
  call vamp2_4 (ERROR_UNIT)
  call vamp2_5 (ERROR_UNIT)
end program main
