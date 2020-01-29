program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use vamp2_uti

  use mpi_f08
  implicit none

  call MPI_INIT ()
  call vamp2_1 (ERROR_UNIT)
  call vamp2_2 (ERROR_UNIT)
  call vamp2_3 (ERROR_UNIT)
  call vamp2_4 (ERROR_UNIT)
  call vamp2_5 (ERROR_UNIT)
  call MPI_FINALIZE ()
end program main
