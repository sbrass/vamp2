program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use vegas_uti
  use mpi_f08

  implicit none
  call MPI_INIT ()

  call vegas_1 (ERROR_UNIT)
  call vegas_2 (ERROR_UNIT)
  call vegas_3 (ERROR_UNIT)
  call vegas_4 (ERROR_UNIT)
  call vegas_5 (ERROR_UNIT)
  call vegas_6 (ERROR_UNIT)

  call MPI_FINALIZE ()
end program main
