program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use vamp2_uti

  use signal
  use test_utils

  use mpi_f08
  implicit none

  integer :: rank

  call MPI_INIT ()
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)
  if (commandline_is_gdb_attach ()) then
     if (rank == 0) call signal_print_pid_and_wait ()
     call MPI_BARRIER (MPI_COMM_WORLD)
  end if

  call vamp2_1 (ERROR_UNIT)
  call vamp2_2 (ERROR_UNIT)
  call vamp2_3 (ERROR_UNIT)
  call vamp2_4 (ERROR_UNIT)
  call vamp2_5 (ERROR_UNIT)
  call MPI_FINALIZE ()
end program main
