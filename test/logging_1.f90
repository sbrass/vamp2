program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use logging

  use mpi_f08

  implicit none

  type (logging_t) :: file, unit

  character(len=256) :: buffer

  integer :: comm_size

  call MPI_INIT ()

  call file%init (filename = string ("logging_1"))

  call MPI_COMM_SIZE (MPI_COMM_WORLD, comm_size)
  write (buffer, "(A,1X,I0)") "Communicator size:", comm_size

  call file%append (string (buffer))
  call file%append (string (buffer))
  call file%clear ()
  call file%flush ()

  call MPI_FINALIZE ()
end program main

