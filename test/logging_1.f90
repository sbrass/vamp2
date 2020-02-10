program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use logging

  use mpi_f08

  implicit none

  type (logging_t) :: file, unit

  integer :: comm_size

  call MPI_INIT ()

  call file%init (filename = string ("logging_1"))

  call MPI_COMM_SIZE (MPI_COMM_WORLD, comm_size)
  write (LOG_BUFFER, "(A,1X,I0)") "Communicator size:", comm_size

  call file%append (string (LOG_BUFFER))
  call file%append (string (LOG_BUFFER))
  call file%clear ()
  call file%flush ()

  call MPI_FINALIZE ()
end program main

