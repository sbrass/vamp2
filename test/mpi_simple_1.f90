program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  use request_base
  use request_simple
  use mpi_f08

  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_t

  use diagnostics

  implicit none

  integer, parameter :: n_channels = 13
  logical, dimension(n_channels) :: parallel_grid = .false.
  class(request_base_t), allocatable :: req

  type(commandline_t) :: cmd
  integer :: n_workers, rank

  call MPI_INIT ()
  call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)

  parallel_grid(4) = .true.
  parallel_grid(7) = .true.
  parallel_grid(8) = .true.

  call cmd%parse ()
  if (cmd%gdb_attach) then
     if (cmd%gdb_attach_rank >= n_workers) &
          call msg_fatal ("Cannot attach to rank outside of communicator.")
     if (cmd%gdb_attach_rank == rank) then
        call signal_print_pid_and_wait ()
     end if
     call MPI_BARRIER (MPI_COMM_WORLD)
  end if

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Setup request_caller_t"
  write (ERROR_UNIT, "(A)") "* =================================================="

  allocate (request_simple_t :: req)
  select type (req)
  type is (request_simple_t)
     call req%init (MPI_COMM_WORLD, n_channels)
     call req%update (parallel_grid)
  end select

  call req%write (ERROR_UNIT)

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Simulate iteration"
  write (ERROR_UNIT, "(A)") "* =================================================="

  parallel_grid = .false.
  parallel_grid(2) = .true.
  parallel_grid(3) = .true.
  parallel_grid(4) = .true.
  parallel_grid(12) = .true.

  select type (req)
  type is (request_simple_t)
     call req%update (parallel_grid)
  end select

  call req%write (ERROR_UNIT)

  call MPI_FINALIZE ()
end program main
