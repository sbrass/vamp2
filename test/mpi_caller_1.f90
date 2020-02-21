program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use kinds, only: default

  use request_base
  use request_caller
  use mpi_f08

  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_is_gdb_attach

  use diagnostics

  implicit none

  integer, parameter :: n_channels = 25
  logical, dimension(n_channels) :: parallel_grid = .false.
  real(default), dimension(n_channels) :: channel_weights = 1
  class(request_base_t), allocatable :: req

  call MPI_INIT ()

  channel_weights(3) = 4
  channel_weights(6) = 4
  channel_weights(9) = 15
  channel_weights(13) = 10
  channel_weights(14) = 10
  channel_weights(15) = 10
  channel_weights(25) = 4
  parallel_grid = channel_weights > 1
  channel_weights = channel_weights / sum (channel_weights)

  if (commandline_is_gdb_attach ()) then
     if (req%is_master ()) call signal_print_pid_and_wait ()
     call MPI_BARRIER (MPI_COMM_WORLD)
  end if

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Setup: request_caller_t"
  write (ERROR_UNIT, "(A)") "* =================================================="

  allocate (request_caller_t :: req)
  select type (req)
  type is (request_caller_t)
     call req%init (MPI_COMM_WORLD, n_channels)
     call req%update_balancer (channel_weights, parallel_grid)
  end select

  call req%write (ERROR_UNIT)

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Simulate iteration"
  write (ERROR_UNIT, "(A)") "* =================================================="

  channel_weights = 1
  channel_weights(3) = 4
  ! channel_weights(6) = 4
  channel_weights(9) = 15
  channel_weights(13) = 5
  channel_weights(14) = 5
  channel_weights(15) = 5
  ! channel_weights(25) = 4
  parallel_grid = channel_weights > 1
  channel_weights = channel_weights / sum (channel_weights)

  select type (req)
  type is (request_caller_t)
     call req%update_balancer (channel_weights, parallel_grid)
  end select

  call req%write (ERROR_UNIT)

  call MPI_FINALIZE ()
end program main
