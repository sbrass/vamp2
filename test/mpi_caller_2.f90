!> Setup of a request req without a load balancer.
program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use kinds, only: default

  use diagnostics
  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_t

  use request_base
  use request_caller

  use mpi_f08

  integer, parameter :: N_CHANNELS = 100
  real(default), dimension(N_CHANNELS) :: channel_weight = 0
  logical, dimension(N_CHANNELS) :: parallel_grid = .false.

  class(request_base_t), allocatable :: req
  type(request_t) :: request

  type(commandline_t) :: cmd

  integer :: n_workers, rank

  call MPI_INIT ()
  call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)

  call cmd%parse ()
  if (cmd%gdb_attach) then
     if (cmd%gdb_attach_rank >= n_workers) &
          call msg_fatal ("Cannot attach to rank outside of communicator.")
     if (cmd%gdb_attach_rank == rank) then
         call signal_print_pid_and_wait ()
      end if
     call MPI_BARRIER (MPI_COMM_WORLD)
  end if

  channel_weight(1:10) = 1
  channel_weight(11:40) = 2
  channel_weight(41:70) = 3
  channel_weight(71:100) = 1
  parallel_grid = channel_weight > 1
  channel_weight = channel_weight / sum (channel_weight)

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Setup request_caller_t without callbacks"
  write (ERROR_UNIT, "(A)") "* =================================================="

  write (ERROR_UNIT, "(A)") "* Initialize request caller object."
  allocate (request_caller_t :: req)
  select type (req)
  type is (request_caller_t)
     call req%init (MPI_COMM_WORLD, n_channels = N_CHANNELS)
     call req%update_balancer (channel_weight, parallel_grid)
  end select
  call req%write ()

  if (req%is_master ()) then
     write (ERROR_UNIT, "(A)") "* Handle workload on master."
     select type (req)
     type is (request_caller_t)
        call req%handle_workload ()
     end select
  else
     write (ERROR_UNIT, "(A)") "* Handle workload on slave."
     do
        call req%request_workload (request)
        if (request%terminate) exit
        write (ERROR_UNIT, "(A,1X,I3)") "RESOURCE", request%handler_id
        call req%release_workload (request)
     end do
  end if

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Finalization"
  write (ERROR_UNIT, "(A)") "* =================================================="

  call req%write (ERROR_UNIT)
  call MPI_FINALIZE ()
end program main
