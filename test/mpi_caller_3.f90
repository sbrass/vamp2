!> Setup of a request req without a load balancer.
program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use kinds, only: default

  use diagnostics
  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_t

  use request_base
  use request_caller
  use request_callback
  use result_handler

  use mpi_f08

  integer, parameter :: N_CHANNELS = 100
  real(default), dimension(N_CHANNELS) :: channel_weight = 0
  logical, dimension(N_CHANNELS) :: parallel_grid = .false.
  type(result_t), dimension(N_CHANNELS), target :: result

  class(request_base_t), allocatable :: req
  type(request_t) :: request

  type(commandline_t) :: cmd

  integer :: n_workers, rank

  call MPI_INIT ()
  call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Request Caller and Channel Balancer (/w callback)"
  write (ERROR_UNIT, "(A)") "* =================================================="

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
  write (ERROR_UNIT, "(A)") "* Setup"
  write (ERROR_UNIT, "(A)") "* =================================================="

  write (ERROR_UNIT, "(A)") "* Initialize request caller object."
  allocate (request_caller_t :: req)
  select type (req)
  type is (request_caller_t)
     call req%init (MPI_COMM_WORLD, n_channels = N_CHANNELS)
     call req%update_balancer (channel_weight, parallel_grid)
  end select

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Initialize all handler (master only)"
  write (ERROR_UNIT, "(A)") "* =================================================="

  call init_all_handler (req)

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
        if (request%group_master) then
           call allocate_handler (req, request%handler_id, result(request%handler_id))
           write (ERROR_UNIT, "(A,1X,I0)") "HANDLE_AND_RELEASE", request%handler_id
           call req%handle_and_release_workload(request)
        else
           write (ERROR_UNIT, "(A,1X,I0)") "RELEASE", request%handler_id
           call req%release_workload (request)
        end if
     end do
  end if

  call req%write (ERROR_UNIT)
  call req%await_handler ()

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Finalization"
  write (ERROR_UNIT, "(A)") "* =================================================="

  call req%write (ERROR_UNIT)
  call MPI_FINALIZE ()
contains
  subroutine init_all_handler (req)
    class(request_base_t), intent(inout) :: req
    integer :: ch, worker
    !! The master worker needs always all handler (callback objects)
    !! in order to perform the communication to the client handler (callbacks).
    if (.not. req%is_master ()) return
    do ch = 1, n_channels
       call allocate_handler (req, ch, result(ch))
    end do
  end subroutine init_all_handler

  subroutine allocate_handler (req, handler_id, result)
    class(request_base_t), intent(inout) :: req
    integer, intent(in) :: handler_id
    type(result_t), intent(in), target :: result
    class(request_handler_t), pointer :: handler
    allocate (result_handler_t :: handler)
    select type (handler)
    type is (result_handler_t)
       !! The handler interface can be anything suiting.
       !! However, the init procedure must call the allocate procedure internally.
       call handler%init (result, result%get_n_requests (), handler_id)
    end select
    call req%add_handler (handler_id, handler)
  end subroutine allocate_handler
end program main
