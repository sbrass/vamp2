program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  use request_base
  use request_simple
  use request_callback, only: request_handler_t
  use result_handler
  use mpi_f08

  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_t

  use diagnostics

  implicit none

  integer, parameter :: n_channels = 13
  logical, dimension(n_channels) :: parallel_grid = .false.
  type(result_t), dimension(n_channels), target :: result
  type(request_t) :: request
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
  write (ERROR_UNIT, "(A)") "* Setup request_caller_t with callback"
  write (ERROR_UNIT, "(A)") "* =================================================="

  allocate (request_simple_t :: req)
  select type (req)
  type is (request_simple_t)
     call req%init (MPI_COMM_WORLD, n_channels)
     call req%update (parallel_grid)
  end select

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Initialize all handler (master only)"
  write (ERROR_UNIT, "(A)") "* =================================================="

  call init_and_call_all_handler (req)
  call req%write (ERROR_UNIT)

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Integrate"
  write (ERROR_UNIT, "(A)") "* =================================================="

  do
     call req%request_workload (request)
     if (request%terminate) exit
     write (ERROR_UNIT, "(A,1X,I3)") "RESOURCE", request%handler_id
     if (request%group_master) then
        if (.not. req%is_master ()) &
             call allocate_handler (req, request%handler_id, result(request%handler_id))
        call req%handle_and_release_workload (request)
     else
        call req%release_workload (request)
     end if
  end do

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Finalization"
  write (ERROR_UNIT, "(A)") "* =================================================="

  call req%await_handler ()
  call req%write (ERROR_UNIT)
  call MPI_BARRIER (MPI_COMM_WORLD)

  call MPI_FINALIZE ()
contains
  subroutine init_and_call_all_handler (req)
    class(request_base_t), intent(inout) :: req
    integer :: ch
    !! The master worker needs always all handler (callback objects)
    !! in order to perform the communication to the client handler (callbacks).
    if (.not. req%is_master ()) return
    do ch = 1, n_channels
       call allocate_handler (req, ch, result(ch))
       select type (req)
       type is (request_simple_t)
          call req%call_handler (ch, &
               source_rank = req%get_request_master (ch))
       end select
    end do
  end subroutine init_and_call_all_handler

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
