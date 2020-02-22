!> Mock setup for the implementation in VAMP2.
program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use kinds, only: default

  use request_base
  use request_simple
  use request_caller
  use request_callback, only: request_handler_t
  use result_handler
  use mpi_f08

  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_t

  use rng_base
  use rng_stream

  use iterator

  use diagnostics

  implicit none

  integer, parameter :: n_channels = 13
  real(default), dimension(N_CHANNELS) :: channel_weight = 0
  logical, dimension(n_channels) :: parallel_grid = .false.
  type(result_t), dimension(n_channels), target :: result

  class(rng_t), allocatable :: rng
  type(iterator_t) :: channel_iter
  integer :: current_channel

  type(request_t) :: request
  class(request_base_t), allocatable :: req

  type(commandline_t) :: cmd
  integer :: n_workers, rank

  call MPI_INIT ()
  call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)

  channel_weight(1:10) = 1
  channel_weight(11:40) = 2
  channel_weight(41:70) = 3
  channel_weight(71:100) = 1
  parallel_grid = channel_weight > 1
  channel_weight = channel_weight / sum (channel_weight)

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
  write (ERROR_UNIT, "(A)") "* Setup request_base_t with callback"
  write (ERROR_UNIT, "(A)") "* =================================================="

  select case (cmd%method)
  case ("simple")
     allocate (request_simple_t :: req)
  case ("load")
     allocate (request_caller_t :: req)
  end select
  select type (req)
  type is (request_simple_t)
     call req%init (MPI_COMM_WORLD, n_channels)
     call req%update (parallel_grid)
     call init_all_handler (req)
     call call_all_handler (req)
  type is (request_caller_t)
     call req%init (MPI_COMM_WORLD, n_channels)
     call req%update_balancer (channel_weight, parallel_grid)
     call init_all_handler (req)
  end select

  call req%write (ERROR_UNIT)

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Allocate and initialize RNG"
  write (ERROR_UNIT, "(A)") "* =================================================="

  allocate (rng_stream_t :: rng)
  call rng%init ()

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Integrate"
  write (ERROR_UNIT, "(A)") "* =================================================="

  call channel_iter%init (1, n_channels, 1)
  if (req%is_master ()) then
     !! Handle request balancer differently.
     select type (req)
     type is (request_caller_t)
        request%terminate = .true.
        call update_iter_and_rng (request, channel_iter, rng)
        call req%handle_workload ()
     end select
  end if

  !! channel_iter is already drained for master.
  channel: do while (channel_iter%is_iterable ())
     call req%request_workload (request)
     call update_iter_and_rng (request, channel_iter, rng)
     if (request%terminate) exit channel
     if (request%group) call MPI_BARRIER (request%comm)
     current_channel = channel_iter%get_current () !! Serial
     !! Assert (current_channel == request%handler_id)
     write (ERROR_UNIT, "(A,1X,I0)") "INTEGRATE", current_channel
     if (request%group_master) then
        if (.not. req%is_master ()) &
             call allocate_handler (req, current_channel, result(current_channel))
        call req%handle_and_release_workload (request)
     else
        call req%release_workload (request)
     end if
     call channel_iter%next_step () !! Serial
  end do channel

  if (.not. req%is_master () .and. .not. request%terminate) then
     !! Sentinel against un-terminated worker.
     !! However, do not interfere with RNG (status of current channel is undefined)
     select type (req)
     type is (request_caller_t)
        call req%terminate ()
     end select
  end if

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Finalization"
  write (ERROR_UNIT, "(A)") "* =================================================="

  call req%await_handler ()
  call req%barrier ()
  call req%write (ERROR_UNIT)

  call MPI_FINALIZE ()
contains
  subroutine init_all_handler (req)
    class(request_base_t), intent(inout) :: req
    integer :: ch
    !! The master worker needs always all handler (callback objects)
    !! in order to perform the communication to the client handler (callbacks).
    if (.not. req%is_master ()) return
    do ch = 1, n_channels
       call allocate_handler (req, ch, result(ch))
    end do
  end subroutine init_all_handler

  subroutine call_all_handler (req)
    class(request_base_t), intent(inout) :: req
    integer :: ch
    if (.not. req%is_master ()) return
    do ch = 1, n_channels
       select type (req)
       type is (request_simple_t)
          call req%call_handler (handler_id = ch, &
               source_rank = req%get_request_master (ch))
       end select
    end do
  end subroutine call_all_handler

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
 
  !! Advance the random number generator for the skipped channels.
  !!
  !! We set current_channel = request%handler_id, hence, we need to advance
  !! the random number generator until th iterator returns the same channel.
  subroutine update_iter_and_rng (request, iter, rng)
    type(request_t), intent(in) :: request
    type(iterator_t), intent(inout) :: iter
    class(rng_t), intent(inout) :: rng
    advance: do while (iter%is_iterable ())
       !! Advance up to iterator%end when in terminate mode,
       !! else advance until we hit the previous channel (of request%handler_id):
       !! Proof: current_channel <= request%handler_id - 1
       if (.not. request%terminate) then
          if (iter%get_current () >= request%handler_id) &
               exit advance
       end if
       select type (rng)
       type is (rng_stream_t)
          write (ERROR_UNIT, "(A,1X,I0)") "ADVANCE", iter%get_current ()
          call rng%next_substream ()
       end select
       call iter%next_step ()
    end do advance
  end subroutine update_iter_and_rng
end program main
