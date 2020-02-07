program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
#define MPI 1

#ifdef MPI
  use request_base
  use request_simple
  use request_callback, only: request_handler_t
  use result_handler
  use mpi_f08

  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_is_gdb_attach
#endif

  use rng_base
  use rng_stream

  use iterator

  implicit none

  integer, parameter :: n_channels = 13
  logical, dimension(n_channels) :: parallel_grid = .false.
  type(result_t), dimension(n_channels), target :: result
  !! VAMP2
  type(iterator_t) :: channel_iter
  integer :: current_channel
  class(rng_t), allocatable :: rng
  !! MPI
  type(request_t) :: request
  class(request_base_t), allocatable :: req

#ifdef MPI
  call MPI_INIT ()
#endif

  parallel_grid(4) = .true.
  parallel_grid(7) = .true.
  parallel_grid(8) = .true.

  allocate (request_simple_t :: req)
  select type (req)
  type is (request_simple_t)
     call req%init (MPI_COMM_WORLD, n_channels, parallel_grid)
  end select

  if (commandline_is_gdb_attach ()) then
     if (req%is_master ()) call signal_print_pid_and_wait ()
     call MPI_BARRIER (MPI_COMM_WORLD)
  end if

  allocate (rng_stream_t :: rng)
  call rng%init ()

  !! TODO
  !! [X] Add all handlers on master and call callback.
  !! [ ] Add handler to group_master only.

#ifdef MPI
  !! The master worker needs always all handler (callback objects) in order to perform the communication to the client handler (callbacks).
  if (req%is_master ()) then
     call channel_iter%init (1, n_channels, 1)
     do while (channel_iter%is_iterable ())
        current_channel = channel_iter%get_current ()
        call allocate_handler (req, current_channel, result(current_channel))
        select type (req)
        type is (request_simple_t)
           call req%call_handler (current_channel, &
                req%map_channel_to_worker(current_channel))
        end select
        call channel_iter%next_step ()
     end do
  end if
#endif

  call channel_iter%init (1, n_channels, 1)
  channel: do while (channel_iter%is_iterable ())
     current_channel = channel_iter%get_current ()
#ifdef MPI
     call req%request_workload (request)
     !! Proof: current_channel ∈ {1, …, N_channels}.
     call advance_rng (request, channel_iter, rng)
     if (request%terminate) exit channel
     if (request%group) call MPI_BARRIER (request%comm)
     current_channel = request%handler_id
#endif
     write (ERROR_UNIT, "(A,1X,I0)") "INTEGRATE", current_channel
#ifdef MPI
     !! Callback handler on master already registered.
     if (request%group_master) then
        if (.not. req%is_master ()) &
             call allocate_handler (req, current_channel, result(current_channel))
        call req%handle_and_release_workload (request)
     else
        call req%release_workload (request)
     end if
#endif
     call channel_iter%next_step ()
  end do channel
#ifdef MPI
  call req%await_handler ()

  call MPI_FINALIZE ()
#endif
contains
  subroutine allocate_handler (req, handler_id, result)
    class(request_base_t), intent(inout) :: req
    integer, intent(in) :: handler_id
    type(result_t), intent(in), target :: result
    class(request_handler_t), pointer :: handler
    allocate (result_handler_t :: handler)
    select type (handler)
    type is (result_handler_t)
       call handler%init (result, result%get_n_requests (), handler_id)
    end select
    call req%add_handler (handler_id, handler)
  end subroutine allocate_handler

  !! Advance the random number generator for the skipped channels.
  !!
  !! We set current_channel = request%handler_id, hence, we need to advance
  !! the random number generator until th iterator returns the same channel.
  subroutine advance_rng (request, iter, rng)
    type(request_t), intent(in) :: request
    type(iterator_t), intent(inout) :: iter
    class(rng_t), intent(inout) :: rng
    advance: do while (iter%is_iterable ())
       !! Advance up to iterator%end when in terminate mode,
       !! else advance until we hit the previous channel (of request%handler_id):
       !! Proof: current_channel <= request%handler_id - 1
       if (.not. request%terminate) then
          if (.not. iter%get_current () < request%handler_id) &
               exit advance
       end if
       select type (rng)
       type is (rng_stream_t)
          write (ERROR_UNIT, "(A,1X,I0)") "ADVANCE", iter%get_current ()
          call rng%next_substream ()
       end select
       call iter%next_step ()
    end do advance
  end subroutine advance_rng
end program main
