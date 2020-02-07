module request_simple
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  use array_list

  use request_base

  use mpi_f08

  implicit none

  private

  type, extends (request_base_t) :: request_simple_t
     integer :: n_workers = 0
     integer :: n_channels = 0
     logical, dimension(:), allocatable :: parallel_grid
     !! Use array list as dynamically-sized stack.
     type(array_list_t) :: channel_stack
     type(array_list_t) :: finished_stack
   contains
     procedure :: init => request_simple_init
     procedure :: write => request_simple_write
     procedure :: map_channel_to_worker => request_simple_map_channel_to_worker
     procedure :: request_workload => request_simple_request_workload
     procedure :: release_workload => request_simple_release_workload
     procedure :: handle_and_release_workload => request_simple_handle_and_release_workload
  end type request_simple_t

  public :: request_simple_t
contains
  subroutine request_simple_init (req, comm, n_channels, parallel_grid)
    class(request_simple_t), intent(out) :: req
    type(MPI_COMM), intent(in) :: comm
    integer, intent(in) :: n_channels
    logical, dimension(:), intent(in) :: parallel_grid
    call req%base_init (comm)
    call MPI_COMM_SIZE (comm, req%n_workers)
    req%n_channels = n_channels
    req%parallel_grid = parallel_grid
    call req%channel_stack%init ()
    call req%finished_stack%init ()
    call init_channel_stack ()
  contains
    !> Add each channel to the channel array list which is either a parallelizable grid,
    !! or the channels maps to the current rank.
    !! After filling the array list, we reverse the order of the array list,
    !! and use it in a quasi stack-like manne:r emoving an channel from the end of the list when requesting.
    subroutine init_channel_stack ()
      integer :: ch
      integer :: worker, rank
      call MPI_COMM_RANK (req%comm, rank)
      do ch = 1, req%n_channels
         if (parallel_grid(ch)) then
            call req%channel_stack%add (ch)
         else
            worker = req%map_channel_to_worker (ch)
            if (worker == rank) call req%channel_stack%add (ch)
         end if
      end do
      call req%channel_stack%reverse_order ()
    end subroutine init_channel_stack
  end subroutine request_simple_init

  subroutine request_simple_write (req, unit)
    class(request_simple_t), intent(in) :: req
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    call req%base_write (u)
    write (ERROR_UNIT, "(A)") "request_simple_write"
    call req%channel_stack%write (u)
    call req%finished_stack%write (u)
  end subroutine request_simple_write

  pure integer function request_simple_map_channel_to_worker (req, channel) &
       result (worker)
    class(request_simple_t), intent(in) :: req
    integer, intent(in) :: channel
    worker = mod (channel, req%n_workers)
  end function request_simple_map_channel_to_worker

  !> Request workload.
  !!
  !! Depending on parallel_grid, we fill the request object differently.
  !! First, we do not set commnuicator for .not. parallel_grid (group and group master are set to .false., also).
  !! And the callback needs to be executed.
  !! Second, for parallel_grid, we set req%comm to the associated communicator and set group to .true..
  !! However, the overall master has the grid's result, therefore, only the master needs to the callback.
  !! Remark: We can actually intercept the callback for the master to himself; the results are already in the current position.
  subroutine request_simple_request_workload (req, request)
    class(request_simple_t), intent(inout) :: req
    type(request_t), intent(out) :: request
    if (req%channel_stack%is_empty ()) then
       request%terminate = .true.
       return
    end if
    request%handler_id = req%channel_stack%remove () !! pop last element.
    associate (channel => request%handler_id)
      if (req%parallel_grid (channel)) then
         request%comm = req%comm
         request%group = .true.
         !! The object communicator is master.
         request%group_master = req%is_master ()
         request%callback = req%is_master ()
      else
         request%comm = MPI_COMM_NULL
         request%group = .false.
         request%group_master = .true.
         request%callback = .true.
      end if
    end associate
  end subroutine request_simple_request_workload

  subroutine request_simple_release_workload (req, request)
    class(request_simple_t), intent(inout) :: req
    type(request_t), intent(in) :: request
    call req%finished_stack%add (request%handler_id)
  end subroutine request_simple_release_workload

  subroutine request_simple_handle_and_release_workload (req, request)
    class(request_simple_t), intent(inout) :: req
    type(request_t), intent(in) :: request
    call req%call_client_handler (request%handler_id)
    call req%release_workload (request)
  end subroutine request_simple_handle_and_release_workload
end module request_simple
