module request_simple
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  use diagnostics

  use array_list

  use balancer_base
  use balancer_simple
  use request_base

  use mpi_f08

  implicit none

  private

  type, extends (request_base_t) :: request_simple_t
     integer :: n_workers = 0
     integer :: n_channels = 0
     logical, dimension(:), allocatable :: parallel_grid
   contains
     procedure :: init => request_simple_init
     procedure :: write => request_simple_write
     procedure :: update => request_simple_update
     procedure :: get_request_master => request_simple_get_request_master
     !! deferred.
     procedure :: request_workload => request_simple_request_workload
     procedure :: release_workload => request_simple_release_workload
     procedure :: handle_and_release_workload => request_simple_handle_and_release_workload
  end type request_simple_t

  public :: request_simple_t
contains
  subroutine request_simple_init (req, comm, n_channels)
    class(request_simple_t), intent(out) :: req
    type(MPI_COMM), intent(in) :: comm
    integer, intent(in) :: n_channels
    integer :: n_workers
    call req%base_init (comm)
    call MPI_COMM_SIZE (req%comm, req%n_workers)
    req%n_channels = n_channels
    allocate (req%parallel_grid (n_channels), source = .false.)
    call allocate_balancer ()
  contains
    subroutine allocate_balancer ()
      class(balancer_base_t), allocatable :: balancer
      allocate (balancer_simple_t :: balancer)
      select type (balancer)
      type is (balancer_simple_t)
         call balancer%init (n_workers = req%n_workers, n_resources = req%n_channels)
      end select
      call req%add_balancer (balancer)
    end subroutine allocate_balancer
  end subroutine request_simple_init

  !> Update number of channels and parallel grids.
  !!
  !! The simple request object does not utilize the request balancer, as the complexity of the request balancer is not required for the simple approach.
  !! The simple approach assigns each worker several channel by a modular mapping from the set of workers {0, …, N} to the set of channels {1, …, N_c}.
  !! Vetoing on those channel which have a parallel grid (check the definition in vegas.f90), where all workers are assigned.
  !!
  !! w = φ(c) = (c - 1) mod N, if not P(c), else ∀w to c.
  !!
  !! The information is stored in a dynamic-sized array list, which is filled, reversed and then used in a stack-like manner keeping track of the unassigned channels.
  !! Assigned and finished channels are then moved to the finished stack.
  subroutine request_simple_update (req, parallel_grid)
    class(request_simple_t), intent(inout) :: req
    logical, dimension(:), intent(in) :: parallel_grid
    integer :: me, worker
    call req%reset ()
    call MPI_COMM_RANK (req%comm, me)
    worker = SHIFT_RANK_TO_WORKER(me)
    select type (balancer => req%balancer)
    type is (balancer_simple_t)
       call balancer%update_state (worker, parallel_grid)
    end select
    req%parallel_grid = parallel_grid
  end subroutine request_simple_update

  subroutine request_simple_write (req, unit)
    class(request_simple_t), intent(in) :: req
    integer, intent(in), optional :: unit
    integer :: u, n_size
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A)") "[REQUEST_SIMPLE]"
    write (u, "(A,1X,I0)") "N_CHANNELS", req%n_channels
    write (u, "(A,1X,I0)") "N_WORKERS", req%n_workers
    n_size = min (25, req%n_channels)
    write (u, "(A,25(1X,L1))") "PARALLEL_GRID", req%parallel_grid(:n_size)
    call req%base_write (u)
  end subroutine request_simple_write

  integer function request_simple_get_request_master (req, channel) &
       result (rank)
    class(request_simple_t), intent(in) :: req
    integer, intent(in) :: channel
    if (.not. allocated (req%balancer)) then
       call msg_bug ("Error: Balancer is not allocated.")
    end if
    rank = shift_worker_to_rank (req%balancer%get_resource_master (channel))
    !! "Caveat emptor" hits here:
    !! The balancer returns either a valid worker id or (-1) depending on the associated resource (it must be active...)
    !! We have to check whether returned worker index is plausible.
  end function request_simple_get_request_master

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
    integer :: rank, worker_id
    call MPI_COMM_RANK (req%comm, rank)
    worker_id = shift_rank_to_worker (rank)
    if (.not. req%balancer%is_pending () &
         .or. .not. req%balancer%is_assignable (worker_id)) then
       request%terminate = .true.
       return
    end if
    call req%balancer%assign_worker (worker_id, request%handler_id)
    associate (channel => request%handler_id)
      if (req%parallel_grid (channel)) then
         request%comm = req%external_comm
         request%group = .true.
         !! The object communicator is master.
         request%group_master = req%get_request_master (channel)
         request%callback = req%is_master ()
      else
         request%comm = req%external_comm
         request%group = .false.
         request%group_master = .true.
         request%callback = .true.
      end if
    end associate
  end subroutine request_simple_request_workload

  subroutine request_simple_release_workload (req, request)
    class(request_simple_t), intent(inout) :: req
    type(request_t), intent(in) :: request
    integer :: rank, worker_id
    call MPI_COMM_RANK (req%comm, rank)
    worker_id = shift_rank_to_worker (rank)
    call req%balancer%free_worker (worker_id)
  end subroutine request_simple_release_workload

  subroutine request_simple_handle_and_release_workload (req, request)
    class(request_simple_t), intent(inout) :: req
    type(request_t), intent(in) :: request
    call req%call_client_handler (request%handler_id)
    call req%release_workload (request)
  end subroutine request_simple_handle_and_release_workload
end module request_simple
