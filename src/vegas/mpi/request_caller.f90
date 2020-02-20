module request_caller
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use kinds, only: default
  use diagnostics

  use request_base
  use balancer_base
  use channel_balancer
  use request_state
  use request_callback

  use mpi_f08 !NODEP!

  implicit none

  private

  type, extends (request_base_t):: request_caller_t
     private
     integer :: n_channels = 0
     integer :: n_workers = 0
     type(request_state_t) :: state
   contains
     procedure :: init => request_caller_init
     procedure :: write => request_caller_write
     procedure :: update_balancer => request_caller_update_balancer
     procedure, private :: provide_communicator_group => request_caller_provide_communicator_group
     procedure, private :: retrieve_communicator_group => request_caller_retrieve_communicator_group
     procedure :: handle_workload => request_caller_handle_workload
     procedure :: request_workload => request_caller_request_workload
     procedure :: release_workload => request_caller_release_workload
     procedure :: handle_and_release_workload => request_caller_handle_and_release_workload
  end type request_caller_t

  public :: request_caller_t
contains

  subroutine request_caller_init (req, comm, n_channels)
    class(request_caller_t), intent(out) :: req
    type(MPI_COMM), intent(in) :: comm
    integer, intent(in) :: n_channels
    req%comm = comm
    req%n_channels = n_channels
    call MPI_COMM_SIZE (comm, req%n_workers)
    !! Exclude master rank (0) from set of workers.
    req%n_workers = req%n_workers - 1
    if (req%n_workers < 1) then
       call msg_fatal ("Cannot handle less than 2 ranks in a master/slave global queue.")
    end if
    call req%state%init (comm, req%n_workers)
    call req%cache%init (comm)
    call allocate_balancer ()
  contains
    subroutine allocate_balancer ()
      class(balancer_base_t), allocatable :: balancer
      allocate (channel_balancer_t :: balancer)
      select type (balancer)
      type is (channel_balancer_t)
         call balancer%init (n_workers = req%n_workers, n_resources = req%n_channels)
      end select
      call req%add_balancer (balancer)
    end subroutine allocate_balancer
  end subroutine request_caller_init

  subroutine request_caller_write (req, unit)
    class(request_caller_t), intent(in) :: req
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    if (allocated (req%balancer)) call req%balancer%write (u)
    call req%handler%write ()
    !! Add Cache Write.
    !! Add State Write.
  end subroutine request_caller_write

  subroutine request_caller_update_balancer (req, weight, parallel_grid)
    class(request_caller_t), intent(inout) :: req
    real(default), dimension(:), intent(in) :: weight
    logical, dimension(:), intent(in) :: parallel_grid
    select type (balancer => req%balancer)
    type is (channel_balancer_t)
       call balancer%update_state(weight, parallel_grid)
    end select
  end subroutine request_caller_update_balancer

  subroutine request_caller_handle_workload (req)
    class(request_caller_t), intent(inout) :: req
    integer :: handler, tag, source
    if (.not. allocated (req%balancer)) then
       call msg_warning ("Request: Error occured, load balancer not allocated.&
          & Terminate all workers.")
       !! We postpone to stop the program here so we can terminate all workers gracefully.
       !! First, we receive their requests, then we overwrite their "original" tag to MPI_TAG_TERMINATE.
       !! Second, we iterate this, until all workers are terminated and return without doing any besides.
    end if
    call req%state%receive_request ()
    do while (.not. req%state%is_terminated ())
       call req%state%await_request ()
       do while (req%state%has_request ())
          call req%state%get_request (source, tag, handler)
          if (.not. allocated (req%balancer)) tag = MPI_TAG_TERMINATE
          select case (tag)
          case (MPI_TAG_REQUEST)
             print *, "[REQUEST]", source, tag, handler
             print *, "--------->", req%balancer%is_assignable (source)
             if (req%balancer%is_assignable (source)) then
                call req%balancer%assign_worker (source, handler)
                if (.not. req%balancer%has_resource_group (handler)) then
                   call req%state%update_request (source, MPI_TAG_ASSIGN_SINGLE, handler)
                else
                   call req%state%update_request (source, MPI_TAG_ASSIGN_GROUP, handler)
                   call req%provide_communicator_group (source, handler)
                end if
             else
                print *, "---------> TERMINATE"
                call req%state%terminate (source)
             end if
          case (MPI_TAG_HANDLER_AND_RELEASE)
             !! call req%call_handler (handler, source)
             call req%balancer%free_worker (source)
          case (MPI_TAG_RELEASE)
             call req%balancer%free_worker (source)
          case (MPI_TAG_TERMINATE)
             !! Allow workers to request their own termination.
             call req%state%terminate (source)
          case default
             write (msg_buffer, "(I6,1X,A,1X,I6)") source, "INVALID TAG -> ", tag
             call msg_warning ()
          end select
       end do
       call req%state%receive_request ()
    end do
    call req%state%free_request ()
  end subroutine request_caller_handle_workload

  subroutine request_caller_provide_communicator_group (req, source, handler)
    class(request_caller_t), intent(in) :: req
    integer, intent(in) :: source
    integer, intent(in) :: handler
    integer, dimension(:), allocatable :: worker
    call req%balancer%get_resource_group (handler, worker)
    call MPI_SEND (worker, size (worker), MPI_INTEGER, &
         source, MPI_TAG_COMMUNICATOR_GROUP, req%comm)
  end subroutine request_caller_provide_communicator_group

  subroutine request_caller_retrieve_communicator_group (req, handler)
    class(request_caller_t), intent(inout) :: req
    integer, intent(in) :: handler
    type(MPI_STATUS) :: status
    integer :: n_workers
    integer, dimension(:), allocatable :: worker
    call MPI_PROBE (0, MPI_TAG_COMMUNICATOR_GROUP, req%comm, status)
    call MPI_GET_COUNT(status, MPI_INTEGER, n_workers)
    allocate (worker (n_workers), source = 0)
    call MPI_RECV (worker, n_workers, MPI_INTEGER, &
         0, MPI_TAG_COMMUNICATOR_GROUP, req%comm, status)
    call req%cache%update (handler, worker)
  end subroutine request_caller_retrieve_communicator_group

  subroutine request_caller_request_workload (req, request)
    class(request_caller_t), intent(inout) :: req
    type(request_t), intent(out) :: request
    type(MPI_STATUS) :: status
    call MPI_SEND (MPI_EMPTY_HANDLER, 1, MPI_INTEGER, &
         0, MPI_TAG_REQUEST, req%comm)
    call MPI_RECV (request%handler_id, 1, MPI_INTEGER, &
         0, MPI_ANY_TAG, req%comm, status)
    request%terminate = .false.
    request%group = .false.
    request%callback = .false.
    request%comm = MPI_COMM_NULL
    select case (status%MPI_TAG)
    case (MPI_TAG_ASSIGN_SINGLE)
       !! Default to req's communicator.
       request%comm = req%comm
       request%group_master = .true.
       request%callback = .true.
    case (MPI_TAG_ASSIGN_GROUP)
       call req%retrieve_communicator_group (request%handler_id)
       request%group = .true.
       call req%cache%get_comm (request%comm)
       request%group_master = req%cache%is_master ()
       request%callback = req%cache%is_master ()
    case (MPI_TAG_TERMINATE)
       request%terminate = status%MPI_TAG == MPI_TAG_TERMINATE
    end select
  end subroutine request_caller_request_workload

  subroutine request_caller_release_workload (req, request)
    class(request_caller_t), intent(inout) :: req
    type(request_t), intent(in) :: request
    call MPI_SEND (request%handler_id, 1, MPI_INTEGER, &
         0, MPI_TAG_RELEASE, req%comm)
  end subroutine request_caller_release_workload

  subroutine request_caller_handle_and_release_workload (req, request)
    class(request_caller_t), intent(inout) :: req
    type(request_t), intent(in) :: request
    if (.not. req%handler%has_handler (request%handler_id)) then
       call msg_bug ("Request: Handler is not registered for this worker.")
    end if
    call req%release_workload (request)
    call req%call_client_handler (request%handler_id)
  end subroutine request_caller_handle_and_release_workload

end module request_caller
