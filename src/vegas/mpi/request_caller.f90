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
     procedure :: handle_workload => request_caller_handle_workload
     procedure :: request_workload => request_caller_request_workload
     procedure :: release_workload => request_caller_release_workload
     procedure :: handle_and_release_workload => request_caller_handle_and_release_workload
     procedure :: terminate => request_caller_terminate
  end type request_caller_t

  public :: request_caller_t
contains

  subroutine request_caller_init (req, comm, n_channels)
    class(request_caller_t), intent(out) :: req
    type(MPI_COMM), intent(in) :: comm
    integer, intent(in) :: n_channels
    call MPI_COMM_DUP (comm, req%comm)
    call MPI_COMM_SIZE (req%comm, req%n_workers)
    !! Exclude master rank (0) from set of workers.
    req%n_workers = req%n_workers - 1
    if (req%n_workers < 1) then
       call msg_fatal ("Cannot handle less than 2 ranks in a master/slave global queue.")
    end if
    req%n_channels = n_channels
    call req%state%init (comm, req%n_workers)
    call req%cache%init (comm)
    if (req%is_master ()) then
       call allocate_balancer ()
    end if
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
    write (u, "(A)") "[REQUEST_CALLER]"
    call req%base_write (u)
    call req%state%write (u)
  end subroutine request_caller_write

  subroutine request_caller_update_balancer (req, weight, parallel_grid)
    class(request_caller_t), intent(inout) :: req
    real(default), dimension(:), intent(in) :: weight
    logical, dimension(:), intent(in) :: parallel_grid
    !! \note bug if not allocated?
    if (.not. allocated (req%balancer)) return
    call req%handler%clear ()
    select type (balancer => req%balancer)
    type is (channel_balancer_t)
       call balancer%update_state(weight, parallel_grid)
    end select
  end subroutine request_caller_update_balancer

  subroutine request_caller_handle_workload (req)
    class(request_caller_t), intent(inout) :: req
    integer :: handler, tag, source, worker_id
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
          !! Formally differentiate between worker_id and source.
          worker_id = source
          if (.not. allocated (req%balancer)) tag = MPI_TAG_TERMINATE
          select case (tag)
          case (MPI_TAG_REQUEST)
             write (ERROR_UNIT, "(A)") "MPI_TAG_REQUEST"
             write (ERROR_UNIT, "(A,3(1X,I0))") "[REQUEST]", source, tag, handler
             write (ERROR_UNIT, "(A,1X,L1)") "--------->", req%balancer%is_assignable (source)
             if (req%balancer%is_assignable (worker_id)) then
                call req%balancer%assign_worker (worker_id, handler)
                if (.not. req%balancer%has_resource_group (handler)) then
                   call req%state%update_request (source, MPI_TAG_ASSIGN_SINGLE, handler)
                else
                   call req%state%update_request (source, MPI_TAG_ASSIGN_GROUP, handler)
                   call provide_request_group (handler, source)
                end if
             else
                write (ERROR_UNIT, "(A)") "---------> TERMINATE"
                call req%state%terminate (source)
             end if
          case (MPI_TAG_HANDLER_AND_RELEASE)
             write (ERROR_UNIT, "(A,2(1X,I0))") "MPI_TAG_HANDLER_AND_RELEASE", handler, source
             !! \note call_handler expects an worker_id ∈ {1, …, N} and shifts it to a rank index!
             !! We have to revert the effect beforehand.
             call req%call_handler (handler, source_rank = source)
             call req%balancer%free_worker (worker_id)
          case (MPI_TAG_RELEASE)
             write (ERROR_UNIT, "(A,1X,I0)") "MPI_TAG_RELEASE", source
             call req%balancer%free_worker (worker_id)
          case (MPI_TAG_TERMINATE)
             !! Allow workers to request their own termination.
             write (ERROR_UNIT, "(A,1X,I0)") "MPI_TAG_TERMINATE", source
             call req%state%terminate (source)
          case default
             write (msg_buffer, "(I6,1X,A,1X,I6,1X,A,1X,I0)") source, "INVALID TAG -> ", tag, "MSG", handler
             call msg_warning ()
          end select
       end do
       call req%state%receive_request ()
    end do
    call req%state%free_request ()
  contains
    subroutine provide_request_group (handler_id, dest_rank)
      integer, intent(in) :: handler_id
      integer, intent(in) :: dest_rank
      integer, dimension(:), allocatable :: rank
      !! Rank indices and worker indices are identical, as we skip the master worker deliberately,
      !! we can reuse the worker indices as rank indices.
      call req%balancer%get_resource_group (handler_id, rank)
      call req%state%provide_request_group (dest_rank, rank)
    end subroutine provide_request_group
  end subroutine request_caller_handle_workload

  subroutine request_caller_request_workload (req, request)
    class(request_caller_t), intent(inout) :: req
    type(request_t), intent(out) :: request
    type(MPI_STATUS) :: status
    call req%state%client_serve (request%handler_id, status)
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
       request%group = .true.
       call retrieve_request_group (request%handler_id)
       call req%cache%get_comm (request%comm)
       request%group_master = req%cache%is_master ()
       request%callback = req%cache%is_master ()
    case (MPI_TAG_TERMINATE)
       request%terminate = status%MPI_TAG == MPI_TAG_TERMINATE
    end select
  contains
    subroutine retrieve_request_group (handler_id)
      integer, intent(in) :: handler_id
      integer, dimension(:), allocatable :: rank
      !! Here, worker and rank indices are interchangeable.
      call req%state%retrieve_request_group (rank)
      call req%cache%update (handler_id, rank)
    end subroutine retrieve_request_group
  end subroutine request_caller_request_workload

  subroutine request_caller_release_workload (req, request)
    class(request_caller_t), intent(inout) :: req
    type(request_t), intent(in) :: request
    call req%state%client_free (request%handler_id, has_callback = request%group_master)
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

  subroutine request_caller_terminate (req)
    class(request_caller_t), intent(inout) :: req
    call req%state%client_terminate ()
  end subroutine request_caller_terminate
end module request_caller
