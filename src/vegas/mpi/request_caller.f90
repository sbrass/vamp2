module request_caller
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use diagnostics

  use request_balancer
  use request_state
  use request_callback

  use mpi_f08 !NODEP!

  implicit none

  private

  type :: request_t
     integer :: handler_id = 0
     logical :: terminate = .false.
     logical :: group = .false.
     logical :: callback = .false.
     type(MPI_COMM) :: comm
  end type request_t

  type :: request_group_cache_t
     private
     type(MPI_COMM) :: parent_comm
     type(MPI_GROUP) :: parent_group
     type(MPI_COMM) :: comm
     type(MPI_GROUP) :: group
     integer, dimension(:), allocatable :: worker
   contains
     procedure :: init => request_group_cache_init
     procedure :: reset => request_group_cache_reset
     procedure :: update => request_group_cache_update
     procedure :: get_comm => request_group_cache_get_comm
  end type request_group_cache_t

  type :: request_caller_t
     private
     type(MPI_COMM) :: comm
     integer :: n_workers = 0
     class(request_balancer_t), allocatable :: balancer
     type(request_state_t) :: state
     type(request_group_cache_t) :: cache
     type(request_handler_manager_t) :: handler
   contains
     procedure :: init => request_caller_init
     procedure :: write => request_caller_write
     procedure :: add_balancer => request_caller_add_balancer
     procedure :: add_handler => request_caller_add_handler
     procedure :: await_handler => request_caller_await_handler
     procedure :: is_master => request_caller_is_master
     procedure :: get_n_workers => request_caller_get_n_workers
     procedure, private :: provide_communicator_group => request_caller_provide_communicator_group
     procedure, private :: retrieve_communicator_group => request_caller_retrieve_communicator_group
     procedure :: handle_workload => request_caller_handle_workload
     procedure :: request_workload => request_caller_request_workload
     procedure :: release_workload => request_caller_release_workload
     procedure :: handler_and_release_workload => request_caller_handler_and_release_workload
     procedure :: get_request_comm => request_caller_get_request_comm
     procedure :: is_request_master => request_caller_is_request_master
  end type request_caller_t

  public :: request_caller_t, request_t
contains

  subroutine request_caller_init (caller, comm)
    class(request_caller_t), intent(out) :: caller
    type(MPI_COMM), intent(in) :: comm
    caller%comm = comm
    call MPI_COMM_SIZE (comm, caller%n_workers)
    caller%n_workers = caller%n_workers - 1
    call caller%state%init (comm, caller%n_workers)
    call caller%cache%init (comm)
  end subroutine request_caller_init

  subroutine request_caller_write (caller, unit)
    class(request_caller_t), intent(in) :: caller
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    if (allocated (caller%balancer)) call caller%balancer%write (u)
    call caller%handler%write ()
    !! Add Cache Write.
    !! Add State Write.
  end subroutine request_caller_write

  subroutine request_caller_add_balancer (caller, balancer)
    class(request_caller_t), intent(inout) :: caller
    class(request_balancer_t), allocatable, intent(inout) :: balancer
    if (allocated (caller%balancer)) deallocate (caller%balancer)
    call move_alloc (balancer, caller%balancer)
    write (ERROR_UNIT, "(A)") "Add load balancer."
  end subroutine request_caller_add_balancer

  subroutine request_caller_add_handler (caller, handler_id, handler)
    class(request_caller_t), intent(inout) :: caller
    integer, intent(in) :: handler_id
    class(request_handler_t), pointer, intent(in) :: handler
    call caller%handler%add (handler_id, handler)
  end subroutine request_caller_add_handler

  subroutine request_caller_await_handler (caller)
    class(request_caller_t), intent(inout) :: caller
    call caller%handler%waitall ()
  end subroutine request_caller_await_handler

  integer function request_caller_get_n_workers (caller) result (n_workers)
    class(request_caller_t), intent(in) :: caller
    n_workers = caller%n_workers
  end function request_caller_get_n_workers

  logical function request_caller_is_master (caller) result (flag)
    class(request_caller_t), intent(in) :: caller
    integer :: rank
    call MPI_COMM_RANK (caller%comm, rank)
    flag = rank == 0
  end function request_caller_is_master

  subroutine request_caller_handle_workload (caller)
    class(request_caller_t), intent(inout) :: caller
    integer :: handler, tag, source
    if (.not. allocated (caller%balancer)) then
       call msg_warning ("Request: Error occured, load balancer not allocated.&
          & Terminate all workers.")
       !! We postpone to stop the program here so we can terminate all workers gracefully.
       !! First, we receive their requests, then we overwrite their "original" tag to MPI_TAG_TERMINATE.
       !! Second, we iterate this, until all workers are terminated and return without doing any besides.
    end if
    call caller%state%receive_request ()
    do while (.not. caller%state%is_terminated ())
       call caller%state%await_request ()
       do while (caller%state%has_request ())
          call caller%state%get_request (source, tag, handler)
          if (.not. allocated (caller%balancer)) tag = MPI_TAG_TERMINATE
          select case (tag)
          case (MPI_TAG_REQUEST)
             print *, "[REQUEST]", source, tag, handler
             print *, "--------->", caller%balancer%is_assignable (source)
             if (caller%balancer%is_assignable (source)) then
                call caller%balancer%assign_worker (source, handler)
                select case (caller%balancer%get_resource_mode (source))
                case (REQUEST_BALANCER_SINGLE)
                   call caller%state%update_request (source, MPI_TAG_ASSIGN_SINGLE, handler)
                case (REQUEST_BALANCER_GROUP)
                   call caller%state%update_request (source, MPI_TAG_ASSIGN_GROUP, handler)
                   call caller%provide_communicator_group (source, handler)
                end select
             else
                print *, "---------> TERMINATE"
                call caller%state%terminate (source)
             end if
          case (MPI_TAG_HANDLER_AND_RELEASE)
             call caller%handler%callback (handler, source, caller%comm)
             call caller%balancer%free_worker (source)
          case (MPI_TAG_RELEASE)
             call caller%balancer%free_worker (source)
          case (MPI_TAG_TERMINATE)
             !! Allow workers to request their own termination.
             call caller%state%terminate (source)
          case default
             write (msg_buffer, "(I6,1X,A,1X,I6)") source, "INVALID TAG -> ", tag
             call msg_warning ()
          end select
       end do
       call caller%state%receive_request ()
    end do
    call caller%state%free_request ()
  end subroutine request_caller_handle_workload

  subroutine request_caller_provide_communicator_group (caller, source, handler)
    class(request_caller_t), intent(in) :: caller
    integer, intent(in) :: source
    integer, intent(in) :: handler
    integer, dimension(:), allocatable :: worker
    call caller%balancer%get_resource_worker (handler, worker)
    call MPI_SEND (worker, size (worker), MPI_INTEGER, &
         source, MPI_TAG_COMMUNICATOR_GROUP, caller%comm)
  end subroutine request_caller_provide_communicator_group

  subroutine request_caller_retrieve_communicator_group (caller, handler)
    class(request_caller_t), intent(inout) :: caller
    integer, intent(in) :: handler
    type(MPI_STATUS) :: status
    integer :: n_workers
    integer, dimension(:), allocatable :: worker
    call MPI_PROBE (0, MPI_TAG_COMMUNICATOR_GROUP, caller%comm, status)
    call MPI_GET_COUNT(status, MPI_INTEGER, n_workers)
    allocate (worker (n_workers), source = 0)
    call MPI_RECV (worker, n_workers, MPI_INTEGER, &
         0, MPI_TAG_COMMUNICATOR_GROUP, caller%comm, status)
    call caller%cache%update (handler, worker)
  end subroutine request_caller_retrieve_communicator_group

  subroutine request_caller_request_workload (caller, request)
    class(request_caller_t), intent(inout) :: caller
    type(request_t), intent(out) :: request
    type(MPI_STATUS) :: status
    call MPI_SEND (MPI_EMPTY_HANDLER, 1, MPI_INTEGER, &
         0, MPI_TAG_REQUEST, caller%comm)
    call MPI_RECV (request%handler_id, 1, MPI_INTEGER, &
         0, MPI_ANY_TAG, caller%comm, status)
    request%terminate = .false.
    request%group = .false.
    request%callback = .false.
    request%comm = MPI_COMM_NULL
    select case (status%MPI_TAG)
    case (MPI_TAG_ASSIGN_SINGLE)
       !! Default to caller's communicator.
       request%comm = caller%comm
       request%callback = .true.
    case (MPI_TAG_ASSIGN_GROUP)
       call caller%retrieve_communicator_group (request%handler_id)
       request%group = .true.
       call caller%get_request_comm (request%comm)
       request%callback = caller%is_request_master (request)
    case (MPI_TAG_TERMINATE)
       request%terminate = status%MPI_TAG == MPI_TAG_TERMINATE
    end select
  end subroutine request_caller_request_workload

  subroutine request_caller_get_request_comm (caller, comm)
    class(request_caller_t), intent(in) :: caller
    type(MPI_COMM), intent(out) :: comm
    call caller%cache%get_comm (comm)
  end subroutine request_caller_get_request_comm

  function request_caller_is_request_master (caller, request) result (flag)
    class(request_caller_t), intent(in) :: caller
    type(request_t), intent(in) :: request
    logical :: flag
    integer :: request_comm_rank, ierror
    call MPI_COMM_RANK (request%comm, request_comm_rank, ierror)
    if (ierror /= 0) then
       call msg_bug ("Request: Error occured during check on request master.")
    end if
    flag = (request_comm_rank == 0)
  end function request_caller_is_request_master

  subroutine request_caller_release_workload (caller, request)
    class(request_caller_t), intent(inout) :: caller
    type(request_t), intent(in) :: request
    call MPI_SEND (request%handler_id, 1, MPI_INTEGER, &
         0, MPI_TAG_RELEASE, caller%comm)
  end subroutine request_caller_release_workload

  subroutine request_caller_handler_and_release_workload (caller, request)
    class(request_caller_t), intent(inout) :: caller
    type(request_t), intent(in) :: request
    if (.not. caller%handler%has_handler (request%handler_id)) then
       call msg_bug ("Request: Handler is not registered for this worker.")
    end if
    call MPI_SEND (request%handler_id, 1, MPI_INTEGER, &
         0, MPI_TAG_HANDLER_AND_RELEASE, caller%comm)
    call caller%handler%client_callback (request%handler_id, 0, caller%comm)
  end subroutine request_caller_handler_and_release_workload

  !!
  !! group cache
  !!

  subroutine request_group_cache_init (cache, comm)
    class(request_group_cache_t), intent(inout) :: cache
    type(MPI_COMM), intent(in) :: comm
    cache%parent_comm = comm
    !! Local operation.
    call MPI_COMM_GROUP (cache%parent_comm, cache%parent_group)
    cache%group = MPI_GROUP_EMPTY
    cache%comm = MPI_COMM_NULL
  end subroutine request_group_cache_init

  subroutine request_group_cache_reset (cache)
    class(request_group_cache_t), intent(out) :: cache
    cache%group = MPI_GROUP_EMPTY
    cache%comm = MPI_COMM_NULL
  end subroutine request_group_cache_reset

  subroutine request_group_cache_update (cache, tag, worker)
    class(request_group_cache_t), intent(inout) :: cache
    integer, intent(in) :: tag
    integer, dimension(:), allocatable, intent(inout) :: worker
    type(MPI_GROUP) :: group
    integer :: result, error
    call move_alloc (worker, cache%worker)
    print *, "CACHE UPDATE", cache%worker, "(", size (cache%worker), ")"
    call MPI_GROUP_INCL (cache%parent_group, size (cache%worker), cache%worker, group)
    call MPI_GROUP_COMPARE (cache%group, group, result)
    if (result == MPI_UNEQUAL) then
       cache%group = group
       if (cache%comm /= MPI_COMM_NULL) call MPI_COMM_FREE (cache%comm)
       !! Group-local operation. However, time consuming.
       call MPI_COMM_CREATE_GROUP (cache%parent_comm, cache%group, tag, &
            cache%comm, error)
       print *, cache%parent_comm, cache%comm
       if (error > 0) then
          write (ERROR_UNIT, "(A)") "Error occured during communicator creation..."
          stop 1
       end if
    else
       print *, "CACHE UPDATE: GROUPS ARE (NEARLY) IDENTICAL"
    end if
  end subroutine request_group_cache_update

  subroutine request_group_cache_get_comm (cache, comm)
    class(request_group_cache_t), intent(in) :: cache
    type(MPI_COMM), intent(out) :: comm
    comm = cache%comm
  end subroutine request_group_cache_get_comm
end module request_caller
