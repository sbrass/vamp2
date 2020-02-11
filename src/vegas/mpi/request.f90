!! Communicator usage:
!! When a data type has an associated communicator field, it always must duplicate the communicator in order to ensure communication encapsulation.
!! Only in special case (e.g. outside of object communication is required) the parent communicator should be directly used.
!! For example, that is the case for the group cache, as it exports its newly created communicator to the outside of the request library.
module request_base
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  use diagnostics

  use request_callback
  use mpi_f08

  implicit none

  private

  type :: request_t
     integer :: handler_id = 0
     logical :: terminate = .false.
     logical :: group = .false.
     logical :: group_master = .false.
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
     procedure :: is_master => request_group_cache_is_master
  end type request_group_cache_t

  !> Base type for requesting and back calling.
  !!
  !! A short remark on the semantics of point-to-point communication (as its intended use in this implemenation):
  !! MPI is strict regarding the order of the messages (for blocking communication), as those can not overtake each other, p. 41.
  !! For non-blocking communication, the standard extends it such that the order depends on "the execution order of the calls that initiate the communication".
  !! The non-overtaking requirement is then extended to this definition of order.
  !!
  !! Color method for tracking communication?
  !! We restrict each slave to a single callback (at once), which can communicate during the next request computation.
  !! Before handling the next (current) callback, the former one has to finish (with a wait call on the client-side).
  !! On the server-side, we only test on the finishing, reporting a communication failure.
  !!
  !! Furthermore, the implementation has to take care that the order of the communication calls on the master and slave code always matches!
  !!
  !! Therefore, we need to secure the order of communication calls.
  !! First, we let the master initiate all callback communication *before* polling.
  !! This fixiates the order.
  !! Second, we require that the implementation of the polling honors this order.
  type, abstract :: request_base_t
     type(MPI_COMM) :: comm
     type(request_group_cache_t) :: cache
     type(request_handler_manager_t) :: handler
   contains
     procedure :: base_init => request_base_init
     procedure :: base_write => request_base_write
     procedure :: is_master => request_base_is_master
     procedure :: add_handler => request_base_add_handler
     procedure :: clear_handler => request_base_clear_handler
     procedure :: call_handler => request_base_call_handler
     procedure :: call_client_handler => request_base_call_client_handler
     procedure :: await_handler => request_base_await_handler
     procedure(request_base_request_workload), deferred :: request_workload
     procedure(request_base_release_workload), deferred :: release_workload
     procedure(request_base_handle_and_release_workload), deferred :: handle_and_release_workload
  end type request_base_t

  !> The basic idea behind the request mechanism is that each associated worker can request a workload either from a predefined local stack, from local stealing or from a global queue.
  !!
  !! We note that it is not necessary to differentiate between master and worker on this level of abstraction.
  !! Hence, the request interface ignores any notion regarding a possible parallelization concept.
  abstract interface
     !> Request workload and returns an request_t object.
     !!
     !! The request_t object has an associated handler_id and provide several ways
     !! to indicate whether the execution is to be terminated, or the request has an associated communictor.
     !! Finally, whether we expect that the handler id will be connected to an callback.
     !!
     !! \param[out] request Request container.
     subroutine request_base_request_workload (req, request)
       import :: request_base_t, request_t
       class(request_base_t), intent(inout) :: req
       type(request_t), intent(out) :: request
     end subroutine request_base_request_workload

     !> Release workload with the information from the request container.
     !!
     !! The release procedure may notify the master about the finishing of the workload associated with the handler_id.
     !! Or, it may just bookkeep whether the workload has finished.
     !! Additionally, if request%callback was true, it could handle the callback (from client side.)
     subroutine request_base_release_workload (req, request)
       import :: request_base_t, request_t
       class(request_base_t), intent(inout) :: req
       type(request_t), intent(in) :: request
     end subroutine request_base_release_workload

     !> Handle associated callback and release workload with the information from the request container.
     !!
     !! The procedure must call the associated callback handler using the handler_id.
     !! Remark: The callback manager is quite squishy regarding a missing handler (silent failure).
     !!         The procedure has to take care whether the callback was actually successful.
     !! The further release of the workload can then be deferred to the release_workload procedure.
     !! \param[in] request.
     subroutine request_base_handle_and_release_workload (req, request)
       import :: request_base_t, request_t
       class(request_base_t), intent(inout) :: req
       type(request_t), intent(in) :: request
     end subroutine request_base_handle_and_release_workload
  end interface

  public :: request_t, request_base_t
contains

  subroutine request_group_cache_init (cache, comm)
    class(request_group_cache_t), intent(inout) :: cache
    type(MPI_COMM), intent(in) :: comm
    call MPI_COMM_DUP (comm, cache%parent_comm)
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
    call MPI_GROUP_INCL (cache%parent_group, size (cache%worker), cache%worker, group)
    call MPI_GROUP_COMPARE (cache%group, group, result)
    if (result == MPI_UNEQUAL) then
       cache%group = group
       if (cache%comm /= MPI_COMM_NULL) call MPI_COMM_FREE (cache%comm)
       !! Group-local operation. However, time consuming.
       call MPI_COMM_CREATE_GROUP (cache%parent_comm, cache%group, tag, &
            cache%comm, error)
       if (error /= 0) then
          call msg_bug ("Error occured during communicator creation...")
       end if
    else
       call msg_message ("CACHE UPDATE: GROUPS ARE (NEARLY) IDENTICAL")
    end if
  end subroutine request_group_cache_update

  subroutine request_group_cache_get_comm (cache, comm)
    class(request_group_cache_t), intent(in) :: cache
    type(MPI_COMM), intent(out) :: comm
    comm = cache%comm
  end subroutine request_group_cache_get_comm

  logical function request_group_cache_is_master (cache) result (flag)
    class(request_group_cache_t), intent(in) :: cache
    integer :: rank, error
    call MPI_COMM_RANK (cache%comm, rank, error)
    if (error /= 0) then
       call msg_bug ("Error: Could not retrieve group rank.")
    end if
    flag = (rank == 0)
  end function request_group_cache_is_master

  !! =================================================
  !! request_base_t
  !! =================================================

  subroutine request_base_init (req, comm)
    class(request_base_t), intent(out) :: req
    type(MPI_COMM), intent(in) :: comm
    call MPI_COMM_DUP (comm, req%comm)
    call req%cache%init (comm)
    call req%handler%init (comm)
  end subroutine request_base_init

  subroutine request_base_write (req, unit)
    class(request_base_t), intent(in) :: req
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (ERROR_UNIT, "(A)") "request_base_write"
    call req%handler%write (u)
  end subroutine request_base_write

  !> Check whether current worker is master rank in object communicator.
  !!
  !! Do not confuse with a group's master !!!
  !! Proof: rank == 0
  logical function request_base_is_master (req) result (flag)
    class(request_base_t), intent(in) :: req
    integer :: rank, ierr
    call MPI_COMM_RANK (req%comm, rank, ierr)
    if (ierr /= 0) then
       write (ERROR_UNIT, "(A,1X,I0)") "MPI Error: request_base_is_master", ierr
       stop 1
    end if
    flag = (rank == 0)
  end function request_base_is_master

  subroutine request_base_add_handler (req, handler_id, handler)
    class(request_base_t), intent(inout) :: req
    integer, intent(in) :: handler_id
    class(request_handler_t), pointer, intent(in) :: handler
    call req%handler%add (handler_id, handler)
  end subroutine request_base_add_handler

  subroutine request_base_clear_handler (req)
    class(request_base_t), intent(inout) :: req
    call req%handler%clear ()
  end subroutine request_base_clear_handler

  !> Call handler for master communication for handler_id.
  !!
  !! \param[in] handler_id The associated key of the callback object.
  subroutine request_base_call_handler (req, handler_id, source)
    class(request_base_t), intent(inout) :: req
    integer, intent(in) :: handler_id
    integer, intent(in) :: source
    call req%handler%callback (handler_id, source)
  end subroutine request_base_call_handler

  !> Call handler for slave communication for handler_id.
  !!
  !! \param[in] handler_id The associated key of the callback object.
  subroutine request_base_call_client_handler (req, handler_id)
    class(request_base_t), intent(inout) :: req
    integer, intent(in) :: handler_id
    call req%handler%client_callback (handler_id, 0)
  end subroutine request_base_call_client_handler

  subroutine request_base_await_handler (req)
    class(request_base_t), intent(inout) :: req
    call req%handler%waitall ()
  end subroutine request_base_await_handler
end module request_base
