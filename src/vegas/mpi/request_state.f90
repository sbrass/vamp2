module request_state
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use iterator
  use diagnostics

  use mpi_f08 !NODEP!

  implicit none

  private

  integer, parameter, public :: MPI_EMPTY_HANDLER = 0

  integer, parameter, public :: MPI_TAG_NULL = 0, &
       MPI_TAG_REQUEST = 1, &
       MPI_TAG_RELEASE = 2, &
       MPI_TAG_HANDLER_AND_RELEASE = 4, &
       MPI_TAG_TERMINATE = 8, &
       MPI_TAG_ASSIGN_SINGLE = 16, &
       MPI_TAG_ASSIGN_GROUP = 32, &
       MPI_TAG_COMMUNICATOR_GROUP = 64

  type :: request_state_t
     private
     type(MPI_COMM) :: comm
     integer :: n_workers = 0
     integer :: n_workers_done = 0
     !! From MPI-3.1 book
     !! i ∈ {1, N_workes_done}, max size = N_workers
     type(MPI_Request), dimension(:), allocatable :: request
     type(MPI_Status), dimension(:), allocatable :: status
     integer, dimension(:), allocatable :: indices
     !! i ∈ {1, N_workers}
     integer, dimension(:), allocatable, public :: handler
     logical, dimension(:), allocatable :: terminated
     type(iterator_t) :: request_iterator
   contains
     procedure :: init => request_state_init
     procedure :: is_terminated => request_state_is_terminated
     procedure :: terminate => request_state_terminate
     procedure :: receive_request => request_state_receive_request
     procedure :: await_request => request_state_await_request
     procedure :: has_request => request_state_has_request
     procedure :: get_request => request_state_get_request
     procedure :: update_request => request_state_update_request
     procedure :: free_request => request_state_free_request
  end type request_state_t

  public :: request_state_t
contains
  subroutine request_state_init (state, comm, n_workers)
    class(request_state_t), intent(out) :: state
    type(MPI_COMM), intent(in) :: comm
    integer, intent(in) :: n_workers
    integer :: rank
    state%comm = comm
    state%n_workers = n_workers
    state%n_workers_done = n_workers
    call state%request_iterator%init (1, n_workers)
    allocate (state%request(state%n_workers))
    allocate (state%status(state%n_workers))
    allocate (state%handler(state%n_workers), source = MPI_EMPTY_HANDLER)
    allocate (state%indices(state%n_workers), source = 0)
    allocate (state%terminated(state%n_workers), source = .false.)
    state%indices = [(rank, rank = 1, n_workers)]
  end subroutine request_state_init

  pure function request_state_is_terminated (state) result (flag)
    class(request_state_t), intent(in) :: state
    logical :: flag
    flag = all (state%terminated)
  end function request_state_is_terminated

  subroutine request_state_terminate (state, rank)
    class(request_state_t), intent(inout) :: state
    integer, intent(in) :: rank
    integer :: error
    call MPI_SEND (MPI_EMPTY_HANDLER, 1, MPI_INTEGER, &
         rank, MPI_TAG_TERMINATE, state%comm, error)
    if (error /= 0) then
       write (msg_buffer, "(A,1X,I3)") "Request: Error occured during terminte, RANK", rank
       call msg_bug ()
    end if
    state%terminated(rank) = .true.
  end subroutine request_state_terminate

  subroutine request_state_receive_request (state)
    class(request_state_t), intent(inout) :: state
    integer :: i, rank, handler
    integer :: error
    do i = 1, state%n_workers_done
       rank = state%indices(i)
       handler = state%handler(rank)
       call MPI_IRECV (handler, 1, MPI_INTEGER, &
            rank, MPI_ANY_TAG, state%comm, state%request(rank), error)
       if (error /= 0) then
          write (msg_buffer, "(A,2(A,1X,I6))") "Request: Error occured during receive request, &
             & RANK", rank, "HANDLER", handler
          call msg_bug ()
       end if
    end do
  end subroutine request_state_receive_request

  subroutine request_state_await_request (state)
    class(request_state_t), intent(inout) :: state
    integer :: error
    !! Proof: REQUEST(i), i ∈ {1, N_workers}, i is equivalent to rank.
    !! Proof: INDICES(j), STATUS(j), j ∈ {1, N_workers_done}
    !! Proof: INDICES(j) → i, injectiv.
    call MPI_WAITSOME (state%n_workers, state%request, state%n_workers_done, &
         state%indices, state%status, error)
    if (error /= 0) then
       write (ERROR_UNIT, "(A)") "Error occured during await request..."
       stop 1
    end if
    call state%request_iterator%init (1, state%n_workers_done)
  end subroutine request_state_await_request

  pure function request_state_has_request (state) result (flag)
    class(request_state_t), intent(in) :: state
    logical :: flag
    flag = state%request_iterator%is_iterable ()
  end function request_state_has_request

  subroutine request_state_get_request (state, rank, tag, handler)
    class(request_state_t), intent(inout) :: state
    integer, intent(out) :: rank
    integer, intent(out) :: tag
    integer, intent(out) :: handler
    integer :: ndx
    if (.not. state%has_request ()) then
       call msg_bug ("Request: Cannot access missing request.")
    end if
    ndx = state%request_iterator%next ()
    rank = state%indices(ndx)
    if (rank /= state%status(ndx)%MPI_SOURCE) then
       write (msg_buffer, "(A,2(1X,I3))") &
          "Request: RANK and SOURCE mismatch", rank, state%status(ndx)%MPI_SOURCE
       call msg_bug ()
    end if
    tag = state%status(ndx)%MPI_TAG
    handler = state%handler(rank)
  end subroutine request_state_get_request

  subroutine request_state_update_request (state, rank, tag, handler)
    class(request_state_t), intent(inout) :: state
    integer, intent(in) :: rank
    integer, intent(in) :: tag
    integer, intent(in) :: handler
    integer :: error
    state%handler(rank) = handler
    call MPI_SEND (handler, 1, MPI_INTEGER, &
         rank, tag, state%comm, error)
    if (error /= 0) then
       write (msg_buffer, "(A,3(A,1X,I3))") "Request: Error occured during update, &
          &RANK", rank, "TAG", tag, "HANDLER", handler
       call msg_bug ()
    end if
  end subroutine request_state_update_request

  subroutine request_state_free_request (state)
    class(request_state_t), intent(inout) :: state
    integer :: rank
    do rank = 1, state%n_workers
       call MPI_REQUEST_FREE (state%request(rank))
    end do
  end subroutine request_state_free_request
end module request_state

