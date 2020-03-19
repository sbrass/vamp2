module request_callback
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use binary_tree
  use diagnostics

  use mpi_f08 !NODEP!

  implicit none

  private

  !> Request handler.
  !!
  !! A request handler allows to dispatch an object for communication a priori
  !! for a slave and a master.
  !! Typically, each slave register its own request handles, whereas the master
  !! requests all possible requests handles matching those of the slaves.
  !! The requests can then be used later on, e.g. during a computation, a slave
  !! may add a request to the master worker and starts sending an object.
  !! The master worker looks up the appropriate request handler, which then
  !! closes the communication, i.e. by a receive call.
  !! Most important: Only a pointer to the buffer object is stored, therefore, the calling function
  !! has to ensure, that the communication object (buffer) will not be changed
  !! during a request handle (receive or send).
  !!
  !! Remark: The handler allows for a tag offset which allows to uniqify the the communication.
  !!         The problem occurs when multiple callback need to handled simultanously and MPI needs to connect the communication calls accordingly.
  !!         Each message has a tuple of (source, tag, comm) associated, we can uniquify this tuple by a unique tag.
  !!         tag = tag_offset + {1, …, N_requests} where tag_offsets are multiple of N_requests.
  !!         The latter condition should checked by a modulo.
  !! What happens if the communication is out of order (is this a problem? check with standard)?
  type, abstract :: request_handler_t
     integer :: n_requests = 0
     integer :: tag_offset = 0
     type(MPI_REQUEST), dimension(:), allocatable :: request
     type(MPI_STATUS), dimension(:), allocatable :: status
     logical :: activated = .false.
     logical :: finished = .false.
   contains
     procedure :: base_write => request_handler_base_write
     procedure(request_handler_write), deferred :: write
     !! \todo{sbrass} implement initialization procedure.
     procedure(request_handler_handle), deferred :: handle
     procedure(request_handler_client_handle), deferred :: client_handle
     procedure :: allocate => request_handler_allocate
     procedure :: get_status => request_handler_get_status
     procedure :: testall => request_handler_testall
     procedure :: waitall => request_handler_waitall
     procedure :: free => request_handler_free
  end type request_handler_t

  type :: request_handler_manager_t
     private
     type(MPI_COMM) :: comm
     type(binary_tree_t) :: tree
   contains
     procedure :: init => request_handler_manager_init
     procedure :: write => request_handler_manager_write
     procedure :: add => request_handler_manager_add
     procedure :: clear => request_handler_manager_clear
     procedure :: has_handler => request_handler_manager_has_handler
     procedure :: test => request_handler_manager_test
     procedure :: wait => request_handler_manager_wait
     procedure :: waitall => request_handler_manager_waitall
     procedure, private :: fill_status => request_handler_manager_fill_status
     procedure, private :: handler_at => request_handler_manager_handler_at
     procedure :: callback => request_handler_manager_callback
     procedure :: client_callback => request_handler_manager_client_callback
  end type request_handler_manager_t

  abstract interface
     subroutine request_handler_write (handler, unit)
       import :: request_handler_t
       class(request_handler_t), intent(in) :: handler
       integer, intent(in), optional :: unit
     end subroutine request_handler_write

     !> Handle a request from server side.
     !!
     !! The message tag can be used in order to uniquify the respective messages between master and slave.
     !! E.g. by explicitly setting it, or by using it in a computation i * N_R + j, i \in {1, …, N} and j \in {1, …, N_R}.
     !!
     !! Must set *activated* to .true. when called.
     !! \param[in] source Integer rank of the source in comm.
     !! \param[in] tag Specify the message tag.
     !! \param[in] comm MPI communicator.
     subroutine request_handler_handle (handler, source_rank, comm)
       import :: request_handler_t, MPI_COMM
       class(request_handler_t), intent(inout) :: handler
       integer, intent(in) :: source_rank
       type(MPI_COMM), intent(in) :: comm
     end subroutine request_handler_handle

     !> Handle a request from client side.
     !!
     !! Must set *activated* to .true. when called.
     !! \param[in] rank Integer of the receiver in comm.
     !! \param[in] tag Specify the message tag.
     !! \param[in] comm MPI communicator.
     subroutine request_handler_client_handle (handler, dest_rank, comm)
       import :: request_handler_t, MPI_COMM
       class(request_handler_t), intent(inout) :: handler
       integer, intent(in) :: dest_rank
       type(MPI_COMM), intent(in) :: comm
     end subroutine request_handler_client_handle
  end interface

  public :: request_handler_t, request_handler_manager_t
contains
  !!
  !! Request handler.
  !!
  subroutine request_handler_base_write (handler, unit)
    class(request_handler_t), intent(in) :: handler
    integer, intent(in), optional :: unit
    integer :: u, i
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A,1X,I0)") "N_REQUESTS", handler%n_requests
    write (u, "(A,1X,I0)") "TAG_OFFSET", handler%tag_offset
    write (u, "(A,1X,L1)") "FINISHED", handler%finished
    write (u, "(A,1X,L1)") "ACTIVATED", handler%activated
    write (u, "(A)") "I | SOURCE | TAG | ERROR | REQUEST_NULL"
    do i = 1, handler%n_requests
       write (u, "(A,4(1X,I0),1X,L1)") "REQUEST", i, &
            handler%status(i)%MPI_SOURCE, &
            handler%status(i)%MPI_TAG, &
            handler%status(i)%MPI_ERROR, &
            (handler%request(i) == MPI_REQUEST_NULL)
    end do
  end subroutine request_handler_base_write

  !> Allocate MPI request and status object.
  !!
  !! Must be called during or after object-initialization.
  !!
  !! \param[inout] handler Handler must be intent inout, as the calling function may already manipulated the extended object.
  !! \param[in] n_requests Number of MPI requests the objects needs to be able to handle.
  !! \param[in] tag_offset First tag to be used, all other must follow in an increasing manner until tag_offset + (N_r + 1).
  !! Proof: tag \in {tag_offset, tag_offset + n_requests}.
  subroutine request_handler_allocate (handler, n_requests, tag_offset)
    class(request_handler_t), intent(inout) :: handler
    integer, intent(in) :: n_requests
    integer, intent(in) :: tag_offset
    allocate (handler%request(n_requests), source = MPI_REQUEST_NULL)
    allocate (handler%status(n_requests))
    handler%n_requests = n_requests
    if (mod (tag_offset, n_requests) /= 0) &
         call msg_bug ("Error during handler allocate, tag_offset is not a multiple of n_requests.")
    !! What is the max.-allowed MPI_TAG?
    handler%tag_offset = tag_offset
    handler%activated = .false.
    handler%finished = .false.
  end subroutine request_handler_allocate

  !> Get status from request objects in a non-destructive way.
  subroutine request_handler_get_status (handler)
    class(request_handler_t), intent(inout) :: handler
    integer :: i
    logical :: flag
    if (.not. handler%activated) return
    handler%finished = .true.
    do i = 1, handler%n_requests
       call MPI_REQUEST_GET_STATUS (handler%request(i), flag, &
            handler%status(i))
       handler%finished = handler%finished .and. flag
    end do
  end subroutine request_handler_get_status

  !> Call MPI_WATIALL and raise finished flag.
  subroutine request_handler_waitall (handler)
    class(request_handler_t), intent(inout) :: handler
    integer :: error
    if (.not. handler%activated .or. handler%finished) return
    call MPI_WAITALL (handler%n_requests, handler%request, handler%status, error)
    if (error /= 0) then
       call msg_bug ("Request: Error occured during waitall on handler.")
    end if
    handler%finished = .true.
  end subroutine request_handler_waitall

  logical function request_handler_testall (handler) result (flag)
    class(request_handler_t), intent(inout) :: handler
    integer :: error
    if (.not. handler%activated .or. .not. handler%finished) then
       call MPI_TESTALL (handler%n_requests, handler%request, handler%finished, &
            handler%status, error)
       ! call print_status ()
       if (error /= 0) then
          call msg_bug ("Request: Error occured during testall on handler.")
       end if
    end if
    flag = handler%finished
  contains
    subroutine print_status ()
      integer :: i
      do i = 1, handler%n_requests
         associate (status => handler%status(i))
           write (ERROR_UNIT, *) status%MPI_SOURCE, status%MPI_TAG, status%MPI_ERROR
         end associate
      end do
    end subroutine print_status
  end function request_handler_testall

  subroutine request_handler_free (handler)
    class(request_handler_t), intent(inout) :: handler
    integer :: i, error
    do i = 1, handler%n_requests
       if (handler%request(i) == MPI_REQUEST_NULL) cycle
       call MPI_REQUEST_FREE (handler%request(i), error)
       if (error /= 0) then
          call msg_bug ("Request: Error occured during free request on handler.")
       end if
    end do
  end subroutine request_handler_free

  !!
  !! Request handler manager.
  !!
  subroutine request_handler_manager_init (rhm, comm)
    class(request_handler_manager_t), intent(out) :: rhm
    type(MPI_COMM), intent(in) :: comm
    call MPI_COMM_DUP (comm, rhm%comm)
  end subroutine request_handler_manager_init

  subroutine request_handler_manager_write (rhm, unit)
    class(request_handler_manager_t), intent(in) :: rhm
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A)") "[REQUEST_CALLBACK_MANAGER]"
    call rhm%tree%write (u)
  end subroutine request_handler_manager_write

  subroutine request_handler_manager_add (rhm, handler_id, handler)
    class(request_handler_manager_t), intent(inout) :: rhm
    integer, intent(in) :: handler_id
    class(request_handler_t), pointer, intent(in) :: handler
    class(*), pointer :: obj
    obj => handler
    call rhm%tree%insert (handler_id, obj)
  end subroutine request_handler_manager_add

  subroutine request_handler_manager_clear (rhm)
    class(request_handler_manager_t), intent(inout) :: rhm
    call rhm%tree%clear ()
  end subroutine request_handler_manager_clear

  !> Get status (in a non-destructive way) for all associated handler.
  subroutine request_handler_manager_fill_status (rhm)
    class(request_handler_manager_t), intent(inout) :: rhm
    type(binary_tree_iterator_t) :: iterator
    integer :: handler_id
    class(request_handler_t), pointer :: handler
    call iterator%init (rhm%tree)
    do while (iterator%is_iterable ())
       call iterator%next (handler_id)
       call rhm%handler_at (handler_id, handler)
       call handler%get_status ()
    end do
  end subroutine request_handler_manager_fill_status

  logical function request_handler_manager_test (rhm, handler_id) result (flag)
    class(request_handler_manager_t), intent(inout) :: rhm
    integer, intent(in) :: handler_id
    class(request_handler_t), pointer :: handler
    call rhm%handler_at (handler_id, handler)
    flag = handler%testall ()
  end function request_handler_manager_test

  subroutine request_handler_manager_wait (rhm, handler_id)
    class(request_handler_manager_t), intent(inout) :: rhm
    integer, intent(in) :: handler_id
    class(request_handler_t), pointer :: handler
    call rhm%handler_at (handler_id, handler)
    call handler%waitall ()
  end subroutine request_handler_manager_wait

  subroutine request_handler_manager_waitall (rhm)
    class(request_handler_manager_t), intent(inout) :: rhm
    type(binary_tree_iterator_t) :: iterator
    integer :: handler_id
    call iterator%init (rhm%tree)
    do while (iterator%is_iterable ())
       call iterator%next (handler_id)
       !! Test handler (destructive test on request handler).
       if (.not. rhm%test (handler_id)) &
            call rhm%wait (handler_id)
    end do
  end subroutine request_handler_manager_waitall

  subroutine request_handler_manager_handler_at (rhm, handler_id, handler)
    class(request_handler_manager_t), intent(in) :: rhm
    integer, intent(in) :: handler_id
    class(request_handler_t), pointer, intent(out) :: handler
    class(*), pointer :: obj
    call rhm%tree%search (handler_id, obj)
    select type (obj)
    class is (request_handler_t)
       handler => obj
    class default
       call msg_bug ("Object is not derived from request_handler_t.")
    end select
  end subroutine request_handler_manager_handler_at

  function request_handler_manager_has_handler (rhm, handler_id) result (flag)
    class(request_handler_manager_t), intent(inout) :: rhm
    integer, intent(in) :: handler_id
    logical :: flag
    flag = rhm%tree%has_key (handler_id)
  end function request_handler_manager_has_handler

  !> Call server-sided procedure of callback with handler_id.
  !!
  !! \param[in] handler_id
  !! \param[in] source
  subroutine request_handler_manager_callback (rhm, handler_id, source_rank)
    class(request_handler_manager_t), intent(inout) :: rhm
    integer, intent(in) :: handler_id
    integer, intent(in) :: source_rank
    class(request_handler_t), pointer :: handler
    if (.not. rhm%tree%has_key (handler_id)) return
    call rhm%handler_at (handler_id, handler)
    call handler%handle (source_rank = source_rank, comm = rhm%comm)
  end subroutine request_handler_manager_callback

  !> Call client-sided procedure of callback with handler_id.
  !!
  !! \param[in] handler_id
  !! \param[in] source Destination rank.
  subroutine request_handler_manager_client_callback (rhm, handler_id, dest_rank)
    class(request_handler_manager_t), intent(inout) :: rhm
    integer, intent(in) :: handler_id
    integer, intent(in) :: dest_rank
    class(request_handler_t), pointer :: handler
    if (.not. rhm%tree%has_key (handler_id)) return
    call rhm%handler_at (handler_id, handler)
    call handler%client_handle (dest_rank = dest_rank, comm = rhm%comm)
  end subroutine request_handler_manager_client_callback
end module request_callback


