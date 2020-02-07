! WHIZARD 2.8.3 Oct 24 2019
!
! Copyright (C) 1999-2019 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

module request_callback
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use binary_tree
  use diagnostics

  use mpi_f08 !NODEP!

  implicit none

  private

  integer, parameter, public :: MPI_TAG_OFFSET = 128

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
     logical :: finished = .false.
   contains
     !! \todo{sbrass} implement initialization procedure.
     procedure(request_handler_handle), deferred :: handle
     procedure(request_handler_client_handle), deferred :: client_handle
     procedure :: allocate => request_handler_allocate
     procedure :: testall => request_handler_testall
     procedure :: waitall => request_handler_waitall
  end type request_handler_t

  type :: request_handler_manager_t
     private
     type(binary_tree_t) :: tree
   contains
     procedure :: init => request_handler_manager_init
     procedure :: write => request_handler_manager_write
     procedure :: add => request_handler_manager_add
     procedure :: has_handler => request_handler_manager_has_handler
     procedure :: test => request_handler_manager_test
     procedure :: wait => request_handler_manager_wait
     procedure :: waitall => request_handler_manager_waitall
     procedure, private :: handler_at => request_handler_manager_handler_at
     procedure :: callback => request_handler_manager_callback
     procedure :: client_callback => request_handler_manager_client_callback
  end type request_handler_manager_t

  abstract interface
     !> Handle a request from server side.
     !!
     !! The message tag can be used in order to uniquify the respective messages between master and slave.
     !! E.g. by explicitly setting it, or by using it in a computation i * N_R + j, i ∈ {1, …, N} and j ∈ {1, …, N_R}.
     !!
     !! \param[in] source Integer rank of the source in comm.
     !! \param[in] tag Specify the message tag.
     !! \param[in] comm MPI communicator.
     subroutine request_handler_handle (handler, source, tag, comm)
       import :: request_handler_t, MPI_COMM
       class(request_handler_t), intent(inout) :: handler
       integer, intent(in) :: source
       integer, intent(in) :: tag
       type(MPI_COMM), intent(in) :: comm
     end subroutine request_handler_handle

     !> Handle a request from client side.
     !!
     !! \param[in] rank Integer of the receiver in comm.
     !! \param[in] tag Specify the message tag.
     !! \param[in] comm MPI communicator.
     subroutine request_handler_client_handle (handler, rank, tag, comm)
       import :: request_handler_t, MPI_COMM
       class(request_handler_t), intent(inout) :: handler
       integer, intent(in) :: rank
       integer, intent(in) :: tag
       type(MPI_COMM), intent(in) :: comm
     end subroutine request_handler_client_handle
  end interface

  public :: request_handler_t, request_handler_manager_t
contains
  !!
  !! Request handler.
  !!

  !> Allocate MPI request and status object.
  !!
  !! Must be called during or after object-initialization.
  !!
  !! \param[inout] handler Handler must be intent inout, as the calling function may already manipulated the extended object.
  !! \param[in] n_requests Number of MPI requests the objects needs to be able
  !! to handle.
  subroutine request_handler_allocate (handler, n_requests, tag_offset)
    class(request_handler_t), intent(inout) :: handler
    integer, intent(in) :: n_requests
    integer, intent(in) :: tag_offset
    allocate (handler%request(n_requests))
    allocate (handler%status(n_requests))
    handler%n_requests = n_requests
    if (mod (tag_offset, n_requests) /= 0) &
         call msg_bug ("Error during handler allocate, tag_offset is not a multiple of n_requests.")
    !! What is the max.-allowed MPI_TAG?
    handler%tag_offset = tag_offset
  end subroutine request_handler_allocate

  !> Call MPI_WATIALL and raise finished flag.
  subroutine request_handler_waitall (handler)
    class(request_handler_t), intent(inout) :: handler
    integer :: error
    if (handler%finished) return
    call MPI_WAITALL (handler%n_requests, handler%request, handler%status, error)
    if (error /= 0) then
       call msg_bug ("Request: Error occured during waitall on handler.")
    end if
    handler%finished = .true.
  end subroutine request_handler_waitall

  logical function request_handler_testall (handler) result (flag)
    class(request_handler_t), intent(inout) :: handler
    integer :: error
    if (.not. handler%finished) then
       call MPI_TESTALL (handler%n_requests, handler%request, handler%finished, &
            handler%status, error)
       if (error /= 0) then
          call msg_bug ("Request: Error occured during testall on handler.")
       end if
    end if
    flag = handler%finished
  ! contains
  !   subroutine print_status ()
  !     integer :: i
  !     do i = 1, handler%n_requests
  !        associate (status => handler%status(i))
  !          write (ERROR_UNIT, *) status%MPI_SOURCE, status%MPI_TAG, status%MPI_ERROR
  !        end associate
  !     end do
  !   end subroutine print_status
  end function request_handler_testall

  !!
  !! Request handler manager.
  !!
  subroutine request_handler_manager_init (rhm)
    class(request_handler_manager_t), intent(out) :: rhm
    !! Do something.
  end subroutine request_handler_manager_init

  subroutine request_handler_manager_write (rhm, unit)
    class(request_handler_manager_t), intent(in) :: rhm
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
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
  !! \param[in] comm Communicator, must match with the corresponding communicator from the client-sided procedure.
  subroutine request_handler_manager_callback (rhm, handler_id, source, comm)
    class(request_handler_manager_t), intent(inout) :: rhm
    integer, intent(in) :: handler_id
    integer, intent(in) :: source
    type(MPI_COMM), intent(in) :: comm
    class(request_handler_t), pointer :: handler
    if (.not. rhm%tree%has_key (handler_id)) return
    call rhm%handler_at (handler_id, handler)
    call handler%handle (source = source, tag = handler_id, comm = comm)
  end subroutine request_handler_manager_callback

  !> Call client-sided procedure of callback with handler_id.
  !!
  !! \param[in] handler_id
  !! \param[in] source Destination rank.
  !! \param[in] comm Communicator (must match between client and server-side procedure).
  subroutine request_handler_manager_client_callback (rhm, handler_id, source, comm)
    class(request_handler_manager_t), intent(inout) :: rhm
    integer, intent(in) :: handler_id
    integer, intent(in) :: source
    type(MPI_COMM), intent(in) :: comm
    class(request_handler_t), pointer :: handler
    if (.not. rhm%tree%has_key (handler_id)) return
    call rhm%handler_at (handler_id, handler)
    call handler%client_handle (rank = source, tag = handler_id, comm = comm)
  end subroutine request_handler_manager_client_callback
end module request_callback


