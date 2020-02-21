module result_handler
  use iso_fortran_env, only: r64 => REAL64, ERROR_UNIT

  use mpi_f08
  use request_callback, only: request_handler_t

  implicit none

  private

  type :: result_t
     integer :: samples = 0
     real(r64) :: sum_integral = 0
     real(r64) :: sum_integral_sq = 0
   contains
     procedure :: send => result_send
     procedure :: receive => result_receive
     procedure :: get_n_requests => result_get_n_requests
  end type result_t

  type, extends(request_handler_t) :: result_handler_t
     type(result_t), pointer :: obj => null ()
   contains
     procedure :: init => result_handler_init
     procedure :: write => result_handler_write
     procedure :: handle => result_handler_handle
     procedure :: client_handle => result_handler_client_handle
     final :: result_handler_final
  end type result_handler_t

  public :: result_t, result_handler_t
contains
  subroutine result_handler_init (handler, result, n_requests, channel)
    class(result_handler_t), intent(inout) :: handler
    type(result_t), intent(in), target :: result
    integer, intent(in) :: n_requests
    integer, intent(in) :: channel
    handler%obj => result
    handler%finished = .false.
    call handler%allocate (n_requests, tag_offset = channel * n_requests)
  end subroutine result_handler_init

  subroutine result_handler_write (handler, unit)
    class(result_handler_t), intent(in) :: handler
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    call handler%base_write (u)
  end subroutine result_handler_write

  subroutine result_handler_handle (handler, source_rank, tag, comm)
    class(result_handler_t), intent(inout) :: handler
    integer, intent(in) :: source_rank
    integer, intent(in) :: tag
    type(MPI_COMM), intent(in) :: comm
    call handler%obj%receive (source_rank, handler%tag_offset, comm, handler%request)
    handler%finished = .false.
  end subroutine result_handler_handle

  subroutine result_handler_client_handle (handler, dest_rank, tag, comm)
    class(result_handler_t), intent(inout) :: handler
    integer, intent(in) :: dest_rank
    integer, intent(in) :: tag
    type(MPI_COMM), intent(in) :: comm
    call handler%obj%send (dest_rank, handler%tag_offset, comm, handler%request)
    handler%finished = .false.
  end subroutine result_handler_client_handle

  !> Finalize result_handler_t.
  !!
  !! Nullify pointer to object.
  subroutine result_handler_final (handler)
    type(result_handler_t), intent(inout) :: handler
    nullify (handler%obj)
  end subroutine result_handler_final

  !> Asymmetric send and receive.
  subroutine result_send (result, receiver, tag_offset, comm, reqs)
    class(result_t), intent(in) :: result
    integer, intent(in) :: receiver
    integer, intent(in) :: tag_offset
    type(MPI_COMM), intent(in) :: comm
    type(MPI_REQUEST), dimension(3), intent(inout) :: reqs
    call MPI_ISEND (result%samples, 1, MPI_INTEGER, &
         receiver, tag_offset + 1, comm, reqs(1))
    call MPI_ISEND (result%sum_integral, 1, MPI_DOUBLE_PRECISION, &
         receiver, tag_offset + 1, comm, reqs(2))
    call MPI_ISEND (result%sum_integral_sq, 1, MPI_DOUBLE_PRECISION, &
         receiver, tag_offset + 2, comm, reqs(3))
  end subroutine result_send

  subroutine result_receive (result, source, tag_offset, comm, reqs)
    class(result_t), intent(inout) :: result
    integer, intent(in) :: source
    integer, intent(in) :: tag_offset
    type(MPI_COMM), intent(in) :: comm
    type(MPI_REQUEST), dimension(3), intent(inout) :: reqs
    call MPI_IRECV (result%samples, 1, MPI_INTEGER, &
         source, tag_offset + 1, comm, reqs(1))
    call MPI_IRECV (result%sum_integral, 1, MPI_DOUBLE_PRECISION, &
         source, tag_offset + 1, comm, reqs(2))
    call MPI_IRECV (result%sum_integral_sq, 1, MPI_DOUBLE_PRECISION, &
         source, tag_offset + 2, comm, reqs(3))
  end subroutine result_receive

  integer function result_get_n_requests (result) result (n_requests)
    class(result_t), intent(in) :: result
    n_requests = 3
  end function result_get_n_requests
end module result_handler
