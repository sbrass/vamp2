module benchmark_handler
  use kinds, only: default
  use io_units
  use format_defs, only: FMT_17, FMT_10

  use request_callback, only: request_handler_t

  use mpi_f08

  implicit none

  type, extends(request_handler_t) :: benchmark_handler_t
     real(default), dimension(:, :), pointer :: data
   contains
     procedure :: fill => benchmark_handler_fill
     procedure :: write => benchmark_handler_write
     procedure :: handle => benchmark_handler_handle
     procedure :: client_handle => benchmark_handler_client_handle
     final :: benchmark_handler_final
  end type benchmark_handler_t

  public :: benchmark_handler_t

contains

  subroutine benchmark_handler_fill (handler, id, data)
    class(benchmark_handler_t), intent(out) :: handler
    integer, intent(in) :: id
    real(default), dimension(:, :), target :: data
    handler%data => data
    call handler%allocate (n_requests = 1, tag_offset = id)
  end subroutine benchmark_handler_fill

  subroutine benchmark_handler_write (handler, unit)
    class(benchmark_handler_t), intent(in) :: handler
    integer, intent(in), optional :: unit
    integer :: u, i
    u = given_output_unit (unit)
    if (.not. associated (handler%data)) then
       write (u, "(A)") "DATA: [EMPTY]"
       return
    end if
    write (u, "(A)") "DATA:"
    do i = 1, size (handler%data, dim = 2)
       write (u, "(I0,999(1X," // FMT_10 // "))") i, handler%data(:, i)
    end do
  end subroutine benchmark_handler_write

  subroutine benchmark_handler_handle (handler, source_rank, comm)
    class(benchmark_handler_t), intent(inout) :: handler
    integer, intent(in) :: source_rank
    type(MPI_COMM), intent(in) :: comm
    call MPI_IRECV (handler%data, size (handler%data), MPI_DOUBLE_PRECISION, &
         source_rank, handler%tag_offset, comm, handler%request(1))
  end subroutine benchmark_handler_handle

  subroutine benchmark_handler_client_handle (handler, dest_rank, comm)
    class(benchmark_handler_t), intent(inout) :: handler
    integer, intent(in) :: dest_rank
    type(MPI_COMM), intent(in) :: comm
    call MPI_ISEND (handler%data, size (handler%data), MPI_DOUBLE_PRECISION, &
         dest_rank, handler%tag_offset, comm, handler%request(1))
  end subroutine benchmark_handler_client_handle

  subroutine benchmark_handler_final (handler)
    type(benchmark_handler_t), intent(inout) :: handler
    nullify (handler%data)
  end subroutine benchmark_handler_final
end module benchmark_handler
