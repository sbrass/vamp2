program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use mpi_f08

  type(MPI_COMM) :: request_comm, callback_comm
  integer :: rank

  integer, parameter :: MPI_POLL = 1, MPI_CALLBACK = 1

  !! Proof: size(reqs) = N - 1 on master, size(reqs) = 1 on slave.
  integer, dimension(:), allocatable :: callback_buffer
  type(MPI_REQUEST), dimension(:), allocatable :: reqs
  type(MPI_STATUS), dimension(:), allocatable :: stats

  call MPI_INIT ()

  call MPI_COMM_DUP (MPI_COMM_WORLD, request_comm)
  call MPI_COMM_DUP (MPI_COMM_WORLD, callback_comm)

  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)

  call allocate_reqs_and_stats (callback_buffer, reqs, stats, callback_comm)
  if (rank == 0) then
     call listen_request (request_comm, callback_comm)
  else
     call poll_request (request_comm, callback_comm)
  end if

  write (ERROR_UNIT, "(A)") "WAITALL"
  call MPI_WAITALL (size (reqs), reqs, stats)
  write (ERROR_UNIT, *) callback_buffer

  call MPI_FINALIZE ()
contains
  subroutine allocate_reqs_and_stats (callback_buffer, reqs, stats, comm)
    integer, dimension(:), allocatable, intent(out) :: callback_buffer
    type(MPI_REQUEST), dimension(:), allocatable, intent(out) :: reqs
    type(MPI_STATUS), dimension(:), allocatable, intent(out) :: stats
    type(MPI_COMM), intent(in) :: comm
    integer :: n_size, rank
    call MPI_COMM_RANK (comm, rank)
    !! remove master.
    if (rank == 0) then
       call MPI_COMM_SIZE (comm, n_size)
       n_size = n_size - 1
    else
       n_size = 1
    end if
    allocate (reqs(n_size))
    allocate (stats(n_size))
    allocate (callback_buffer(n_size))
    callback_buffer = rank
  end subroutine allocate_reqs_and_stats

  subroutine listen_request (comm, callback_comm)
    type(MPI_COMM), intent(in) :: comm
    type(MPI_COMM), intent(in) :: callback_comm
    integer :: i, n_size
    integer :: buf
    type(MPI_STATUS) :: status
    call MPI_COMM_SIZE (comm, n_size)
    n_size = n_size - 1 !! exclude master.
    write (ERROR_UNIT, "(A,1X,I0)") "LISTEN ON", n_size
    do i = 1, n_size
       !! Push: Asymmetric send/receive, p. 29.
       call MPI_RECV (buf, 1, MPI_INTEGER, &
            MPI_ANY_SOURCE, MPI_ANY_TAG, comm, status)
       write (ERROR_UNIT, "(A,3(1X,I0))") "RECEIVE", status%MPI_SOURCE, status%MPI_TAG, status%MPI_ERROR
       write (ERROR_UNIT, "(A,1X,I0)") "--->", buf
       call server_callback (status%MPI_SOURCE, callback_comm)
    end do
  end subroutine listen_request

  !! Push.
  subroutine poll_request (comm, callback_comm)
    type(MPI_COMM), intent(in) :: comm
    type(MPI_COMM), intent(in) :: callback_comm
    integer :: buf
    buf = -1
    write (ERROR_UNIT, "(A)") "CLIENT SEND"
    call MPI_SEND (buf, 1, MPI_INTEGER, &
         0, MPI_POLL, comm)
    write (ERROR_UNIT, "(A)") "CLIENT CALLBACK"
    call client_callback(callback_comm)
  end subroutine poll_request

  subroutine server_callback (source, comm)
    integer, intent(in) :: source
    type(MPI_COMM), intent(in) :: comm
    call MPI_IRECV (callback_buffer(source), 1, MPI_INTEGER, &
         source, MPI_CALLBACK, comm, reqs(source))
  end subroutine server_callback

  subroutine client_callback (comm)
    type(MPI_COMM), intent(in) :: comm
    call MPI_ISEND (callback_buffer(1), 1, MPI_INTEGER, &
         0, MPI_CALLBACK, comm, reqs(1))
  end subroutine client_callback
end program main
