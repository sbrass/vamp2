program main
  use iso_fortran_env, only: ERROR_UNIT, r64 => REAL64
  use test_utils
  use mpi_f08
  implicit none

  integer :: n_events
  integer :: rank, start_it, end_it, n_local_events
  call MPI_INIT ()
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)
  call commandline_get_n_events (n_events)
  if (rank == 0) then
     call compute_and_scatter_intervals (n_events, start_it, end_it)
  else
     call retrieve_intervals (start_it, end_it)
  end if
  n_local_events = end_it - start_it + 1
  call MPI_ALLREDUCE (n_local_events, n_events, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD)
  write (ERROR_UNIT, "(A,1X,I3,4(1X,A,1X,I9),1X,A)") &
       "=>", rank, "[", start_it, ",", end_it, "] (", n_local_events, "/", n_events, ")"
  call MPI_FINALIZE ()
contains
  subroutine compute_and_scatter_intervals (n_events, start_it, end_it)
    integer, intent(in) :: n_events
    integer, intent(out) :: start_it, end_it
    integer, dimension(:), allocatable :: all_start_it, all_end_it
    integer :: rank, n_workers, n_events_per_worker
    call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
    allocate (all_start_it (n_workers), source = 1)
    allocate (all_end_it (n_workers), source = n_events)
    n_events_per_worker = floor (real (n_events, r64) / n_workers)
    all_start_it = [(1 + rank * n_events_per_worker, rank = 0, n_workers - 1)]
    all_end_it = [(rank * n_events_per_worker, rank = 1, n_workers)]
    all_end_it(n_workers) = n_events
    do rank = 1, n_workers
       write (ERROR_UNIT, "(I3,1X,I9,1X,I9)") rank, all_start_it(rank), all_end_it(rank)
    end do
    call MPI_SCATTER (all_start_it, 1, MPI_INTEGER, start_it, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
    call MPI_SCATTER (all_end_it, 1, MPI_INTEGER, end_it, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
  end subroutine compute_and_scatter_intervals

  subroutine retrieve_intervals (start_it, end_it)
    integer, intent(out) :: start_it, end_it
    integer :: local_start_it, local_end_it
    call MPI_SCATTER (local_start_it, 1, MPI_INTEGER, start_it, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
    call MPI_SCATTER (local_end_it, 1, MPI_INTEGER, end_it, 1, MPI_INTEGER, 0, MPI_COMM_WORLD)
  end subroutine retrieve_intervals
end program main
