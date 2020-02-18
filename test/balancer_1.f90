!> Test balancer_simple_t implementation.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_t

  use balancer_base
  use balancer_simple

  use mpi_f08

  implicit none

  class(balancer_base_t), allocatable :: balancer

  integer, parameter :: N_RESOURCES = 4
  logical, dimension(N_RESOURCES) :: parallel_grid = .false.
  integer :: worker, handler
  integer :: rank, n_workers

  type(commandline_t) :: cmd

  call MPI_INIT ()
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)
  call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)

  call cmd%parse ()
  if (cmd%gdb_attach) then
     if (cmd%gdb_attach_rank > n_workers) &
          call msg_fatal ("Cannot attach to rank outside of communicator.")
     if (cmd%gdb_attach_rank == rank) then
        call signal_print_pid_and_wait ()
     end if
     call MPI_BARRIER (MPI_COMM_WORLD)
  end if

  parallel_grid(3) = .true.
  worker = rank + 1

  allocate (balancer_simple_t :: balancer)
  select type (balancer)
  type is (balancer_simple_t)
     call balancer%init (n_workers = n_workers, n_resources = n_resources)
     call balancer%update_state (worker, parallel_grid)
  end select
  call balancer%write (ERROR_UNIT)

  do while (balancer%is_pending ())
     if (balancer%is_assignable (worker_id = worker)) then
        call balancer%assign_worker (worker_id = worker, resource_id = handler)
        write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "BALANCER", worker, handler
        call balancer%free_worker(worker_id = worker)
     end if
  end do

  call balancer%write (ERROR_UNIT)

  call MPI_FINALIZE ()
end program main
