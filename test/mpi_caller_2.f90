!! Setup request caller and add the default load balancer.
program main
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  use request_base, only: request_t
  use request_caller
  use request_balancer

  use signal, only: signal_print_pid_and_wait
  use test_utils, only: commandline_is_gdb_attach

  use mpi_f08

  type(request_caller_t) :: caller
  type(request_t) :: request

  class(request_balancer_t), allocatable :: balancer

  integer, parameter :: N_RESOURCES = 10

  integer :: n_workers, rank

  call MPI_INIT ()
  call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)

  if (commandline_is_gdb_attach ()) then
     if (rank == 0) call signal_print_pid_and_wait ()
     call MPI_BARRIER (MPI_COMM_WORLD)
  end if

  write (ERROR_UNIT, "(A)") "* Initialize request caller object."
  call caller%init (MPI_COMM_WORLD)

  if (caller%is_master ()) then
     write (ERROR_UNIT, "(A)") "* Allocate default load balancer and initialize on master."
     allocate (request_balancer_t :: balancer)
     call balancer%init (n_workers - 1, n_resources)
     call caller%add_balancer (balancer)
     write (ERROR_UNIT, "(A)") "* Handle workload on master."
     call caller%handle_workload ()
     call caller%write (ERROR_UNIT)
  else
     write (ERROR_UNIT, "(A)") "* Handle workload on slave."
     do
        call caller%request_workload (request)
        if (request%terminate) exit
        write (ERROR_UNIT, "(A,1X,I3)") "RESOURCE", request%handler_id
        call caller%release_workload (request)
     end do
  end if
  call MPI_FINALIZE ()
end program main
