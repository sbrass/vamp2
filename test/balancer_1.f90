!> Test balancer_simple_t implementation.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use diagnostics
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
     if (cmd%gdb_attach_rank >= n_workers) &
          call msg_fatal ("Cannot attach to rank outside of communicator.")
     if (cmd%gdb_attach_rank == rank) then
        call signal_print_pid_and_wait ()
     end if
     call MPI_BARRIER (MPI_COMM_WORLD)
  end if

  parallel_grid(3) = .true.
  worker = shift_rank_to_worker (rank)

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Setup"
  write (ERROR_UNIT, "(A)") "* =================================================="

  allocate (balancer_simple_t :: balancer)
  select type (balancer)
  type is (balancer_simple_t)
     call balancer%init (n_workers = n_workers, n_resources = n_resources)
     call balancer%update_state (worker, parallel_grid)
  end select

  call balancer%write (ERROR_UNIT)
  call check_are_worker_pending ()
  call check_are_worker_assignable ()
  call check_is_pending ()
  call check_resource_group ()
  call check_resource_master ()

  do while (balancer%is_pending ())
     if (balancer%is_assignable (worker_id = worker)) then
        call balancer%assign_worker (worker_id = worker, resource_id = handler)
        write (ERROR_UNIT, "(A)") "* =================================================="
        write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "BALANCER", worker, handler
        call check_are_worker_pending ()
        call check_are_worker_assignable ()
        call check_is_pending ()
        call check_resource_group ()
        call check_resource_master ()
        call balancer%free_worker(worker_id = worker)
     end if
  end do

  write (ERROR_UNIT, "(A)") "* =================================================="
  call balancer%write (ERROR_UNIT)
  call check_are_worker_pending ()
  call check_are_worker_assignable ()
  call check_is_pending ()
  call check_resource_group ()
  call check_resource_master ()

  call MPI_FINALIZE ()
contains
  subroutine check_are_worker_assignable ()
    integer :: i
    write (ERROR_UNIT, "(A)") "* =================================================="
    write (ERROR_UNIT, "(A)") "* CHECK WORKER ASSIGNABLE"
    write (ERROR_UNIT, "(A)") "* =================================================="
    do i = 1, N_WORKERS
       write (ERROR_UNIT, "(A,1X,I0,1X,L1)") "WORKER", i, balancer%is_assignable(i)
    end do
  end subroutine check_are_worker_assignable

  subroutine check_are_worker_pending ()
    integer :: i
    write (ERROR_UNIT, "(A)") "* =================================================="
    write (ERROR_UNIT, "(A)") "* CHECK WORKER PENDING"
    write (ERROR_UNIT, "(A)") "* =================================================="
    do i = 1, N_WORKERS
       write (ERROR_UNIT, "(A,1X,I0,1X,L1)") "WORKER", i, balancer%is_worker_pending(i)
    end do
  end subroutine check_are_worker_pending

  subroutine check_is_pending ()
    write (ERROR_UNIT, "(A)") "* =================================================="
    write (ERROR_UNIT, "(A)") "* CHECK BALANCER PENDING"
    write (ERROR_UNIT, "(A)") "* =================================================="
    write (ERROR_UNIT, "(A,1X,L1)") "PENDING", balancer%is_pending ()
  end subroutine check_is_pending

  subroutine check_resource_group ()
    integer :: i
    integer, dimension(:), allocatable :: worker_group
    write (ERROR_UNIT, "(A)") "* =================================================="
    write (ERROR_UNIT, "(A)") "* CHECK RESOURCE GROUP"
    write (ERROR_UNIT, "(A)") "* =================================================="
    do i = 1, N_RESOURCES
       write (ERROR_UNIT, "(A,1X,I0,1X,L1)") "CHANNEL", i, balancer%has_resource_group (resource_id = i)
       if (balancer%has_resource_group (resource_id = i)) then
          call balancer%get_resource_group(resource_id = i, group = worker_group)
          write (ERROR_UNIT, "(A,999(1X,I0))") "=> GROUP", worker_group
       end if
    end do
  end subroutine check_resource_group

  subroutine check_resource_master ()
    integer :: i
    write (ERROR_UNIT, "(A)") "* =================================================="
    write (ERROR_UNIT, "(A)") "* CHECK RESOURCE MASTER"
    write (ERROR_UNIT, "(A)") "* =================================================="
    do i = 1, N_RESOURCES
       write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "CHANNEL", i, balancer%get_resource_master(resource_id = i)
    end do
  end subroutine check_resource_master
end program main
