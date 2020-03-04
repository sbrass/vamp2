!! Apply the load balancing to a randomly provided worker request.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use balancer_base
  use channel_balancer

  implicit none

  class(balancer_base_t), allocatable :: balancer

  integer, parameter :: N_WORKERS = 10, &
       N_CHANNELS = 5

  real(r64), dimension(N_CHANNELS) :: channel_weight = 0
  logical, dimension(N_CHANNELS) :: parallel_grid = .false.

  integer, dimension(N_WORKERS) :: resource_id = 0

  !! Retrieve resource weight from n_box_parallel and channel weights.
  !! That's our prior allowing different resources to be greedy.
  channel_weight(1) = 4
  channel_weight(2) = 1
  channel_weight(3) = 10
  channel_weight(4) = 1
  channel_weight(5) = 1
  parallel_grid = channel_weight > 1
  channel_weight = channel_weight / sum (channel_weight)

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Setup"
  write (ERROR_UNIT, "(A)") "* =================================================="
  allocate (channel_balancer_t :: balancer)
  select type (balancer)
  type is (channel_balancer_t)
     call balancer%init (n_workers, n_channels)
     call balancer%update_state (channel_weight, parallel_grid)
  end select

  call balancer%write (ERROR_UNIT)
  call check_are_worker_pending ()
  call check_are_worker_assignable ()
  call check_is_pending ()
  call check_resource_group ()
  call check_resource_master ()

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* DRAIN BALANCER 1/2"
  write (ERROR_UNIT, "(A)") "* =================================================="

  !! Order: 3 1 2 6 4 5 9 10 8 7
  call balancer%assign_worker (3, resource_id(3))
  write (ERROR_UNIT, *) "WORKER", 3, resource_id(3)
  call balancer%assign_worker (1, resource_id(1))
  write (ERROR_UNIT, *) "WORKER", 1, resource_id(1)
  call balancer%assign_worker (2, resource_id(2))
  write (ERROR_UNIT, *) "WORKER", 2, resource_id(2)
  call balancer%assign_worker (6, resource_id(6))
  write (ERROR_UNIT, *) "WORKER", 6, resource_id(6)
  call balancer%assign_worker (4, resource_id(4))
  write (ERROR_UNIT, *) "WORKER", 4, resource_id(4)
  call balancer%assign_worker (5, resource_id(5))
  write (ERROR_UNIT, *) "WORKER", 5, resource_id(5)
  call balancer%assign_worker (9, resource_id(9))
  write (ERROR_UNIT, *) "WORKER", 9, resource_id(9)
  call balancer%assign_worker (10, resource_id(10))
  write (ERROR_UNIT, *) "WORKER", 10, resource_id(10)
  call balancer%assign_worker (8, resource_id(8))
  write (ERROR_UNIT, *) "WORKER", 8, resource_id(8)
  call balancer%assign_worker (7, resource_id(7))
  write (ERROR_UNIT, *) "WORKER", 7, resource_id(7)

  write (ERROR_UNIT, "(A)") "* =================================================="

  call balancer%write (ERROR_UNIT)
  call check_are_worker_pending ()
  call check_are_worker_assignable ()
  call check_is_pending ()
  call check_resource_group ()
  call check_resource_master ()

  !! Order: 1 3 2 6 7 9 8 10 4 5
  call balancer%free_worker (1, resource_id(1))
  call balancer%free_worker (3, resource_id(3))
  call balancer%free_worker (2, resource_id(2))
  call balancer%free_worker (6, resource_id(6))
  call balancer%free_worker (7, resource_id(7))
  call balancer%free_worker (9, resource_id(9))
  call balancer%free_worker (8, resource_id(8))
  call balancer%free_worker (10, resource_id(10))
  call balancer%free_worker (4, resource_id(4))
  call balancer%free_worker (5, resource_id(5))

  call check_are_worker_pending ()
  call check_are_worker_assignable ()
  call check_is_pending ()
  call check_resource_group ()
  call check_resource_master ()

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* DRAIN BALANCER 2/2"
  write (ERROR_UNIT, "(A)") "* =================================================="

  !! Second turn.
  !! Order: 2 1 | 6 7 3 5 4 8 9 10
  call balancer%assign_worker(2, resource_id(2))
  write (ERROR_UNIT, *) "WORKER", 2, resource_id(2)
  call balancer%assign_worker(1, resource_id(1))
  write (ERROR_UNIT, *) "WORKER", 1, resource_id(1)
  call balancer%assign_worker(6, resource_id(6))
  write (ERROR_UNIT, *) "WORKER", 6, resource_id(6)
  call balancer%assign_worker(7, resource_id(7))
  write (ERROR_UNIT, *) "WORKER", 7, resource_id(7)
  call balancer%assign_worker(3, resource_id(3))
  write (ERROR_UNIT, *) "WORKER", 3, resource_id(3)
  call balancer%assign_worker(5, resource_id(5))
  write (ERROR_UNIT, *) "WORKER", 5, resource_id(5)
  call balancer%assign_worker(4, resource_id(4))
  write (ERROR_UNIT, *) "WORKER", 4, resource_id(4)
  call balancer%assign_worker(8, resource_id(8))
  write (ERROR_UNIT, *) "WORKER", 8, resource_id(8)
  call balancer%assign_worker(9, resource_id(9))
  write (ERROR_UNIT, *) "WORKER", 9, resource_id(9)
  call balancer%assign_worker(10, resource_id(10))
  write (ERROR_UNIT, *) "WORKER", 10, resource_id(10)

  write (ERROR_UNIT, "(A)") "* =================================================="

  call balancer%write (ERROR_UNIT)
  call check_are_worker_pending ()
  call check_are_worker_assignable ()
  call check_is_pending ()
  call check_resource_group ()
  call check_resource_master ()

  call balancer%free_worker(2, resource_id(2))
  call balancer%free_worker(1, resource_id(1))
  call balancer%free_worker(6, resource_id(6))
  call balancer%free_worker(7, resource_id(7))
  call balancer%free_worker(3, resource_id(3))
  call balancer%free_worker(5, resource_id(5))
  call balancer%free_worker(4, resource_id(4))
  call balancer%free_worker(8, resource_id(8))
  call balancer%free_worker(9, resource_id(9))
  call balancer%free_worker(10, resource_id(10))

  write (ERROR_UNIT, "(A)") "* =================================================="
  write (ERROR_UNIT, "(A)") "* Finish"
  write (ERROR_UNIT, "(A)") "* =================================================="
  call balancer%write (ERROR_UNIT)
  call check_are_worker_pending ()
  call check_are_worker_assignable ()
  call check_is_pending ()
  call check_resource_group ()
  call check_resource_master ()
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
    do i = 1, N_CHANNELS
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
    do i = 1, N_CHANNELS
       write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "CHANNEL", i, balancer%get_resource_master(resource_id = i)
    end do
  end subroutine check_resource_master
end program main
