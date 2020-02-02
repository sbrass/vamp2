!! Apply the load balancing to a randomly provided worker request.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use request_balancer
  use channel_balancer

  use test_utils

  implicit none

  class(request_balancer_t), allocatable :: balancer

  integer, parameter :: N_WORKERS = 10, &
       N_CHANNELS = 5

  real(r64), dimension(N_CHANNELS) :: channel_weight = 0
  logical, dimension(N_CHANNELS) :: parallel_grid = .false.

  integer, dimension(N_WORKERS) :: resource_id = 0

  integer :: i

  !! Retrieve resource weight from n_box_parallel and channel weights.
  !! That's our prior allowing different resources to be greedy.
  channel_weight(1) = 4
  channel_weight(2) = 1
  channel_weight(3) = 10
  channel_weight(4) = 1
  channel_weight(5) = 1
  parallel_grid = channel_weight > 1
  channel_weight = channel_weight / sum (channel_weight)

  allocate (channel_balancer_t :: balancer)
  select type (balancer)
  type is (channel_balancer_t)
     call balancer%init (n_workers, n_channels)
     call balancer%add_parallel_grid (parallel_grid)
     call balancer%add_channel_weight (channel_weight)
  end select

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
  write(ERROR_UNIT, *) resource_id

  select type (balancer)
  type is (channel_balancer_t)
     call balancer%write (ERROR_UNIT)
  end select

  !! Order: 1 3 2 6 7 9 8 10 4 5
  call balancer%free_worker (1)
  call balancer%free_worker (3)
  call balancer%free_worker (2)
  call balancer%free_worker (6)
  call balancer%free_worker (7)
  call balancer%free_worker (9)
  call balancer%free_worker (8)
  call balancer%free_worker (10)
  call balancer%free_worker (4)
  call balancer%free_worker (5)
end program main
