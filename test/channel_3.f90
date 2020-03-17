!! Apply the load balancing to a randomly provided worker request.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use request_balancer
  use balancer_channel

  use test_utils

  implicit none

  class(request_balancer_t), allocatable :: balancer
  type(rng_t) :: generator

  integer, parameter :: N_WORKERS = 120, &
       N_CHANNELS = 100

  real(r64), dimension(N_CHANNELS) :: channel_weight = 0
  logical, dimension(N_CHANNELS) :: parallel_grid = .false.

  integer :: i, i_resource, i_worker

  !! Retrieve resource weight from n_box_parallel and channel weights.
  !! That's our prior allowing different resources to be greedy.
  channel_weight(1:10) = 5
  channel_weight(11:40) = 2
  channel_weight(41:70) = 3
  channel_weight(71:100) = 1
  channel_weight = channel_weight / sum (channel_weight)
  parallel_grid(11:70) = .true.

  call generator%init (1234)

  allocate (balancer_channel_t :: balancer)
  call balancer%init (n_workers, n_channels)
  select type (balancer)
  type is (balancer_channel_t)
     call balancer%add_parallel_grid (parallel_grid)
     call balancer%compute_resource_weight (channel_weight)
     call balancer%write (ERROR_UNIT)
  end select

  do while (balancer%is_pending ())
     i_worker = int ((N_WORKERS - 1) * generator%draw ()) + 1
     call balancer%assign_worker (i_worker, i_resource)
     ! write (ERROR_UNIT, *) "WORKER: ", i_worker, "->", i_resource
     call balancer%free_worker (i_worker)
  end do
end program main
