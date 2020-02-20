!! Fix the interface and basic functionality of request_balancer_t.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use balancer_base
  use channel_balancer

  use test_utils

  implicit none

  class(balancer_base_t), allocatable :: balancer

  integer, parameter :: N_WORKERS = 120, &
       N_CHANNELS = 100

  real(r64), dimension(N_CHANNELS) :: channel_weight = 0
  logical, dimension(N_CHANNELS) :: parallel_grid = .false.

  !! Retrieve resource weight from n_box_parallel and channel weights.
  !! That's our prior allowing different resources to be greedy.
  channel_weight(1:10) = 1
  channel_weight(11:40) = 2
  channel_weight(41:70) = 3
  channel_weight(71:100) = 1
  parallel_grid = channel_weight > 1
  channel_weight = channel_weight / sum (channel_weight)

  allocate (channel_balancer_t :: balancer)
  select type (balancer)
  type is (channel_balancer_t)
     call balancer%init (n_workers, n_channels)
     call balancer%update_state (channel_weight, parallel_grid)
  end select
  call balancer%write (ERROR_UNIT)
end program main
