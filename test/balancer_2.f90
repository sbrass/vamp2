!> Test balancer_channel_t implementation.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use kinds, only: default

  use balancer_base
  use balancer_channel

  implicit none

  class(balancer_base_t), allocatable :: balancer

  integer, parameter :: N_RESOURCES = 4, &
       N_WORKERS = 5
  logical, dimension(N_RESOURCES) :: parallel_grid = .false.
  real(default), dimension(N_RESOURCES) :: weight = 0

  integer :: worker, handler

  weight = [1, 1, 8, 3]
  weight = weight / sum (weight)
  parallel_grid(3) = .true.

  allocate (balancer_channel_t :: balancer)
  select type (balancer)
  type is (balancer_channel_t)
     call balancer%init (n_workers = n_workers, n_resources = n_resources)
     call balancer%update_state (weight, parallel_grid)
  end select
  call balancer%write (ERROR_UNIT)

  do while (balancer%is_pending ())
     !! Assign all workers.
     call balancer%assign_worker (worker_id = 3, resource_id = handler)
     write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "BALANCER", 3, handler
     call balancer%assign_worker (worker_id = 2, resource_id = handler)
     write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "BALANCER", 2, handler
     call balancer%assign_worker (worker_id = 1, resource_id = handler)
     write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "BALANCER", 1, handler
     call balancer%assign_worker (worker_id = 4, resource_id = handler)
     write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "BALANCER", 4, handler
     call balancer%assign_worker (worker_id = 5, resource_id = handler)
     write (ERROR_UNIT, "(A,1X,I0,1X,I0)") "BALANCER", 5, handler

     call balancer%write (ERROR_UNIT)

     !! Free all workers.
     call balancer%free_worker(worker_id = 5, resource_id = 3)
     write (ERROR_UNIT, "(A,1X,I0)") "FREE", 5
     call balancer%free_worker(worker_id = 2, resource_id = 2)
     write (ERROR_UNIT, "(A,1X,I0)") "FREE", 2
     call balancer%free_worker(worker_id = 1, resource_id = 4)
     write (ERROR_UNIT, "(A,1X,I0)") "FREE", 1
     call balancer%free_worker(worker_id = 3, resource_id = 1)
     write (ERROR_UNIT, "(A,1X,I0)") "FREE", 3
     call balancer%free_worker(worker_id = 4, resource_id = 3)
     write (ERROR_UNIT, "(A,1X,I0)") "FREE", 4
  end do

  call balancer%write (ERROR_UNIT)
end program main
