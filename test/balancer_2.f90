!! Skip partition during initialization and add partition later.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use partition
  use request_balancer

  implicit none

  type(request_balancer_t) :: balancer

  integer, parameter :: N_WORKERS = 2, &
       N_RESOURCES = 2
  type(partition_t), dimension(:), allocatable :: partition
  integer, dimension(N_WORKERS) :: map = [2, 1]

  call balancer%init (n_workers = n_workers, n_resources = n_resources, skip_partition = .true.)

  allocate (partition(2))
  call partition(1)%init ("External partition SINGLE.", PARTITION_SINGLE, 1)
  call partition(2)%init ("External partition ALL.", PARTITION_ALL, 1)
  call balancer%add_partition (partition, map)

  call balancer%write (ERROR_UNIT)

  ! call balancer%assign_worker (1, i_resource)
  ! write (ERROR_UNIT, *) "ASSIGN WORKER", 1, "TO", i_resource
  ! call balancer%assign_worker (2, i_resource)
  ! write (ERROR_UNIT, *) "ASSIGN WORKER", 2, "TO", i_resource
  ! call balancer%free_worker (2)
  ! write (ERROR_UNIT, *) "FREE WORKER", 2
  ! call balancer%free_worker (1)
  ! write (ERROR_UNIT, *) "FREE WORKER", 1
end program main
