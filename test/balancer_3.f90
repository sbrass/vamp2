!! Fix the interface and basic functionality of request_balancer_t.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use request_balancer

  implicit none

  type(request_balancer_t) :: balancer

  integer, parameter :: N_WORKERS = 2, &
       N_RESOURCES = 2

  real(r64), dimension(N_RESOURCES) :: resource_weight = 0

  integer :: i, i_resource, i_worker

  resource_weight = [1, 5]

  call balancer%init (n_workers, n_resources)
  call balancer%add_resource_weight (resource_weight)
  call balancer%write (ERROR_UNIT)
end program main
