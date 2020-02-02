!! Init request balancer.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use request_balancer

  implicit none

  type(request_balancer_t) :: balancer

  integer, parameter :: N_WORKERS = 2, &
       N_RESOURCES = 2

  call balancer%init (n_workers = n_workers, n_resources = n_resources)
  call balancer%write (ERROR_UNIT)
end program main
