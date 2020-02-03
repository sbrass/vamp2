module test_func
  use kinds, only: default
  use constants, only: pi
  use vegas, only: vegas_func_t

  implicit none

  private

  real(default), parameter, public :: test_func_exact_result = &
       1.393203929685678 !! Integration domain: (0, π)³.

  type, extends (vegas_func_t) :: test_func_t
   contains
     procedure :: evaluate => test_func_evaluate
  end type test_func_t

  public :: test_func_t
contains
  real(default) function test_func_evaluate (self, x) result (f)
    class(test_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    f = 1.0 / (pi**3)
    f = f / ( 1.0 - cos (x(1)) * cos (x(2)) * cos (x(3)))
  end function test_func_evaluate

end module test_func

program main
  use iso_fortran_env, only: ERROR_UNIT
  use kinds, only: default
  use constants, only: pi
  use rng_base
  use rng_stream

  use vegas
  use test_func

  use mpi_f08

  implicit none

  type(vegas_t) :: mc
  class(rng_t), allocatable :: rng
  class(vegas_func_t), allocatable :: func

  real(default), dimension(3), parameter :: x_lower = 0, &
       x_upper = pi
  real(default) :: result, abserr

  call MPI_INIT ()

  allocate (rng_stream_t :: rng)
  call rng%init ()

  allocate (test_func_t :: func)
  mc = vegas_t (n_dim = 3)

  call mc%set_limits (x_lower, x_upper)

  write (ERROR_UNIT, "(A,1X,I0)") "Warmup calls:", 10000
  call mc%set_calls (10000)

  call mc%integrate (func, rng, 3, result=result, abserr=abserr)
  write (ERROR_UNIT, "(A,1X,F9.7,1X,A,1X,F9.7)") "Warmup:", result, "±", abserr

  write (ERROR_UNIT, "(A,1X,I0)") "Final calls:", 2000
  call mc%set_calls (2000)
  call mc%integrate (func, rng, 3, opt_refine_grid = .false., result=result, abserr=abserr)
  write (ERROR_UNIT, "(A,1X,F9.7,1X,A,1X,F9.7)") "Final:", result, "±", abserr
  write (ERROR_UNIT, "(A,1X,F9.7)") "Exact:", test_func_exact_result
  write (ERROR_UNIT, "(A,1X,F9.7,1X,A,1X,F9.7,1X,A)") "Error:", (test_func_exact_result - result), &
       "=", abs (test_func_exact_result - result) / abserr, "sigma"

  call mc%final ()
  call rng%final ()

  call MPI_FINALIZE ()
end program main
