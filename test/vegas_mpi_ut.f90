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

  integer :: rank

  call MPI_INIT ()
  call MPI_COMM_RANK (MPI_COMM_WORLD, rank)

  allocate (rng_stream_t :: rng)
  call rng%init ()

  allocate (test_func_t :: func)
  mc = vegas_t (n_dim = 3)

  call mc%set_limits (x_lower, x_upper)

  call mc%set_comm (MPI_COMM_WORLD)

  call mc%set_calls (10000)
  call mc%integrate (func, rng, 3, result=result, abserr=abserr)
  if (rank == 0) &
       call print_result (10000, result, test_func_exact_result, abserr)

  call mc%set_calls (2000)
  call mc%integrate (func, rng, 3, refine_grid = .false., result=result, abserr=abserr)
  if (rank == 0) &
       call print_result (2000, result, test_func_exact_result, abserr)

  call mc%final ()
  call rng%final ()

  call MPI_FINALIZE ()
contains
  subroutine print_result (n_calls, val, exact_val, err)
    integer, intent(in) :: n_calls
    real(default), intent(in) :: val, exact_val, err
    write (ERROR_UNIT, "(A,1X,I0)") "Calls:", n_calls
    write (ERROR_UNIT, "(A,1X,F9.7,1X,A,1X,F9.7)") "Result:", val, "±", err
    write (ERROR_UNIT, "(A,1X,F9.7)") "Exact:", exact_val
    write (ERROR_UNIT, "(A,1X,F9.7,1X,A,1X,F9.7,1X,A)") "Error:", (exact_val - val), &
         "=", abs (exact_val - val) / err, "sigma"
  end subroutine print_result
end program main
