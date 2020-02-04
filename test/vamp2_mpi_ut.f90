module test_func
  use kinds, only: default
  use constants, only: pi
  use vamp2, only: vamp2_func_t

  implicit none

  private

  type, extends (vamp2_func_t) :: test_func_t
   contains
     procedure :: evaluate_maps => test_func_evaluate_maps
     procedure :: evaluate_func => test_func_evaluate_func
  end type test_func_t

  public :: test_func_t
contains
  subroutine test_func_evaluate_maps (self, x)
    class(test_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    select case (self%current_channel)
    case (1)
       self%xi(:, 1) = x
       self%xi(1, 2) = x(1) * x(2)
       self%xi(2, 2) = 0.5 * ( 1. + log(x(1) / x(2)) / log(x(1) * x(2)))
    case (2)
       self%xi(1, 1) = x(1)**x(2)
       self%xi(2, 1) = x(1)**(1. - x(2))
       self%xi(:, 2) = x
    end select
    self%det(1) = 1.
    self%det(2) = abs (log(self%xi(1, 2)))
    self%valid_x = .true.
  end subroutine test_func_evaluate_maps

  real(default) function test_func_evaluate_func (self, x) result (f)
    class(test_func_t), intent(in) :: self
    real(default), dimension(:), intent(in) :: x
    f = 4. * sin(pi * self%xi(1, 1))**2 * sin(pi * self%xi(2, 1))**2 &
         + 2. * sin(pi * self%xi(2, 2))**2
  end function test_func_evaluate_func
end module test_func

program main
  use iso_fortran_env, only: ERROR_UNIT
  use kinds, only: default
  use format_defs, only: FMT_12

  use rng_base
  use rng_stream

  use test_func

  use vamp2

  use mpi_f08

  implicit none

  type(vamp2_t) :: mc
  class(rng_t), allocatable :: rng
  class(vamp2_func_t), allocatable :: func
  real(default), dimension(2), parameter :: x_lower = 0., &
       x_upper = 1.
  real(default) :: result, abserr

  call MPI_INIT ()

  allocate (rng_stream_t :: rng)
  call rng%init ()

  allocate (test_func_t :: func)
  call func%init (n_dim = 2, n_channel = 2)

  mc = vamp2_t (2, 2)

  call mc%set_limits (x_lower, x_upper)
  call mc%set_calls (1000)


  call mc%integrate (func, rng, 3, opt_verbose = .true., result=result, abserr=abserr)
  write (ERROR_UNIT, "(A," // FMT_12 // ",A," // FMT_12 // ")") "Result:", result, "±", abserr

  call mc%set_calls (200)
  call mc%integrate (func, rng, 3, opt_verbose = .true., result=result, abserr=abserr)
  write (ERROR_UNIT, "(A," // FMT_12 // ",1X,A,1X," // FMT_12 // ")") "Result:", result, "±", abserr

  call mc%final ()
  call rng%final ()

  call MPI_FINALIZE ()
end program main
