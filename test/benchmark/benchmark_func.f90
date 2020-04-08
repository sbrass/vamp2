module benchmark_func
  use kinds, only: default
  use signal, only: signal_nanosleep
  use vamp2, only: vamp2_func_t

  implicit none

  private

  type, extends (vamp2_func_t) :: benchmark_func_t
     !
   contains
     procedure :: evaluate_maps => benchmark_func_evaluate_maps
     procedure :: evaluate_func => benchmark_func_evaluate_func
  end type benchmark_func_t

  public :: benchmark_func_t
contains
  subroutine benchmark_func_evaluate_maps (self, x)
    class(benchmark_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    integer :: i
    !! Do nothing.
    do i = 1, self%n_channel
       self%xi(:, i) = x
    end do
    self%det(:) = 1
    self%valid_x = .true.
  end subroutine benchmark_func_evaluate_maps

  real(default) function benchmark_func_evaluate_func (self, x) result (f)
    class(benchmark_func_t), intent(in) :: self
    real(default), dimension(:), intent(in) :: x
    real(default) :: sleep_time
    !! Sleep for 100 μseconds.
    ! call signal_nanosleep (seconds = 0, nseconds = 1)
    f = 1
  end function benchmark_func_evaluate_func
end module benchmark_func
