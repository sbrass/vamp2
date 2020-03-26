! WHIZARD 2.8.3 Oct 24 2019
!
! Copyright (C) 1999-2019 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

module vegas
  use kinds, only: default

  use diagnostics
  use io_units
  use format_utils, only: write_indent
  use format_defs, only: FMT_17
  use rng_base, only: rng_t
  use rng_stream, only: rng_stream_t

  implicit none
  private

  public :: vegas_func_t
  public :: vegas_config_t
  public :: vegas_grid_t
  public :: operator (==)
  public :: vegas_result_t
  public :: vegas_t

  integer, parameter, public :: VEGAS_MODE_IMPORTANCE = 0, &
       & VEGAS_MODE_STRATIFIED = 1, VEGAS_MODE_IMPORTANCE_ONLY = 2
  character(len=*), parameter, private :: &
     descr_fmt =         "(1X,A)", &
     integer_fmt =       "(1X,A18,1X,I15)", &
     integer_array_fmt = "(1X,I18,1X,I15)", &
     logical_fmt =       "(1X,A18,1X,L1)", &
     double_fmt =        "(1X,A18,1X,E24.16E4)", &
     double_array_fmt =  "(1X,I18,1X,E24.16E4)", &
     double_array_pac_fmt = "(1X,I18,1X,E16.8E4)", &
     double_array2_fmt =  "(1X,2(1X,I8),1X,E24.16E4)", &
     double_array2_pac_fmt = "(1X,2(1X,I8),1X,E16.8E4)"


  type, abstract :: vegas_func_t
     !
   contains
     procedure(vegas_func_evaluate), deferred, pass, public :: evaluate
  end type vegas_func_t

  type :: vegas_config_t
     integer :: n_dim = 0
     real(default) :: alpha = 1.5
     integer :: n_bins_max = 50
     integer :: iterations = 5
     integer :: mode = VEGAS_MODE_STRATIFIED
     integer :: calls_per_box = 0
     integer :: n_calls = 0
     integer :: n_calls_min = 20
     integer :: n_boxes = 1
     integer :: n_bins = 1
   contains
     procedure, public :: write => vegas_config_write
  end type vegas_config_t

  type :: vegas_grid_t
     integer :: n_dim = 1
     integer :: n_bins = 1
     real(default), dimension(:), allocatable :: x_lower
     real(default), dimension(:), allocatable :: x_upper
     real(default), dimension(:), allocatable :: delta_x
     real(default), dimension(:,:), allocatable :: xi
   contains
     procedure, public :: write => vegas_grid_write
     procedure, private :: resize => vegas_grid_resize
     procedure, public :: get_probability => vegas_grid_get_probability
  end type vegas_grid_t

  type :: vegas_result_t
     integer :: it_start = 0
     integer :: it_num = 0
     integer :: samples = 0
     real(default) :: sum_int_wgtd = 0.
     real(default) :: sum_wgts
     real(default) :: sum_chi = 0.
     real(default) :: chi2 = 0.
     real(default) :: efficiency = 0.
     real(default) :: efficiency_pos = 0.
     real(default) :: efficiency_neg = 0.
     real(default) :: max_abs_f = 0.
     real(default) :: max_abs_f_pos = 0.
     real(default) :: max_abs_f_neg = 0.
     real(default) :: result = 0.
     real(default) :: std = 0.
     real(default) :: evt_weight = 0.
     real(default) :: evt_weight_excess = 0.
   contains
     procedure, public :: write => vegas_result_write
     procedure, public :: update => vegas_result_update
     procedure, public :: update_efficiency => vegas_result_update_efficiency
     procedure, public :: reset => vegas_result_reset
  end type vegas_result_t

  type :: vegas_t
     private
     type(vegas_config_t) :: config
     real(default) :: hypercube_volume = 0.
     real(default) :: jacobian = 0.
     real(default), dimension(:, :), allocatable :: d
     type(vegas_grid_t) :: grid
     integer, dimension(:), allocatable :: bin
     integer, dimension(:), allocatable :: box
     type(vegas_result_t) :: result
   contains
     procedure, public :: final => vegas_final
     procedure, public :: set_limits => vegas_set_limits
     procedure, public :: set_calls => vegas_set_n_calls
     procedure, public :: get_grid => vegas_get_grid
     procedure, public :: get_config => vegas_get_config
     procedure, public :: set_config => vegas_set_config
     procedure, public :: get_result => vegas_get_result
     procedure, public :: get_calls => vegas_get_n_calls
     procedure, public :: get_integral => vegas_get_integral
     procedure, public :: get_variance => vegas_get_variance
     procedure, public :: get_efficiency => vegas_get_efficiency
     procedure, public :: get_max_abs_f => vegas_get_max_abs_f
     procedure, public :: get_sum_abs_f_pos => vegas_get_sum_abs_f_pos
     procedure, public :: get_sum_abs_f_neg => vegas_get_sum_abs_f_neg
     procedure, public :: get_max_abs_f_pos => vegas_get_max_abs_f_pos
     procedure, public :: get_max_abs_f_neg => vegas_get_max_abs_f_neg
     procedure, public :: get_evt_weight => vegas_get_evt_weight
     procedure, public :: get_evt_weight_excess => vegas_get_evt_weight_excess
     procedure, public :: get_distribution => vegas_get_distribution
     procedure, public :: set_distribution => vegas_set_distribution
     procedure, private :: init_grid => vegas_init_grid
     procedure, public :: reset_result => vegas_reset_result
     procedure, public :: reset_grid => vegas_reset_grid
     procedure, public :: refine => vegas_refine_grid
     procedure, public :: integrate => vegas_integrate
     procedure, private :: random_point => vegas_random_point
     procedure, private :: simple_random_point => vegas_simple_random_point
     procedure, private :: accumulate_distribution => vegas_accumulate_distribution
     procedure :: generate_weighted => vegas_generate_weighted_event
     procedure, public :: generate_unweighted=> vegas_generate_unweighted_event
     procedure, public :: write_grid => vegas_write_grid
     procedure, public :: read_grid => vegas_read_grid
     procedure :: write_binary_grid => vegas_write_binary_grid
     procedure :: read_binary_grid => vegas_read_binary_grid
  end type vegas_t


  abstract interface
     real(default) function vegas_func_evaluate (self, x) result (f)
       import :: default, vegas_func_t
       class(vegas_func_t), intent(inout) :: self
       real(default), dimension(:), intent(in) :: x
     end function vegas_func_evaluate
  end interface

  interface vegas_grid_t
     module procedure vegas_grid_init
  end interface vegas_grid_t

  interface operator (==)
     module procedure vegas_grid_equal
  end interface operator (==)
  interface vegas_t
     module procedure vegas_init
  end interface vegas_t

contains

  subroutine vegas_config_write (self, unit, indent)
    class(vegas_config_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of dimensions            = ", self%n_dim
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Adaption power (alpha)          = ", self%alpha
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Max. number of bins (per dim.)  = ", self%n_bins_max
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of iterations            = ", self%iterations
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Mode (stratified or importance) = ", self%mode
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Calls per box                   = ", self%calls_per_box
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of calls                 = ", self%n_calls
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Min. number of calls            = ", self%n_calls_min
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of bins                  = ", self%n_bins
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of boxes                 = ", self%n_boxes
  end subroutine vegas_config_write

  type(vegas_grid_t) function vegas_grid_init (n_dim, n_bins_max) result (self)
    integer, intent(in) :: n_dim
    integer, intent(in) :: n_bins_max
    self%n_dim = n_dim
    self%n_bins = 1
    allocate (self%x_upper(n_dim), source=1.0_default)
    allocate (self%x_lower(n_dim), source=0.0_default)
    allocate (self%delta_x(n_dim), source=1.0_default)
    allocate (self%xi((n_bins_max + 1), n_dim), source=0.0_default)
  end function vegas_grid_init

  subroutine vegas_grid_write (self, unit, pacify)
    class(vegas_grid_t), intent(in) :: self
    integer, intent(in), optional :: unit
    logical, intent(in), optional :: pacify
    logical :: pac
    integer :: u, i, j
    pac = .false.; if (present (pacify))  pac = pacify
    u = given_output_unit (unit)
    write (u, descr_fmt) "begin vegas_grid_t"
    write (u, integer_fmt) "n_dim = ", self%n_dim
    write (u, integer_fmt) "n_bins = ", self%n_bins
    write (u, descr_fmt) "begin x_lower"
    do j = 1, self%n_dim
       if (pac) then
          write (u, double_array_pac_fmt)  j, self%x_lower(j)
       else
          write (u, double_array_fmt)  j, self%x_lower(j)
       end if
    end do
    write (u, descr_fmt) "end x_lower"
    write (u, descr_fmt) "begin x_upper"
    do j = 1, self%n_dim
       if (pac) then
          write (u, double_array_pac_fmt) j, self%x_upper(j)
       else
          write (u, double_array_fmt) j, self%x_upper(j)
       end if
    end do
    write (u, descr_fmt) "end x_upper"
    write (u, descr_fmt) "begin delta_x"
    do j = 1, self%n_dim
       if (pac) then
          write (u, double_array_pac_fmt)  j, self%delta_x(j)
       else
          write (u, double_array_fmt)  j, self%delta_x(j)
       end if
    end do
    write (u, descr_fmt) "end delta_x"
    write (u, descr_fmt) "begin xi"
    do j = 1, self%n_dim
       do i = 1, self%n_bins + 1
          if (pac) then
             write (u, double_array2_pac_fmt) i, j, self%xi(i, j)
          else
             write (u, double_array2_fmt) i, j, self%xi(i, j)
          end if
       end do
    end do
    write (u, descr_fmt) "end xi"
    write (u, descr_fmt) "end vegas_grid_t"
  end subroutine vegas_grid_write

  logical function vegas_grid_equal (grid_a, grid_b) result (yorn)
    type(vegas_grid_t), intent(in) :: grid_a, grid_b
    yorn = .true.
    yorn = yorn .and. (grid_a%n_dim == grid_b%n_dim)
    yorn = yorn .and. (grid_a%n_bins == grid_b%n_bins)
    yorn = yorn .and. all (grid_a%x_lower == grid_b%x_lower)
    yorn = yorn .and. all (grid_a%x_upper == grid_b%x_upper)
    yorn = yorn .and. all (grid_a%delta_x == grid_b%delta_x)
    yorn = yorn .and. all (grid_a%xi == grid_b%xi)
  end function vegas_grid_equal

  subroutine vegas_grid_resize (self, n_bins, w)
    class(vegas_grid_t), intent(inout) :: self
    integer, intent(in) :: n_bins
    real(default), dimension(:, :), intent(in) :: w
    real(default), dimension(size(self%xi)) :: xi_new
    integer :: i, j, k
    real(default) :: pts_per_bin
    real(default) :: d_width
    do j = 1, self%n_dim
       if (self%n_bins /= n_bins) then
          pts_per_bin = real(self%n_bins, default) / real(n_bins, default)
          self%n_bins = n_bins
       else
          if (all (w(:, j) == 0.)) then
             call msg_bug ("[VEGAS] grid_resize: resize weights are zero.")
          end if
          pts_per_bin = sum(w(:, j)) / self%n_bins
       end if
       d_width = 0.
       k = 0
       do i = 2, self%n_bins
          do while (pts_per_bin > d_width)
             k = k + 1
             d_width = d_width + w(k, j)
          end do
          d_width = d_width - pts_per_bin
          associate (x_upper => self%xi(k + 1, j), x_lower => self%xi(k, j))
            xi_new(i) = x_upper - (x_upper - x_lower) * d_width / w(k, j)
          end associate
       end do
       self%xi(:, j) = 0. ! Reset grid explicitly
       self%xi(2:n_bins, j) = xi_new(2:n_bins)
       self%xi(n_bins + 1, j) = 1.
    end do
  end subroutine vegas_grid_resize

  function vegas_grid_get_probability (self, x) result (g)
    class(vegas_grid_t), intent(in) :: self
    real(default), dimension(:), intent(in) :: x
    integer, parameter :: N_BINARY_SEARCH = 100
    real(default) :: g, y
    integer :: j, i_lower, i_higher, i_mid
    g = 1.
    if (self%n_bins > N_BINARY_SEARCH) then
       g = binary_search (x)
    else
       g = linear_search (x)
    end if
    ! Move division to the end, which is more efficient.
    if (g /= 0) g = 1. / g
  contains
    real(default) function linear_search (x) result (g)
      real(default), dimension(:), intent(in) :: x
      real(default) :: y
      integer :: j, i
      g = 1.
      ndim: do j = 1, self%n_dim
         y = (x(j) - self%x_lower(j)) / self%delta_x(j)
         if (y >= 0. .and. y <= 1.) then
            do i = 2, self%n_bins + 1
               if (self%xi(i, j) > y) then
                 g = g * (self%delta_x(j) * &
                      & self%n_bins * (self%xi(i, j) - self%xi(i - 1, j)))
                 cycle ndim
              end if
           end do
           g = 0
           exit ndim
        else
           g = 0
           exit ndim
        end if
     end do ndim
   end function linear_search

   real(default) function binary_search (x) result (g)
     real(default), dimension(:), intent(in) :: x
     ndim: do j = 1, self%n_dim
        y = (x(j) - self%x_lower(j)) / self%delta_x(j)
        if (y >= 0. .and. y <= 1.) then
           i_lower = 1
           i_higher = self%n_bins + 1
           search: do
              if (i_lower >= (i_higher - 1)) then
                 g = g * (self%delta_x(j) * &
                      & self%n_bins * (self%xi(i_higher, j) - self%xi(i_higher - 1, j)))
                 cycle ndim
              end if
              i_mid = (i_higher + i_lower) / 2
              if (y > self%xi(i_mid, j)) then
                 i_lower = i_mid
              else
                 i_higher = i_mid
              end if
           end do search
        else
           g = 0.
           exit ndim
        end if
     end do ndim
   end function binary_search
 end function vegas_grid_get_probability

  subroutine vegas_result_write (self, unit, indent)
    class(vegas_result_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Start iteration                 = ", self%it_start
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Iteration number                = ", self%it_num
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Sample number                   = ", self%samples
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 //")") &
         & "Sum of weighted integrals       = ", self%sum_int_wgtd
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")")  &
         & "Sum of weights                  = ", self%sum_wgts
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Sum of chi                      = ", self%sum_chi
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "chi2                            = ", self%chi2
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Overall efficiency              = ", self%efficiency
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "f-positive efficiency           = ", self%efficiency_pos
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "f-negative efficiency           = ", self%efficiency_neg
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Maximum absolute overall value  = ", self%max_abs_f
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Maximum absolute positive value = ", self%max_abs_f_pos
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Maximum absolute negative value = ", self%max_abs_f_neg
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Integral (of latest iteration)  = ", self%result
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Standard deviation              = ", self%std
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Event weight                    = ", self%evt_weight
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Event weight excess             = ", self%evt_weight_excess
  end subroutine vegas_result_write

  subroutine vegas_result_update (result, integral, variance)
    class(vegas_result_t), intent(inout) :: result
    real(default), intent(in) :: integral
    real(default), intent(in) :: variance
    real(default) :: guarded_variance, sq_integral
    real(default) :: wgt, chi
    !! Guard against zero (or negative) variance.
    !! \Delta = I * \epsilon -> I = \Delta.
    if (variance < epsilon (integral) &
         * max (integral**2, tiny(integral))) then
       guarded_variance = &
            max ((epsilon (integral) * integral)**2, &
            tiny (integral))
    else
      guarded_variance = variance
    end if
    wgt = 1._default / guarded_variance
    sq_integral = integral**2
    result%result = integral
    result%std = sqrt (guarded_variance)
    result%samples = result%samples + 1
    if (result%samples == 1) then
       result%chi2 = 0._default
    else
       chi = integral
       if (result%sum_wgts > 0) then
          chi = chi - result%sum_int_wgtd / result%sum_wgts
       end if
       result%chi2 = result%chi2 * (result%samples - 2.0_default)
       result%chi2 = (wgt / (1._default + (wgt / result%sum_wgts))) &
            & * chi**2
       result%chi2 = result%chi2 / (result%samples - 1._default)
    end if
    result%sum_wgts = result%sum_wgts + wgt
    result%sum_int_wgtd = result%sum_int_wgtd + (integral * wgt)
    result%sum_chi = result%sum_chi + (sq_integral * wgt)
  end subroutine vegas_result_update

  subroutine vegas_result_update_efficiency (result, &
       n_calls, max_pos, max_neg, sum_pos, sum_neg)
    class(vegas_result_t), intent(inout) :: result
    integer, intent(in) :: n_calls
    real(default), intent(in) :: max_pos
    real(default), intent(in) :: max_neg
    real(default), intent(in) :: sum_pos
    real(default), intent(in) :: sum_neg
    result%max_abs_f_pos = n_calls * max_pos
    result%max_abs_f_neg = n_calls * max_neg
    result%max_abs_f = &
         & max (result%max_abs_f_pos, result%max_abs_f_neg)
    result%efficiency_pos = 0
    if (max_pos > 0) then
       result%efficiency_pos = &
            & sum_pos / max_pos
    end if
    result%efficiency_neg = 0
    if (max_neg > 0) then
       result%efficiency_neg = &
            & sum_neg / max_neg
    end if
    result%efficiency = 0
    if (result%max_abs_f > 0.) then
       result%efficiency = (sum_pos + sum_neg) &
            & / result%max_abs_f
    end if
  end subroutine vegas_result_update_efficiency

  subroutine vegas_result_reset (result)
    class(vegas_result_t), intent(inout) :: result
    result%sum_int_wgtd = 0
    result%sum_wgts = 0
    result%sum_chi = 0
    result%it_num = 0
    result%samples = 0
    result%chi2 = 0
    result%efficiency = 0
    result%efficiency_pos = 0
    result%efficiency_neg = 0
    result%max_abs_f = 0
    result%max_abs_f_pos = 0
    result%max_abs_f_neg = 0
  end subroutine vegas_result_reset

  type(vegas_t) function vegas_init (n_dim, alpha, n_bins_max, iterations, mode) result (self)
    integer, intent(in) :: n_dim
    integer, intent(in), optional :: n_bins_max
    real(default), intent(in), optional :: alpha
    integer, intent(in), optional :: iterations
    integer, intent(in), optional :: mode
    self%config%n_dim = n_dim
    if (present (alpha)) self%config%alpha = alpha
    if (present (n_bins_max)) self%config%n_bins_max = n_bins_max
    if (present (iterations)) self%config%iterations = iterations
    if (present (mode)) self%config%mode = mode
    self%grid = vegas_grid_t (n_dim, self%config%n_bins_max)
    allocate (self%d(self%config%n_bins_max, n_dim), source=0.0_default)
    allocate (self%box(n_dim), source=1)
    allocate (self%bin(n_dim), source=1)
    self%config%n_bins = 1
    self%config%n_boxes = 1
    call self%set_limits (self%grid%x_lower, self%grid%x_upper)
    call self%reset_grid ()
    call self%reset_result ()
  end function vegas_init

  subroutine vegas_final (self)
    class(vegas_t), intent(inout) :: self
    deallocate (self%grid%x_upper)
    deallocate (self%grid%x_lower)
    deallocate (self%grid%delta_x)
    deallocate (self%d)
    deallocate (self%grid%xi)
    deallocate (self%box)
    deallocate (self%bin)
  end subroutine vegas_final

  subroutine vegas_set_limits (self, x_lower, x_upper)
    class(vegas_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x_lower
    real(default), dimension(:), intent(in) :: x_upper
    if (size (x_lower) /= self%config%n_dim &
         & .or. size (x_upper) /= self%config%n_dim) then
       write (msg_buffer, "(A, I5, A, I5, A, I5)") &
            "VEGAS: [set_limits] n_dim of new lower/upper integration limit&
            & does not match previously set n_dim. ", self%config%n_dim, " =/=&
            & ", size (x_lower), " =/= ", size (x_upper)
       call msg_fatal ()
    end if
    if (any(x_upper < x_lower)) then
       call msg_fatal ("VEGAS: [set_limits] upper limits are smaller than lower limits.")
    end if
    if (any((x_upper - x_lower) > huge(0._default))) then
       call msg_fatal ("VEGAS: [set_limits] upper and lower limits exceed rendering.")
    end if
    self%grid%x_upper = x_upper
    self%grid%x_lower = x_lower
    self%grid%delta_x = self%grid%x_upper - self%grid%x_lower
    self%hypercube_volume = product (self%grid%delta_x)
    call self%reset_result ()
  end subroutine vegas_set_limits

  subroutine vegas_set_n_calls (self, n_calls)
    class(vegas_t), intent(inout) :: self
    integer, intent(in) :: n_calls
    if (.not. (n_calls > 0)) then
       write (msg_buffer, "(A, I5)") &
            "VEGAS: [set_calls] number of calls must be a positive number. Keep&
            & number of calls = ", self%config%n_calls
       call msg_warning ()
    else
       self%config%n_calls = max (n_calls, self%config%n_calls_min)
       if (self%config%n_calls /= n_calls) then
          write (msg_buffer, "(A,I5)") &
               "VEGAS: [set calls] number of calls is too few, reset to ", self%config%n_calls
          call msg_warning ()
       end if
       call self%init_grid ()
    end if
  end subroutine vegas_set_n_calls

  type(vegas_grid_t) function vegas_get_grid (self) result (grid)
    class(vegas_t), intent(in) :: self
    grid = self%grid
    grid%n_dim = self%config%n_dim
    grid%n_bins = self%config%n_bins
  end function vegas_get_grid

  subroutine vegas_get_config (self, config)
    class(vegas_t), intent(in) :: self
    type(vegas_config_t), intent(out) :: config
    config = self%config
  end subroutine vegas_get_config

  subroutine vegas_set_config (self, config)
    class(vegas_t), intent(inout) :: self
    class(vegas_config_t), intent(in) :: config
    self%config%alpha = config%alpha
    self%config%iterations = config%iterations
    self%config%mode = config%mode
    self%config%n_calls_min = config%n_calls_min
   end subroutine vegas_set_config

  type(vegas_result_t) function vegas_get_result (self) result (result)
    class(vegas_t), intent(in) :: self
    result = self%result
  end function vegas_get_result

  subroutine vegas_set_result (self, result)
    class(vegas_t), intent(inout) :: self
    type(vegas_result_t), intent(in) :: result
    self%result = result
  end subroutine vegas_set_result

  elemental real(default) function vegas_get_n_calls (self) result (n_calls)
    class(vegas_t), intent(in) :: self
    n_calls = self%config%n_calls
  end function vegas_get_n_calls

  elemental real(default) function vegas_get_integral (self) result (integral)
    class(vegas_t), intent(in) :: self
    integral = 0.
    if (self%result%sum_wgts > 0.) then
       integral = self%result%sum_int_wgtd / self%result%sum_wgts
    end if
  end function vegas_get_integral

  elemental real(default) function vegas_get_variance (self) result (variance)
    class(vegas_t), intent(in) :: self
    variance = 0.
    if (self%result%sum_wgts > 0.) then
       variance = 1.0 / self%result%sum_wgts
    end if
  end function vegas_get_variance

  elemental real(default) function vegas_get_efficiency (self) result (efficiency)
    class(vegas_t), intent(in) :: self
    efficiency = 0.
    if (self%result%efficiency > 0. ) then
       efficiency = self%result%efficiency
    end if
  end function vegas_get_efficiency

  elemental real(default) function vegas_get_max_abs_f (self) result (max_abs_f)
    class(vegas_t), intent(in) :: self
    max_abs_f = 0.
    if (self%result%max_abs_f > 0.) then
       max_abs_f = self%result%max_abs_f
    end if
  end function vegas_get_max_abs_f

  elemental real(default) function vegas_get_sum_abs_f_pos (self) result (sum_abs_f)
    class(vegas_t), intent(in) :: self
    sum_abs_f = self%result%efficiency_pos * self%result%max_abs_f_pos
  end function vegas_get_sum_abs_f_pos

  elemental real(default) function vegas_get_sum_abs_f_neg (self) result (sum_abs_f)
    class(vegas_t), intent(in) :: self
    sum_abs_f = self%result%efficiency_neg * self%result%max_abs_f_neg
  end function vegas_get_sum_abs_f_neg

  elemental real(default) function vegas_get_max_abs_f_pos (self) result (max_abs_f)
    class(vegas_t), intent(in) :: self
    max_abs_f = 0.
    if (self%result%max_abs_f_pos > 0.) then
       max_abs_f = self%result%max_abs_f_pos
    end if
  end function vegas_get_max_abs_f_pos

  elemental real(default) function vegas_get_max_abs_f_neg (self) result (max_abs_f)
    class(vegas_t), intent(in) :: self
    max_abs_f = 0.
    if (self%result%max_abs_f_neg > 0.) then
       max_abs_f = self%result%max_abs_f_neg
    end if
  end function vegas_get_max_abs_f_neg

  real(default) function vegas_get_evt_weight (self) result (evt_weight)
    class(vegas_t), intent(in) :: self
    evt_weight = self%result%evt_weight
  end function vegas_get_evt_weight

  real(default) function vegas_get_evt_weight_excess (self) result (evt_weight_excess)
    class(vegas_t), intent(in) :: self
    evt_weight_excess = self%result%evt_weight_excess
  end function vegas_get_evt_weight_excess

  function vegas_get_distribution (self) result (d)
    class(vegas_t), intent(in) :: self
    real(default), dimension(:, :), allocatable :: d
    d = self%d
  end function vegas_get_distribution

  subroutine vegas_set_distribution (self, d)
    class(vegas_t), intent(inout) :: self
    real(default), dimension(:, :), intent(in) :: d
    if (size (d, dim = 2) /= self%config%n_dim) then
       call msg_bug ("[VEGAS] set_distribution: new distribution has wrong size of dimension")
    end if
    if (size (d, dim = 1) /= self%config%n_bins_max) then
       call msg_bug ("[VEGAS] set_distribution: new distribution has wrong number of bins")
    end if
    self%d = d
  end subroutine vegas_set_distribution

  subroutine vegas_init_grid (self)
    class(vegas_t), intent(inout) :: self
    integer :: n_bins, n_boxes, box_per_bin, n_total_boxes
    real(default), dimension(:, :), allocatable :: w
    n_bins = self%config%n_bins_max
    n_boxes = 1
    if (self%config%mode /= VEGAS_MODE_IMPORTANCE_ONLY) then
       ! We try for 2 calls per box
       n_boxes = max (floor ((self%config%n_calls / 2.)**(1. / self%config%n_dim)), 1)
       self%config%mode = VEGAS_MODE_IMPORTANCE
       if (2 * n_boxes >= self%config%n_bins_max) then
          ! if n_bins/box < 2
          box_per_bin = max (n_boxes / self%config%n_bins_max, 1)
          n_bins =  min (n_boxes / box_per_bin, self%config%n_bins_max)
          n_boxes = box_per_bin * n_bins
          self%config%mode = VEGAS_MODE_STRATIFIED
       end if
    end if
    n_total_boxes = n_boxes**self%config%n_dim
    self%config%calls_per_box = max (floor (real (self%config%n_calls) / n_total_boxes), 2)
    self%config%n_calls = self%config%calls_per_box * n_total_boxes
    !  Total volume of x-space/(average n_calls per bin)
    self%jacobian = self%hypercube_volume * real(n_bins, default)&
         &**self%config%n_dim / real(self%config%n_calls, default)
    self%config%n_boxes = n_boxes
    if (n_bins /= self%config%n_bins) then
       allocate (w(self%config%n_bins, self%config%n_dim), source=1.0_default)
       call self%grid%resize (n_bins, w)
       self%config%n_bins = n_bins
    end if
  end subroutine vegas_init_grid

  subroutine vegas_reset_result (self)
    class(vegas_t), intent(inout) :: self
    call self%result%reset ()
  end subroutine vegas_reset_result

  subroutine vegas_reset_grid (self)
    class(vegas_t), intent(inout) :: self
    self%config%n_bins = 1
    self%d = 0._default
    self%grid%xi = 0._default
    self%grid%xi(1, :) = 0._default
    self%grid%xi(2, :) = 1._default
    call self%reset_result ()
  end subroutine vegas_reset_grid

  subroutine vegas_refine_grid (self)
    class(vegas_t), intent(inout) :: self
    integer :: j
    real(default), dimension(self%config%n_bins, self%config%n_dim) :: w
    ndim: do j = 1, self%config%n_dim
       call average_distribution (self%config%n_bins, self%d(:self%config&
            &%n_bins, j), self%config%alpha, w(:, j))
    end do ndim
    call self%grid%resize (self%config%n_bins, w)
  contains
      subroutine average_distribution (n_bins, d, alpha, w)
        integer, intent(in) :: n_bins
        real(default), dimension(:), intent(inout) :: d
        real(default), intent(in) :: alpha
        real(default), dimension(n_bins), intent(out) :: w
        if (n_bins > 2) then
           d(1) = (d(1) + d(2)) / 2.0_default
           d(2:n_bins - 1) = (d(1:n_bins - 2) + d(2:n_bins - 1) + d(3:n_bins)) /&
                & 3.0_default
           d(n_bins) = d(n_bins - 1) + d(n_bins) / 2.0_default
        end if
        w = 1.0_default
        if (.not. all (d < tiny (1.0_default))) then
           d = d / sum (d)
           where (d < tiny (1.0_default))
              d = tiny (1.0_default)
           end where
           where (d /= 1.0_default)
              w = ((d - 1.) / log(d))**alpha
           elsewhere
              ! Analytic limes for d -> 1
              w = 1.0_default
           end where
        end if
      end subroutine average_distribution

  end subroutine vegas_refine_grid

  subroutine vegas_integrate (self, func, rng, iterations, reset_result,&
       & refine_grid, verbose, result, abserr)
    class(vegas_t), intent(inout) :: self
    class(vegas_func_t), intent(inout) :: func
    class(rng_t), intent(inout) :: rng
    integer, intent(in), optional :: iterations
    logical, intent(in), optional :: reset_result
    logical, intent(in), optional :: refine_grid
    logical, intent(in), optional :: verbose
    real(default), optional, intent(out) :: result, abserr
    integer :: it, k
    real(default), dimension(self%config%n_dim) :: x
    real(default) :: fval, fval_sq, bin_volume
    real(default) :: fval_box, fval_sq_box
    real(default) :: total_integral, total_sq_integral
    real(default) :: cumulative_int, cumulative_std
    real(default) :: sum_abs_f_pos, max_abs_f_pos
    real(default) :: sum_abs_f_neg, max_abs_f_neg
    logical :: opt_reset_result
    logical :: opt_refine_grid
    logical :: opt_verbose
    integer :: n_size
    integer :: n_dim_par
    logical :: box_success
    ! MPI-specific variables below
    call set_options ()
    call self%init_grid ()
    if (opt_reset_result) call self%result%reset ()
    self%result%it_start = self%result%it_num
    cumulative_int = 0.
    cumulative_std = 0.
    n_dim_par = floor (self%config%n_dim / 2.)
    n_size = 1
    if (opt_verbose) then
       call msg_message ("Results: [it, calls, integral, error, chi^2, eff.]")
    end if
    iteration: do it = 1, self%config%iterations
       self%result%it_num = self%result%it_start + it
       self%d = 0.
       self%box = 1
       self%bin = 1
       total_integral = 0.
       total_sq_integral = 0.
       sum_abs_f_pos = 0.
       max_abs_f_pos = 0.
       sum_abs_f_neg = 0.
       max_abs_f_neg = 0.
       box_success = .true.
       select type (rng)
       type is (rng_stream_t)
          call rng%next_substream ()
       end select
       loop_over_par_boxes: do while (box_success)
          loop_over_perp_boxes: do while (box_success)
             fval_box = 0._default
             fval_sq_box = 0._default
             do k = 1, self%config%calls_per_box
                call self%random_point (rng, x, bin_volume)
                ! Call the function, yeah, call it...
                fval = self%jacobian * bin_volume * func%evaluate (x)
                fval_sq = fval**2
                fval_box = fval_box + fval
                fval_sq_box = fval_sq_box + fval_sq
                if (fval > 0.) then
                   max_abs_f_pos = max(abs (fval), max_abs_f_pos)
                   sum_abs_f_pos = sum_abs_f_pos + abs(fval)
                else
                   max_abs_f_neg = max(abs (fval), max_abs_f_neg)
                   sum_abs_f_neg = sum_abs_f_neg + abs(fval)
                end if
                if (self%config%mode /= VEGAS_MODE_STRATIFIED) then
                   call self%accumulate_distribution (fval_sq)
                end if
             end do
             fval_sq_box = sqrt (fval_sq_box * self%config%calls_per_box)
             ! (a - b) * (a + b) = a**2 - b**2
             fval_sq_box = (fval_sq_box - fval_box) * (fval_sq_box + fval_box)
             if (fval_sq_box <= 0.0) fval_sq_box = fval_box**2 * epsilon (1.0_default)
             total_integral = total_integral + fval_box
             total_sq_integral = total_sq_integral + fval_sq_box
             if (self%config%mode == VEGAS_MODE_STRATIFIED) then
                call self%accumulate_distribution (fval_sq_box)
             end if
             call increment_box_coord (self%box(n_dim_par + 1:self%config&
                  &%n_dim), box_success)
          end do loop_over_perp_boxes
          shift: do k = 1, n_size
             call increment_box_coord (self%box(1:n_dim_par), box_success)
             if (.not. box_success) exit shift
          end do shift

       end do loop_over_par_boxes

       associate (result => self%result)
         ! Compute final results for this iterations
         call result%update (total_integral, variance = &
               total_sq_integral / (self%config%calls_per_box - 1._default))
         call result%update_efficiency (n_calls = self%config%n_calls, &
              max_pos = max_abs_f_pos, max_neg = max_abs_f_neg, &
              sum_pos = sum_abs_f_pos, sum_neg = sum_abs_f_neg)
         cumulative_int = result%sum_int_wgtd / result%sum_wgts
         cumulative_std = sqrt (1 / result%sum_wgts)
       end associate
       if (opt_verbose) then
          write (msg_buffer, "(I0,1x,I0,1x, 4(E24.16E4,1x))") &
               & it, self%config%n_calls, cumulative_int, cumulative_std, &
               & self%result%chi2, self%result%efficiency
          call msg_message ()
       end if
       if (opt_refine_grid) call self%refine ()
    end do iteration
    if (present(result)) result = cumulative_int
    if (present(abserr)) abserr = abs(cumulative_std)
  contains
    subroutine set_options ()
      if (present (iterations)) self%config%iterations = iterations
      opt_reset_result = .true.
      if (present (reset_result)) opt_reset_result = reset_result
      opt_refine_grid = .true.
      if (present (refine_grid)) opt_refine_grid = refine_grid
      opt_verbose = .false.
      if (present (verbose)) opt_verbose = verbose
    end subroutine set_options

    subroutine increment_box_coord (box, success)
      integer, dimension(:), intent(inout) :: box
      logical, intent(out) :: success
      integer :: j
      success = .true.
      do j = size (box), 1, -1
         box(j) = box(j) + 1
         if (box(j) <= self%config%n_boxes) return
         box(j) = 1
      end do
      success = .false.
    end subroutine increment_box_coord

  end subroutine vegas_integrate

  subroutine vegas_random_point (self, rng, x, bin_volume)
    class(vegas_t), intent(inout) :: self
    class(rng_t), intent(inout) :: rng
    real(default), dimension(self%config%n_dim), intent(out) :: x
    real(default), intent(out) :: bin_volume
    integer :: j
    real(default) :: r, y, z, bin_width
    bin_volume = 1.
    ndim: do j = 1, self%config%n_dim
       call rng%generate (r)
       z = ((self%box(j) - 1 + r) / self%config%n_boxes) * self%config%n_bins + 1
       self%bin(j) = max (min (int (z), self%config%n_bins), 1)
       if (self%bin(j) == 1) then
          bin_width = self%grid%xi(2, j)
          y = (z - self%bin(j)) * bin_width
       else
          bin_width = self%grid%xi(self%bin(j) + 1, j) - self%grid%xi(self%bin(j), j)
          y = self%grid%xi(self%bin(j), j) + (z - self%bin(j)) * bin_width
       end if
       x(j) = self%grid%x_lower(j) + y * self%grid%delta_x(j)
       bin_volume = bin_volume * bin_width
    end do ndim
  end subroutine vegas_random_point

  subroutine vegas_simple_random_point (self, rng, x, bin_volume)
    class(vegas_t), intent(inout) :: self
    class(rng_t), intent(inout) :: rng
    real(default), dimension(self%config%n_dim), intent(out) :: x
    real(default), intent(out) :: bin_volume
    integer :: j, k
    real(default) :: r, y, z, bin_width
    bin_volume = 1.
    ndim: do j = 1, self%config%n_dim
       call rng%generate (r)
       z = r * self%config%n_bins + 1
       k = max (min (int (z), self%config%n_bins), 1)
       if (k == 1) then
          bin_width = self%grid%xi(2, j)
          y = (z - 1) * bin_width
       else
          bin_width = self%grid%xi(k + 1, j) - self%grid%xi(k, j)
          y = self%grid%xi(k, j) + (z - k) * bin_width
       end if
       x(j) = self%grid%x_lower(j) + y * self%grid%delta_x(j)
       bin_volume = bin_volume * bin_width
    end do ndim
  end subroutine vegas_simple_random_point

  subroutine vegas_accumulate_distribution (self, y)
    class(vegas_t), intent(inout) :: self
    real(default), intent(in) :: y
    integer :: j
    do j = 1, self%config%n_dim
       self%d(self%bin(j), j) = self%d(self%bin(j), j) + y
    end do
  end subroutine vegas_accumulate_distribution

  subroutine vegas_generate_weighted_event (self, func, rng, x)
    class(vegas_t), intent(inout) :: self
    class(vegas_func_t), intent(inout) :: func
    class(rng_t), intent(inout) :: rng
    real(default), dimension(self%config%n_dim), intent(inout) :: x
    real(default) :: bin_volume
    call self%simple_random_point (rng, x, bin_volume)
    ! Cancel n_calls from jacobian with n_calls
    self%result%evt_weight = self%config%n_calls * self%jacobian * bin_volume &
         & * func%evaluate (x)
  end subroutine vegas_generate_weighted_event

  subroutine vegas_generate_unweighted_event (self, func, rng, x)
    class(vegas_t), intent(inout) :: self
    class(vegas_func_t), intent(inout) :: func
    class(rng_t), intent(inout) :: rng
    real(default), dimension(self%config%n_dim), intent(out) :: x
    real(default) :: bin_volume
    real(default) :: max_abs_f
    real(default) :: r
    associate (result => self%result)
      generate: do
         call self%generate_weighted (func, rng, x)
         max_abs_f = merge (result%max_abs_f_pos, result%max_abs_f_neg, &
              & result%evt_weight > 0.)
         if (result%evt_weight > max_abs_f) then
            result%evt_weight_excess = &
                 & result%evt_weight / max_abs_f - 1._default
            exit generate
         end if
         call rng%generate (r)
         ! Do not use division, because max_abs_f could be zero.
         if (max_abs_f * r <= abs(result%evt_weight)) then
          exit generate
         end if
      end do generate
    end associate
  end subroutine vegas_generate_unweighted_event

  subroutine vegas_write_grid (self, unit)
    class(vegas_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer :: u
    integer :: i, j
    u = given_output_unit (unit)
    write (u, descr_fmt) "begin type(vegas_t)"
    write (u, integer_fmt) "n_dim =", self%config%n_dim
    write (u, integer_fmt) "n_bins_max =", self%config%n_bins_max
    write (u, double_fmt) "alpha =", self%config%alpha
    write (u, integer_fmt) "iterations =", self%config%iterations
    write (u, integer_fmt) "mode =", self%config%mode
    write (u, integer_fmt) "calls_per_box =", self%config%calls_per_box
    write (u, integer_fmt) "n_calls =", self%config%n_calls
    write (u, integer_fmt) "n_calls_min =", self%config%n_calls_min
    write (u, integer_fmt) "n_boxes =", self%config%n_boxes
    write (u, integer_fmt) "n_bins =", self%config%n_bins
    write (u, integer_fmt) "it_start =", self%result%it_start
    write (u, integer_fmt) "it_num =", self%result%it_num
    write (u, integer_fmt) "samples =", self%result%samples
    write (u, double_fmt) "sum_int_wgtd =", self%result%sum_int_wgtd
    write (u, double_fmt) "sum_wgts =", self%result%sum_wgts
    write (u, double_fmt) "sum_chi =", self%result%sum_chi
    write (u, double_fmt) "chi2 =", self%result%chi2
    write (u, double_fmt) "efficiency =", self%result%efficiency
    write (u, double_fmt) "efficiency =", self%result%efficiency_pos
    write (u, double_fmt) "efficiency =", self%result%efficiency_neg
    write (u, double_fmt) "max_abs_f =", self%result%max_abs_f
    write (u, double_fmt) "max_abs_f_pos =", self%result%max_abs_f_pos
    write (u, double_fmt) "max_abs_f_neg =", self%result%max_abs_f_neg
    write (u, double_fmt) "result =", self%result%result
    write (u, double_fmt) "std =", self%result%std
    write (u, double_fmt) "hypercube_volume =", self%hypercube_volume
    write (u, double_fmt) "jacobian =", self%jacobian
    write (u, descr_fmt) "begin x_lower"
    do j = 1, self%config%n_dim
       write (u, double_array_fmt) j, self%grid%x_lower(j)
    end do
    write (u, descr_fmt) "end x_lower"
    write (u, descr_fmt) "begin x_upper"
    do j = 1, self%config%n_dim
       write (u, double_array_fmt) j, self%grid%x_upper(j)
    end do
    write (u, descr_fmt) "end x_upper"
    write (u, descr_fmt) "begin delta_x"
    do j = 1, self%config%n_dim
       write (u, double_array_fmt) j, self%grid%delta_x(j)
    end do
    write (u, descr_fmt) "end delta_x"
    write (u, integer_fmt) "n_bins =", self%config%n_bins
    write (u, descr_fmt) "begin bin"
    do j = 1, self%config%n_dim
       write (u, integer_array_fmt) j, self%bin(j)
    end do
    write (u, descr_fmt) "end n_bin"
    write (u, integer_fmt) "n_boxes =", self%config%n_boxes
    write (u, descr_fmt) "begin box"
    do j = 1, self%config%n_dim
       write (u, integer_array_fmt) j, self%box(j)
    end do
    write (u, descr_fmt) "end box"
    write (u, descr_fmt) "begin d"
    do j = 1, self%config%n_dim
       do i = 1, self%config%n_bins_max
          write (u, double_array2_fmt) i, j, self%d(i, j)
       end do
    end do
    write (u, descr_fmt) "end d"
    write (u, descr_fmt) "begin xi"
    do j = 1, self%config%n_dim
       do i = 1, self%config%n_bins_max + 1
          write (u,  double_array2_fmt) i, j, self%grid%xi(i, j)
       end do
    end do
    write (u, descr_fmt) "end xi"
    write (u, descr_fmt) "end type(vegas_t)"
  end subroutine vegas_write_grid

  subroutine vegas_read_grid (self, unit)
    class(vegas_t), intent(out) :: self
    integer, intent(in) :: unit
    integer :: i, j
    character(len=80) :: buffer
    integer :: ibuffer, jbuffer
    read (unit, descr_fmt) buffer
    read (unit, integer_fmt) buffer, ibuffer
    read (unit, integer_fmt) buffer, jbuffer
    select type(self)
    type is (vegas_t)
       self = vegas_t (n_dim = ibuffer, n_bins_max = jbuffer)
    end select
    read (unit, double_fmt) buffer, self%config%alpha
    read (unit, integer_fmt) buffer, self%config%iterations
    read (unit, integer_fmt) buffer, self%config%mode
    read (unit, integer_fmt) buffer, self%config%calls_per_box
    read (unit, integer_fmt) buffer, self%config%n_calls
    read (unit, integer_fmt) buffer, self%config%n_calls_min
    read (unit, integer_fmt) buffer, self%config%n_boxes
    read (unit, integer_fmt) buffer, self%config%n_bins
    self%grid%n_bins = self%config%n_bins
    read (unit, integer_fmt) buffer, self%result%it_start
    read (unit, integer_fmt) buffer, self%result%it_num
    read (unit, integer_fmt) buffer, self%result%samples
    read (unit, double_fmt) buffer, self%result%sum_int_wgtd
    read (unit, double_fmt) buffer, self%result%sum_wgts
    read (unit, double_fmt) buffer, self%result%sum_chi
    read (unit, double_fmt) buffer, self%result%chi2
    read (unit, double_fmt) buffer, self%result%efficiency
    read (unit, double_fmt) buffer, self%result%efficiency_pos
    read (unit, double_fmt) buffer, self%result%efficiency_neg
    read (unit, double_fmt) buffer, self%result%max_abs_f
    read (unit, double_fmt) buffer, self%result%max_abs_f_pos
    read (unit, double_fmt) buffer, self%result%max_abs_f_neg
    read (unit, double_fmt) buffer, self%result%result
    read (unit, double_fmt) buffer, self%result%std
    read (unit, double_fmt) buffer, self%hypercube_volume
    read (unit, double_fmt) buffer, self%jacobian
    read (unit, descr_fmt) buffer
    do j = 1, self%config%n_dim
       read (unit, double_array_fmt) jbuffer, self%grid%x_lower(j)
    end do
    read (unit, descr_fmt) buffer
    read (unit, descr_fmt) buffer
    do j = 1, self%config%n_dim
       read (unit, double_array_fmt) jbuffer, self%grid%x_upper(j)
    end do
    read (unit, descr_fmt) buffer
    read (unit, descr_fmt) buffer
    do j = 1, self%config%n_dim
       read (unit, double_array_fmt) jbuffer, self%grid%delta_x(j)
    end do
    read (unit, descr_fmt) buffer
    read (unit, integer_fmt) buffer, self%config%n_bins
    read (unit, descr_fmt) buffer
    do j = 1, self%config%n_dim
       read (unit, integer_array_fmt) jbuffer, self%bin(j)
    end do
    read (unit, descr_fmt) buffer
    read (unit, integer_fmt) buffer, self%config%n_boxes
    read (unit, descr_fmt) buffer
    do j = 1, self%config%n_dim
       read (unit, integer_array_fmt) jbuffer, self%box(j)
    end do
    read (unit, descr_fmt) buffer
    read (unit, descr_fmt) buffer
    do j = 1, self%config%n_dim
       do i = 1, self%config%n_bins_max
          read (unit, double_array2_fmt) ibuffer, jbuffer, self%d(i, j)
       end do
    end do
    read (unit, descr_fmt) buffer
    read (unit, descr_fmt) buffer
    do j = 1, self%config%n_dim
       do i = 1, self%config%n_bins_max + 1
          read (unit,  double_array2_fmt) ibuffer, jbuffer, self%grid%xi(i, j)
       end do
    end do
    read (unit, descr_fmt) buffer
    read (unit, descr_fmt) buffer
  end subroutine vegas_read_grid

  subroutine vegas_write_binary_grid (self, unit)
    class(vegas_t), intent(in) :: self
    integer, intent(in) :: unit
    integer :: i, j
    write (unit) self%config%n_dim
    write (unit) self%config%n_bins_max
    write (unit) self%config%alpha
    write (unit) self%config%iterations
    write (unit) self%config%mode
    write (unit) self%config%calls_per_box
    write (unit) self%config%n_calls
    write (unit) self%config%n_calls_min
    write (unit) self%config%n_boxes
    write (unit) self%config%n_bins
    write (unit) self%result%it_start
    write (unit) self%result%it_num
    write (unit) self%result%samples
    write (unit) self%result%sum_int_wgtd
    write (unit) self%result%sum_wgts
    write (unit) self%result%sum_chi
    write (unit) self%result%chi2
    write (unit) self%result%efficiency
    write (unit) self%result%efficiency_pos
    write (unit) self%result%efficiency_neg
    write (unit) self%result%max_abs_f
    write (unit) self%result%max_abs_f_pos
    write (unit) self%result%max_abs_f_neg
    write (unit) self%result%result
    write (unit) self%result%std
    write (unit) self%hypercube_volume
    write (unit) self%jacobian
    do j = 1, self%config%n_dim
       write (unit) j, self%grid%x_lower(j)
    end do
    do j = 1, self%config%n_dim
       write (unit) j, self%grid%x_upper(j)
    end do
    do j = 1, self%config%n_dim
       write (unit) j, self%grid%delta_x(j)
    end do
    write (unit) self%config%n_bins
    do j = 1, self%config%n_dim
       write (unit) j, self%bin(j)
    end do
    write (unit) self%config%n_boxes
    do j = 1, self%config%n_dim
       write (unit) j, self%box(j)
    end do
    do j = 1, self%config%n_dim
       do i = 1, self%config%n_bins_max
          write (unit) i, j, self%d(i, j)
       end do
    end do
    do j = 1, self%config%n_dim
       do i = 1, self%config%n_bins_max + 1
          write (unit) i, j, self%grid%xi(i, j)
       end do
    end do
  end subroutine vegas_write_binary_grid

  subroutine vegas_read_binary_grid (self, unit)
    class(vegas_t), intent(out) :: self
    integer, intent(in) :: unit
    integer :: i, j
    integer :: ibuffer, jbuffer
    read (unit) ibuffer
    read (unit) jbuffer
    select type(self)
    type is (vegas_t)
       self = vegas_t (n_dim = ibuffer, n_bins_max = jbuffer)
    end select
    read (unit) self%config%alpha
    read (unit) self%config%iterations
    read (unit) self%config%mode
    read (unit) self%config%calls_per_box
    read (unit) self%config%n_calls
    read (unit) self%config%n_calls_min
    read (unit) self%config%n_boxes
    read (unit) self%config%n_bins
    self%grid%n_bins = self%config%n_bins
    read (unit) self%result%it_start
    read (unit) self%result%it_num
    read (unit) self%result%samples
    read (unit) self%result%sum_int_wgtd
    read (unit) self%result%sum_wgts
    read (unit) self%result%sum_chi
    read (unit) self%result%chi2
    read (unit) self%result%efficiency
    read (unit) self%result%efficiency_pos
    read (unit) self%result%efficiency_neg
    read (unit) self%result%max_abs_f
    read (unit) self%result%max_abs_f_pos
    read (unit) self%result%max_abs_f_neg
    read (unit) self%result%result
    read (unit) self%result%std
    read (unit) self%hypercube_volume
    read (unit) self%jacobian
    do j = 1, self%config%n_dim
       read (unit) jbuffer, self%grid%x_lower(j)
    end do
    do j = 1, self%config%n_dim
       read (unit) jbuffer, self%grid%x_upper(j)
    end do
    do j = 1, self%config%n_dim
       read (unit) jbuffer, self%grid%delta_x(j)
    end do
    read (unit) self%config%n_bins
    do j = 1, self%config%n_dim
       read (unit) jbuffer, self%bin(j)
    end do
    read (unit) self%config%n_boxes
    do j = 1, self%config%n_dim
       read (unit) jbuffer, self%box(j)
    end do
    do j = 1, self%config%n_dim
       do i = 1, self%config%n_bins_max
          read (unit) ibuffer, jbuffer, self%d(i, j)
       end do
    end do
    do j = 1, self%config%n_dim
       do i = 1, self%config%n_bins_max + 1
          read (unit) ibuffer, jbuffer, self%grid%xi(i, j)
       end do
    end do
  end subroutine vegas_read_binary_grid


end module vegas
