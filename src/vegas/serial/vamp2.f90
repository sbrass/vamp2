module vamp2

  use kinds, only: default
  use io_units
  use format_utils, only: pac_fmt
  use format_utils, only: write_separator, write_indent
  use format_defs, only: FMT_17
  use diagnostics
  use iterator
  use rng_base
  use rng_stream, only: rng_stream_t

  use vegas



  implicit none
  private

  public :: vamp2_func_t
  public :: vamp2_config_t
  public :: vamp2_result_t
  public :: vamp2_equivalences_t
  public :: vamp2_t

  integer, parameter, public :: &
       VEQ_IDENTITY = 0, VEQ_INVERT = 1, VEQ_SYMMETRIC = 2, VEQ_INVARIANT = 3
  character(len=*), parameter, private :: &
       descr_fmt =         "(1X,A)", &
       integer_fmt =       "(1X,A18,1X,I15)", &
       integer_array_fmt = "(1X,I18,1X,I15)", &
       logical_fmt =       "(1X,A18,1X,L1)", &
       double_fmt =        "(1X,A18,1X,E24.16E4)", &
       double_array_fmt =  "(1X,I18,1X,E24.16E4)", &
       double_array_pac_fmt = "(1X,I18,1X,E16.8E4)", &
       double_array2_fmt = "(1X,2(1X,I8),1X,E24.16E4)", &
       double_array2_pac_fmt = "(1X,2(1X,I8),1X,E16.8E4)"

  type, abstract, extends(vegas_func_t) :: vamp2_func_t
     integer :: current_channel = 0
     integer :: n_dim = 0
     integer :: n_channel = 0
     integer :: n_calls = 0
     logical :: valid_x = .false.
     real(default), dimension(:, :), allocatable :: xi
     real(default), dimension(:), allocatable :: det
     real(default), dimension(:), allocatable :: wi
     real(default), dimension(:), allocatable :: gi
     type(vegas_grid_t), dimension(:), allocatable :: grids
     real(default) :: g = 0.
   contains
     procedure, public :: init => vamp2_func_init
     procedure, public :: set_channel => vamp2_func_set_channel
     procedure, public :: get_n_calls => vamp2_func_get_n_calls
     procedure, public :: reset_n_calls => vamp2_func_reset_n_calls
     procedure(vamp2_func_evaluate_maps), deferred :: evaluate_maps
     procedure, private :: evaluate_weight => vamp2_func_evaluate_weight
     procedure(vamp2_func_evaluate_func), deferred :: evaluate_func
     procedure, public :: evaluate => vamp2_func_evaluate
  end type vamp2_func_t

  type, extends(vegas_config_t) :: vamp2_config_t
     integer :: n_channel = 0
     integer :: n_calls_min_per_channel = 20
     integer :: n_calls_threshold = 10
     integer :: n_chains = 0
     logical :: stratified = .true.
     logical :: equivalences = .false.
     real(default) :: beta = 0.5_default
     real(default) :: accuracy_goal = 0._default
     real(default) :: error_goal = 0._default
     real(default) :: rel_error_goal = 0._default
   contains
     procedure, public :: write => vamp2_config_write
  end type vamp2_config_t

  type, extends(vegas_result_t) :: vamp2_result_t
   contains
     procedure, public :: write => vamp2_result_write
  end type vamp2_result_t

  type :: vamp2_equi_t
     integer :: ch
     integer :: ch_src
     integer, dimension(:), allocatable :: perm
     integer, dimension(:), allocatable :: mode
   contains
     procedure :: write => vamp2_equi_write
  end type vamp2_equi_t

  type :: vamp2_equivalences_t
     private
     integer :: n_eqv = 0
     integer :: n_channel = 0
     integer :: n_dim = 0
     type(vamp2_equi_t), dimension(:), allocatable :: eqv
     integer, dimension(:), allocatable :: map
     integer, dimension(:), allocatable :: multiplicity
     integer, dimension(:), allocatable :: symmetry
     logical, dimension(:), allocatable :: independent
     integer, dimension(:), allocatable :: equivalent_to_ch
     logical, dimension(:, :), allocatable :: dim_is_invariant
   contains
     procedure :: write => vamp2_equivalences_write
     procedure, public :: is_allocated => vamp2_equivalences_is_allocated
     procedure, public :: get_channels => vamp2_equivalences_get_channels
     procedure, public :: get_mode => vamp2_equivalences_get_mode
     procedure, public :: get_perm => vamp2_equivalences_get_perm
     procedure, public :: set_equivalence => vamp2_equivalences_set_equivalence
     procedure, public :: freeze => vamp2_equivalences_freeze
  end type vamp2_equivalences_t

  type :: vamp2_t
     private
     type(vamp2_config_t) :: config
     type(vegas_t), dimension(:), allocatable :: integrator
     integer, dimension(:), allocatable :: chain
     real(default), dimension(:), allocatable :: weight
     real(default), dimension(:), allocatable :: integral
     real(default), dimension(:), allocatable :: variance
     real(default), dimension(:), allocatable :: efficiency
     type(vamp2_result_t) :: result
     type(vamp2_equivalences_t) :: equivalences
     logical :: event_prepared
     real(default), dimension(:), allocatable :: event_weight
   contains
     procedure, public :: final => vamp2_final
     procedure, public :: write => vamp2_write
     procedure, public :: get_config => vamp2_get_config
     procedure, public :: set_config => vamp2_set_config
     procedure, public :: set_calls => vamp2_set_n_calls
     procedure, public :: set_limits => vamp2_set_limits
     procedure, public :: set_chain => vamp2_set_chain
     procedure, public :: set_equivalences => vamp2_set_equivalences
     procedure, public :: get_n_calls => vamp2_get_n_calls
     procedure, public :: get_integral => vamp2_get_integral
     procedure, public :: get_variance => vamp2_get_variance
     procedure, public :: get_efficiency => vamp2_get_efficiency
     procedure :: get_evt_weight => vamp2_get_evt_weight
     procedure :: get_evt_weight_excess => vamp2_get_evt_weight_excess
     procedure :: get_grid => vamp2_get_grid
     procedure, private :: adapt_weights => vamp2_adapt_weights
     procedure, private :: apply_equivalences => vamp2_apply_equivalences
     procedure, public :: reset_result => vamp2_reset_result
     procedure, public :: integrate => vamp2_integrate
     procedure, private :: prepare_integrate_iteration => vamp2_prepare_integrate_iteration
     procedure, private :: compute_result_and_efficiency => vamp2_compute_result_and_efficiency
     procedure, public :: generate_weighted => vamp2_generate_weighted_event
     procedure, public :: generate_unweighted => vamp2_generate_unweighted_event
     procedure, public :: write_grids => vamp2_write_grids
     procedure, public :: read_grids => vamp2_read_grids
     procedure :: write_binary_grids => vamp2_write_binary_grids
     procedure :: read_binary_grids => vamp2_read_binary_grids
  end type vamp2_t

  abstract interface
     subroutine vamp2_func_evaluate_maps (self, x)
       import :: vamp2_func_t, default
       class(vamp2_func_t), intent(inout) :: self
       real(default), dimension(:), intent(in) :: x
     end subroutine vamp2_func_evaluate_maps
  end interface

  abstract interface
     real(default) function vamp2_func_evaluate_func (self, x) result (f)
       import :: vamp2_func_t, default
       class(vamp2_func_t), intent(in) :: self
       real(default), dimension(:), intent(in) :: x
     end function vamp2_func_evaluate_func
  end interface

  interface vamp2_equivalences_t
     module procedure vamp2_equivalences_init
  end interface vamp2_equivalences_t

  interface vamp2_t
     module procedure vamp2_init
  end interface vamp2_t

contains

  subroutine vamp2_func_init (self, n_dim, n_channel)
    class(vamp2_func_t), intent(out) :: self
    integer, intent(in) :: n_dim
    integer, intent(in) :: n_channel
    self%n_dim = n_dim
    self%n_channel = n_channel
    allocate (self%xi(n_dim, n_channel), source=0._default)
    allocate (self%det(n_channel), source=1._default)
    allocate (self%wi(n_channel), source=0._default)
    allocate (self%gi(n_channel), source=0._default)
    allocate (self%grids(n_channel))
  end subroutine vamp2_func_init

  subroutine vamp2_func_set_channel (self, channel)
    class(vamp2_func_t), intent(inout) :: self
    integer, intent(in) :: channel
    self%current_channel = channel
  end subroutine vamp2_func_set_channel

  integer function vamp2_func_get_n_calls (self) result (n_calls)
    class(vamp2_func_t), intent(in) :: self
    n_calls = self%n_calls
  end function vamp2_func_get_n_calls

  subroutine vamp2_func_reset_n_calls (self)
    class(vamp2_func_t), intent(inout) :: self
    self%n_calls = 0
  end subroutine vamp2_func_reset_n_calls

  subroutine vamp2_func_evaluate_weight (self)
    class(vamp2_func_t), intent(inout) :: self
    integer :: ch
    self%g = 0.
    self%gi = 0.
    !$OMP PARALLEL DO PRIVATE(ch) SHARED(self)
    do ch = 1, self%n_channel
       if (self%wi(ch) /= 0) then
          self%gi(ch) = self%grids(ch)%get_probability (self%xi(:, ch))
       end if
    end do
    !$OMP END PARALLEL DO
    if (self%gi(self%current_channel) /= 0) then
       do ch = 1, self%n_channel
          if (self%wi(ch) /= 0 .and. self%det(ch) /= 0) then
             self%g = self%g + self%wi(ch) * self%gi(ch) / self%det(ch)
          end if
       end do
       self%g = self%g / self%gi(self%current_channel)
    end if
  end subroutine vamp2_func_evaluate_weight

  real(default) function vamp2_func_evaluate (self, x) result (f)
    class(vamp2_func_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x
    call self%evaluate_maps (x)
    f = 0.
    self%gi = 0.
    self%g = 1
    if (self%valid_x) then
       call self%evaluate_weight ()
       if (self%g /= 0) then
          f = self%evaluate_func (x) / self%g
          self%n_calls = self%n_calls + 1
       end if
    end if
  end function vamp2_func_evaluate

  subroutine vamp2_config_write (self, unit, indent)
    class(vamp2_config_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call self%vegas_config_t%write (unit, indent)
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of channels                               = ", self%n_channel
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Min. number of calls per channel (setting calls) = ", &
         & self%n_calls_min_per_channel
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Threshold number of calls (adapting weights)     = ", &
         & self%n_calls_threshold
    call write_indent (u, ind)
    write (u, "(2x,A,I0)") &
         & "Number of chains                                 = ", self%n_chains
    call write_indent (u, ind)
    write (u, "(2x,A,L1)") &
         & "Stratified                                       = ", self%stratified
    call write_indent (u, ind)
    write (u, "(2x,A,L1)") &
         & "Equivalences                                     = ", self%equivalences
    call write_indent (u, ind)
    write (u, "(2x,A," // FMT_17 // ")") &
         & "Adaption power (beta)                            = ", self%beta
    if (self%accuracy_goal > 0) then
       call write_indent (u, ind)
       write (u, "(2x,A," // FMT_17 // ")") &
            & "accuracy_goal                                 = ", self%accuracy_goal
    end if
    if (self%error_goal > 0) then
       call write_indent (u, ind)
       write (u, "(2x,A," // FMT_17 // ")") &
            & "error_goal                                    = ", self%error_goal
    end if
    if (self%rel_error_goal > 0) then
       call write_indent (u, ind)
       write (u, "(2x,A," // FMT_17 // ")") &
            & "rel_error_goal                                = ", self%rel_error_goal
    end if
  end subroutine vamp2_config_write

  subroutine vamp2_result_write (self, unit, indent)
    class(vamp2_result_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call self%vegas_result_t%write (unit, indent)
  end subroutine vamp2_result_write

  subroutine vamp2_equi_write (self, unit, indent)
    class(vamp2_equi_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call write_indent (u, ind)
    write (u, "(2(A,1X,I0))") "src:", self%ch_src, "-> dest:", self%ch
    call write_indent (u, ind)
    write (u, "(A,99(1X,I0))") "Perm: ", self%perm
    call write_indent (u, ind)
    write (u, "(A,99(1X,I0))") "Mode: ", self%mode
  end subroutine vamp2_equi_write

  type(vamp2_equivalences_t) function vamp2_equivalences_init (&
       n_eqv, n_channel, n_dim) result (eqv)
    integer, intent(in) :: n_eqv, n_channel, n_dim
    eqv%n_eqv = n_eqv
    eqv%n_channel = n_channel
    eqv%n_dim = n_dim
    allocate (eqv%eqv(n_eqv))
    allocate (eqv%map(n_channel), source = 0)
    allocate (eqv%multiplicity(n_channel), source = 0)
    allocate (eqv%symmetry(n_channel), source = 0)
    allocate (eqv%independent(n_channel), source = .true.)
    allocate (eqv%equivalent_to_ch(n_channel), source = 0)
    allocate (eqv%dim_is_invariant(n_dim, n_channel), source = .false.)
  end function vamp2_equivalences_init

  subroutine vamp2_equivalences_write (self, unit, indent)
    class(vamp2_equivalences_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind, i_eqv, ch
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    write (u, "(A)") "Inequivalent channels:"
    if (allocated (self%independent)) then
       do ch = 1, self%n_channel
          if (self%independent(ch)) then
             write (u, "(2X,A,1x,I0,A,4x,A,I0,4x,A,I0,4x,A,999(L1))") &
                  "Channel", ch, ":", &
                  "Mult. = ", self%multiplicity(ch), &
                  "Symm. = ", self%symmetry(ch), &
                  "Invar.: ", self%dim_is_invariant(:, ch)
          end if
       end do
    else
       write (u, "(A)") "[not allocated]"
    end if
    write (u, "(A)") "Equivalence list:"
    if (allocated (self%eqv)) then
       do i_eqv = 1, self%n_eqv
          write (u, "(2X,A,1X,I0)") "i_eqv:", i_eqv
          call self%eqv(i_eqv)%write (unit, indent = ind + 4)
       end do
    else
       write (u, "(A)") "[not allocated]"
    end if
  end subroutine vamp2_equivalences_write

  logical function vamp2_equivalences_is_allocated (self) result (yorn)
    class(vamp2_equivalences_t), intent(in) :: self
    yorn = allocated (self%eqv)
  end function vamp2_equivalences_is_allocated

  subroutine vamp2_equivalences_get_channels (eqv, i_eqv, dest, src)
    class(vamp2_equivalences_t), intent(in) :: eqv
    integer, intent(in) :: i_eqv
    integer, intent(out) :: dest, src
    dest = eqv%eqv(i_eqv)%ch
    src = eqv%eqv(i_eqv)%ch_src
  end subroutine vamp2_equivalences_get_channels

  function vamp2_equivalences_get_mode (eqv, i_eqv) result (mode)
    class(vamp2_equivalences_t), intent(in) :: eqv
    integer, intent(in) :: i_eqv
    integer, dimension(:), allocatable :: mode
    mode = eqv%eqv(i_eqv)%mode
  end function vamp2_equivalences_get_mode

  function vamp2_equivalences_get_perm (eqv, i_eqv) result (perm)
    class(vamp2_equivalences_t), intent(in) :: eqv
    integer, intent(in) :: i_eqv
    integer, dimension(:), allocatable :: perm
    perm = eqv%eqv(i_eqv)%perm
  end function vamp2_equivalences_get_perm

  subroutine vamp2_equivalences_set_equivalence &
       (eqv, i_eqv, dest, src, perm, mode)
    class(vamp2_equivalences_t), intent(inout) :: eqv
    integer, intent(in) :: i_eqv
    integer, intent(in) :: dest, src
    integer, dimension(:), intent(in) :: perm, mode
    integer :: i
    if (dest < 1 .or. dest > eqv%n_channel)  call msg_bug &
         ("VAMP2: set_equivalences: destination channel out of range.")
    if (src < 1 .or. src > eqv%n_channel)  call msg_bug &
         ("VAMP2: set_equivalences: source channel out of range.")
    if (size(perm) /= eqv%n_dim)  call msg_bug &
         ("VAMP2: set_equivalences: size(perm) does not match n_dim.")
    if (size(mode) /= eqv%n_dim)  call msg_bug &
         ("VAMP2: set_equivalences: size(mode) does not match n_dim.")
    eqv%eqv(i_eqv)%ch = dest
    eqv%eqv(i_eqv)%ch_src = src
    allocate (eqv%eqv(i_eqv)%perm (size (perm)))
    do i = 1, size (perm)
       eqv%eqv(i_eqv)%perm(i) = perm(i)
    end do
    allocate (eqv%eqv(i_eqv)%mode (size (mode)))
    do i = 1, size (mode)
       eqv%eqv(i_eqv)%mode(i) = mode(i)
    end do
  end subroutine vamp2_equivalences_set_equivalence

  subroutine vamp2_equivalences_freeze (self)
    class(vamp2_equivalences_t), intent(inout) :: self
    integer :: i_eqv, ch, upper, lower
    ch = 0
    do i_eqv = 1, self%n_eqv
       if (ch /= self%eqv(i_eqv)%ch) then
          ch = self%eqv(i_eqv)%ch
          self%map(ch) = i_eqv
       end if
    end do
    do ch = 1, self%n_channel
       lower = self%map(ch)
       if (ch == self%n_channel) then
          upper = self%n_eqv
       else
          upper = self%map(ch + 1) - 1
       end if
       associate (eqv => self%eqv, n_eqv => size (self%eqv(lower:upper)))
         if (.not. all(eqv(lower:upper)%ch == ch) .or. &
              eqv(lower)%ch_src > ch) then
            do i_eqv = lower, upper
               call self%eqv(i_eqv)%write ()
            end do
            call msg_bug ("VAMP2: vamp2_equivalences_freeze: &
                 &equivalence order is not correct.")
         end if
         self%symmetry(ch) = count (eqv(lower:upper)%ch_src == ch)
         if (mod (n_eqv, self%symmetry(ch)) /= 0) then
            do i_eqv = lower, upper
               call self%eqv(i_eqv)%write ()
            end do
            call msg_bug ("VAMP2: vamp2_equivalences_freeze: &
                 &permutation count is not correct.")
         end if
         self%multiplicity(ch) = n_eqv / self%symmetry(ch)
         self%independent(ch) = all (eqv(lower:upper)%ch_src >= ch)
         self%equivalent_to_ch(ch) = eqv(lower)%ch_src
         self%dim_is_invariant(:, ch) = eqv(lower)%mode == VEQ_INVARIANT
       end associate
    end do
  end subroutine vamp2_equivalences_freeze

  type(vamp2_t) function vamp2_init (n_channel, n_dim, alpha, beta, n_bins_max,&
       & n_calls_min_per_channel, iterations, mode) result (self)
    integer, intent(in) :: n_channel
    integer, intent(in) :: n_dim
    integer, intent(in), optional :: n_bins_max
    integer, intent(in), optional :: n_calls_min_per_channel
    real(default), intent(in), optional :: alpha
    real(default), intent(in), optional :: beta
    integer, intent(in), optional :: iterations
    integer, intent(in), optional :: mode
    integer :: ch
    self%config%n_dim = n_dim
    self%config%n_channel = n_channel
    call set_options ()
    allocate (self%chain(n_channel), source=0)
    allocate (self%integrator(n_channel))
    allocate (self%weight(n_channel), source=0._default)
    do ch = 1, n_channel
       self%integrator(ch) = vegas_t (n_dim, alpha, n_bins_max, 1, mode)
    end do
    self%weight = 1._default / self%config%n_channel
    call self%reset_result ()
    allocate (self%event_weight(self%config%n_channel), source = 0._default)
    self%event_prepared = .false.
  contains
    subroutine set_options ()
      if (present (n_bins_max)) self%config%n_bins_max = n_bins_max
      if (present (n_calls_min_per_channel)) self%config%n_calls_min_per_channel = n_calls_min_per_channel
      if (present (alpha)) self%config%alpha = alpha
      if (present (beta)) self%config%beta = beta
      if (present (iterations)) self%config%iterations = iterations
      if (present (mode)) self%config%mode = mode
    end subroutine set_options
  end function vamp2_init

  subroutine vamp2_final (self)
    class(vamp2_t), intent(inout) :: self
    integer :: ch
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%final ()
    end do
  end subroutine vamp2_final

  subroutine vamp2_write (self, unit, indent)
    class(vamp2_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: u, ind, ch
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call write_indent (u, ind)
    write (u, "(A)") "VAMP2: VEGAS AMPlified 2"
    call write_indent (u, ind)
    call self%config%write (unit, indent)
    call self%result%write (unit, indent)
  end subroutine vamp2_write

  subroutine vamp2_get_config (self, config)
    class(vamp2_t), intent(in) :: self
    type(vamp2_config_t), intent(out) :: config
    config = self%config
  end subroutine vamp2_get_config

  subroutine vamp2_set_config (self, config)
    class(vamp2_t), intent(inout) :: self
    class(vamp2_config_t), intent(in) :: config
    integer :: ch
    self%config%equivalences = config%equivalences
    self%config%n_calls_min_per_channel = config%n_calls_min_per_channel
    self%config%n_calls_threshold = config%n_calls_threshold
    self%config%n_calls_min = config%n_calls_min
    self%config%beta = config%beta
    self%config%accuracy_goal = config%accuracy_goal
    self%config%error_goal = config%error_goal
    self%config%rel_error_goal = config%rel_error_goal
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%set_config (config)
    end do
  end subroutine vamp2_set_config

  subroutine vamp2_set_n_calls (self, n_calls)
    class(vamp2_t), intent(inout) :: self
    integer, intent(in) :: n_calls
    integer :: ch
    self%config%n_calls_min = self%config%n_calls_min_per_channel &
         & * self%config%n_channel
    self%config%n_calls = max(n_calls, self%config%n_calls_min)
    if (self%config%n_calls > n_calls) then
       write (msg_buffer, "(A,I0)") "VAMP2: [set_calls] number of calls too few,&
            & reset to = ", self%config%n_calls
       call msg_message ()
    end if
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%set_calls (max (nint (self%config%n_calls *&
            & self%weight(ch)), self%config%n_calls_min_per_channel))
    end do
  end subroutine vamp2_set_n_calls

  subroutine vamp2_set_limits (self, x_upper, x_lower)
    class(vamp2_t), intent(inout) :: self
    real(default), dimension(:), intent(in) :: x_upper
    real(default), dimension(:), intent(in) :: x_lower
    integer :: ch
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%set_limits (x_upper, x_lower)
    end do
  end subroutine vamp2_set_limits

  subroutine vamp2_set_chain (self, n_chains, chain)
    class(vamp2_t), intent(inout) :: self
    integer, intent(in) :: n_chains
    integer, dimension(:), intent(in) :: chain
    if (size (chain) /= self%config%n_channel) then
       call msg_bug ("VAMP2: set chain: size of chain array does not match n_channel.")
    else
       call msg_message ("VAMP2: set chain: use chained weights.")
    end if
    self%config%n_chains = n_chains
    self%chain = chain
  end subroutine vamp2_set_chain

  subroutine vamp2_set_equivalences (self, equivalences)
    class(vamp2_t), intent(inout) :: self
    type(vamp2_equivalences_t), intent(in) :: equivalences
    self%equivalences = equivalences
  end subroutine vamp2_set_equivalences

  elemental real(default) function vamp2_get_n_calls (self) result (n_calls)
    class(vamp2_t), intent(in) :: self
    n_calls = sum (self%integrator%get_calls ())
  end function vamp2_get_n_calls

  elemental real(default) function vamp2_get_integral (self) result (integral)
    class(vamp2_t), intent(in) :: self
    integral = 0.
    if (self%result%sum_wgts > 0.) then
       integral = self%result%sum_int_wgtd / self%result%sum_wgts
    end if
  end function vamp2_get_integral

  elemental real(default) function vamp2_get_variance (self) result (variance)
    class(vamp2_t), intent(in) :: self
    variance = 0.
    if (self%result%sum_wgts > 0.) then
       variance = 1.0 / self%result%sum_wgts
    end if
  end function vamp2_get_variance

  elemental real(default) function vamp2_get_efficiency (self) result (efficiency)
    class(vamp2_t), intent(in) :: self
    efficiency = 0.
    if (self%result%efficiency > 0.) then
       efficiency = self%result%efficiency
    end if
  end function vamp2_get_efficiency

  real(default) function vamp2_get_evt_weight (self) result (evt_weight)
    class(vamp2_t), intent(in) :: self
    evt_weight = self%result%evt_weight
  end function vamp2_get_evt_weight

  real(default) function vamp2_get_evt_weight_excess (self) result (evt_weight_excess)
    class(vamp2_t), intent(in) :: self
    evt_weight_excess = self%result%evt_weight_excess
  end function vamp2_get_evt_weight_excess

  type(vegas_grid_t) function vamp2_get_grid (self, channel) result (grid)
    class(vamp2_t), intent(in) :: self
    integer, intent(in) :: channel
    if (channel < 1 .or. channel > self%config%n_channel) &
         call msg_bug ("VAMP2: vamp2_get_grid: channel index < 1 or > n_channel.")
    grid = self%integrator(channel)%get_grid ()
  end function vamp2_get_grid

  subroutine vamp2_adapt_weights (self)
    class(vamp2_t), intent(inout) :: self
    integer :: n_weights_underflow
    real(default) :: weight_min, sum_weights_underflow
    self%weight = self%weight * self%integrator%get_variance ()**self%config%beta
    if (sum (self%weight) == 0) self%weight = real(self%config%n_calls, default)
    if (self%config%n_chains > 0) then
       call chain_weights ()
    end if
    self%weight = self%weight / sum(self%weight)
    if (self%config%n_calls_threshold /= 0) then
       weight_min = real(self%config%n_calls_threshold, default) &
            & / self%config%n_calls
       sum_weights_underflow = sum (self%weight, self%weight < weight_min)
       n_weights_underflow = count (self%weight < weight_min)
       where (self%weight < weight_min)
          self%weight = weight_min
       elsewhere
          self%weight = self%weight * (1. - n_weights_underflow * weight_min) &
               & / (1. - sum_weights_underflow)
       end where
    end if
    call self%set_calls (self%config%n_calls)
  contains
    subroutine chain_weights ()
      integer :: ch
      real(default) :: average
      do ch = 1, self%config%n_chains
         average = max (sum (self%weight, self%chain == ch), 0._default)
         if (average /= 0) then
            average = average / count (self%chain == ch)
            where (self%chain == ch)
               self%weight = average
            end where
         end if
      end do
    end subroutine chain_weights

  end subroutine vamp2_adapt_weights

  subroutine vamp2_apply_equivalences (self)
    class(vamp2_t), intent(inout) :: self
    integer :: ch, ch_src, j, j_src, i_eqv
    real(default), dimension(:, :, :), allocatable :: d
    real(default), dimension(:, :), allocatable :: d_src
    integer, dimension(:), allocatable :: mode, perm
    if (.not. self%equivalences%is_allocated ()) then
       call msg_bug ("VAMP2: vamp2_apply_equivalences: &
            &cannot apply not-allocated equivalences.")
    end if
    allocate (d(self%config%n_bins_max, self%config%n_dim, &
         self%config%n_channel), source=0._default)
    associate (eqv => self%equivalences, nb => self%config%n_bins_max)
      do i_eqv = 1, self%equivalences%n_eqv
         call eqv%get_channels (i_eqv, ch, ch_src)
         d_src = self%integrator(ch_src)%get_distribution ()
         mode = eqv%get_mode (i_eqv)
         perm = eqv%get_perm (i_eqv)
         do j = 1, self%config%n_dim
            select case (mode (j))
            case (VEQ_IDENTITY)
               d(:, j, ch) = d(:, j, ch) + &
                    d_src(:, perm(j))
            case (VEQ_INVERT)
               d(:, j, ch) = d(:, j, ch) + &
                    d_src(nb:1:-1, perm(j))
            case (VEQ_SYMMETRIC)
               d(:, j, ch) = d(:, j, ch) + &
                    d_src(:, perm(j)) / 2. + &
                    d_src(nb:1:-1, perm(j)) / 2.
            case (VEQ_INVARIANT)
               d(:, j, ch) = 1._default
            end select
         end do
      end do
    end associate
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%set_distribution (d(:, :, ch))
    end do
  end subroutine vamp2_apply_equivalences

  subroutine vamp2_reset_result (self)
    class(vamp2_t), intent(inout) :: self
    call self%result%reset ()
  end subroutine vamp2_reset_result

  subroutine vamp2_integrate (self, func, rng, iterations, reset_result,&
       & refine_grids, adapt_weights, verbose, result, abserr)
    class(vamp2_t), intent(inout) :: self
    class(vamp2_func_t), intent(inout) :: func
    class(rng_t), intent(inout) :: rng
    integer, intent(in), optional :: iterations
    logical, intent(in), optional :: reset_result
    logical, intent(in), optional :: refine_grids
    logical, intent(in), optional :: adapt_weights
    logical, intent(in), optional :: verbose
    real(default), optional, intent(out) :: result, abserr
    integer :: it, ch
    type(iterator_t) :: channel_iterator
    real(default) :: cumulative_int, cumulative_std
    logical :: opt_reset_result
    logical :: opt_adapt_weights
    logical :: opt_refine_grids
    logical :: opt_verbose
    call set_options ()
    if (opt_verbose) then
       call msg_message ("Results: [it, calls, integral, error, chi^2, eff.]")
    end if
    if (opt_reset_result) call self%reset_result ()
    iteration: do it = 1, self%config%iterations
       call channel_iterator%init (1, self%config%n_channel)
       call self%prepare_integrate_iteration (func)
       channel: do while (channel_iterator%is_iterable ())
          ch = channel_iterator%get_current ()
          call func%set_channel (ch)
          call self%integrator(ch)%integrate ( &
               & func, rng, iterations, refine_grid = .false., verbose = .false.)
          call channel_iterator%next_step ()
       end do channel
       call self%compute_result_and_efficiency ()
       associate (result => self%result)
         cumulative_int = result%sum_int_wgtd / result%sum_wgts
         cumulative_std = sqrt (1 / result%sum_wgts)
         if (opt_verbose) then
            write (msg_buffer, "(I0,1x,I0,1x, 4(E24.16E4,1x))") &
                 & it, self%config%n_calls, cumulative_int, cumulative_std, &
                 & result%chi2, result%efficiency
            call msg_message ()
         end if
       end associate
       if (opt_adapt_weights) then
          call self%adapt_weights ()
       end if
       if (opt_refine_grids) then
          if (self%config%equivalences .and. self%equivalences%is_allocated ()) then
             call self%apply_equivalences ()
          end if
          do ch = 1, self%config%n_channel
             call self%integrator(ch)%refine ()
          end do
       end if
    end do iteration
    if (present (result)) result = cumulative_int
    if (present (abserr)) abserr = abs (cumulative_std)
  contains
    subroutine set_options ()
      if (present (iterations)) self%config%iterations = iterations
      opt_reset_result = .true.
      if (present (reset_result)) opt_reset_result = reset_result
      opt_adapt_weights = .true.
      if (present (adapt_weights)) opt_adapt_weights = adapt_weights
      opt_refine_grids = .true.
      if (present (refine_grids)) opt_refine_grids = refine_grids
      opt_verbose = .false.
      if (present (verbose)) opt_verbose = verbose
    end subroutine set_options

  end subroutine vamp2_integrate

  !> Prepare iteration, i.e. provide weights and grids to function object.
  subroutine vamp2_prepare_integrate_iteration (self, func)
    class(vamp2_t), intent(inout) :: self
    class(vamp2_func_t), intent(inout) :: func
    call fill_func_with_weights_and_grids (func)
  contains
    subroutine fill_func_with_weights_and_grids (func)
      class(vamp2_func_t), intent(inout) :: func
      integer :: ch
      do ch = 1, self%config%n_channel
         func%wi(ch) = self%weight(ch)
         !! \todo Use pointers instead of a deep copy.
         func%grids(ch) = self%integrator(ch)%get_grid ()
      end do
    end subroutine fill_func_with_weights_and_grids

  end subroutine vamp2_prepare_integrate_iteration

  !> Compute the result and efficiency of the current status of the integrator.
  subroutine vamp2_compute_result_and_efficiency (self)
    class(vamp2_t), intent(inout) :: self
    real(default) :: total_integral, total_variance
    real(default) :: max_abs_f_pos, max_abs_f_neg, &
         sum_abs_f_pos, sum_abs_f_neg
    call compute_integral_and_variance (total_integral, total_variance)
    call self%result%update (total_integral, total_variance)
    call compute_efficiency (max_pos = max_abs_f_pos, max_neg = max_abs_f_neg, &
         sum_pos = sum_abs_f_pos, sum_neg = sum_abs_f_neg)
    !! Do not average of number of calls, we have already averaged the efficiencies of all channels.
    call self%result%update_efficiency (n_calls  = 1, &
         max_pos = max_abs_f_pos, max_neg = max_abs_f_neg, &
         sum_pos = sum_abs_f_pos, sum_neg = sum_abs_f_neg)
  contains
    subroutine compute_integral_and_variance (integral, variance)
      real(default), intent(out) :: integral, variance
      real(default) :: sq_integral
      integral = dot_product (self%weight, self%integrator%get_integral ())
      sq_integral = dot_product (self%weight, self%integrator%get_integral ()**2)
      variance = self%config%n_calls * dot_product (self%weight**2, self%integrator%get_variance ())
      variance = sqrt (variance + sq_integral)
      variance = 1._default / self%config%n_calls * &
           & (total_variance + total_integral) * (total_variance - total_integral)
    end subroutine compute_integral_and_variance

    !> We compute the weight-averaged sum and maximum of the channel (integration) weights \f$w_{i,c}\f$.
    !!
    !! The averaged integration weight and maximum are
    !! \f[
    !!  \langle w \rangle = \sum_i \alpha_i \frac{\sum_j w_{i, j}}{N_i},
    !! \f]
    !! \f[
    !!  \langle \max w \rangle = \sum_i \alpha_i |\max_j w_{i, j}|.
    !! \f]
    subroutine compute_efficiency (max_pos, max_neg, &
         sum_pos, sum_neg)
      real(default), intent(out) :: max_pos, max_neg
      real(default), intent(out) :: sum_pos, sum_neg
      max_abs_f_pos = dot_product (self%weight, self%integrator%get_max_abs_f_pos ())
      max_abs_f_neg = dot_product (self%weight, self%integrator%get_max_abs_f_neg ())
      sum_abs_f_pos = dot_product (self%weight, &
           self%integrator%get_sum_abs_f_pos () / self%integrator%get_calls ())
      sum_abs_f_neg = dot_product (self%weight, &
           self%integrator%get_sum_abs_f_neg () / self%integrator%get_calls ())
    end subroutine compute_efficiency
  end subroutine vamp2_compute_result_and_efficiency

  subroutine vamp2_generate_weighted_event (&
       self, func, rng, x)
    class(vamp2_t), intent(inout) :: self
    class(vamp2_func_t), intent(inout) :: func
    class(rng_t), intent(inout) :: rng
    real(default), dimension(self%config%n_dim), intent(out)  :: x
    integer :: ch, i
    real(default) :: r
    if (.not. self%event_prepared) then
       call prepare_event ()
    end if
    call rng%generate (r)
    nchannel: do ch = 1, self%config%n_channel
       r = r - self%event_weight(ch)
       if (r <= 0._default) exit nchannel
    end do nchannel
    ch = min (ch, self%config%n_channel)
    call func%set_channel (ch)
    call self%integrator(ch)%generate_weighted (func, rng, x)
    ! Norm weight by f_max, hidden in event_weight(ch), else by 1
    self%result%evt_weight = self%integrator(ch)%get_evt_weight () &
         * self%weight(ch) / self%event_weight(ch)
  contains
    subroutine prepare_event ()
      integer :: i
      self%event_prepared = .false.
      do i = 1, self%config%n_channel
         func%wi(i) = self%weight(i)
         func%grids(i) = self%integrator(i)%get_grid ()
      end do
      if (any (self%integrator%get_max_abs_f () > 0)) then
         self%event_weight = self%weight * self%integrator%get_max_abs_f ()
      else
         self%event_weight = self%weight
      end if
      self%event_weight = self%event_weight / sum (self%event_weight)
      self%event_prepared = .true.
    end subroutine prepare_event

  end subroutine vamp2_generate_weighted_event

  subroutine vamp2_generate_unweighted_event ( &
       & self, func, rng, x, opt_event_rescale)
    class(vamp2_t), intent(inout) :: self
    class(vamp2_func_t), intent(inout) :: func
    class(rng_t), intent(inout) :: rng
    real(default), dimension(self%config%n_dim), intent(out)  :: x
    real(default), intent(in), optional :: opt_event_rescale
    integer :: ch, i
    real(default) :: r, max_abs_f, event_rescale
    event_rescale = 1._default
    if (present (opt_event_rescale)) then
       event_rescale = opt_event_rescale
    end if
    if (.not. self%event_prepared) then
       call prepare_event ()
    end if
    generate: do
       call rng%generate (r)
       nchannel: do ch = 1, self%config%n_channel
          r = r - self%event_weight(ch)
          if (r <= 0._default) exit nchannel
       end do nchannel
       ch = min (ch, self%config%n_channel)
       call func%set_channel (ch)
       call self%integrator(ch)%generate_weighted (func, rng, x)
       self%result%evt_weight = self%integrator(ch)%get_evt_weight ()
       max_abs_f = merge ( &
            self%integrator(ch)%get_max_abs_f_pos (), &
            self%integrator(ch)%get_max_abs_f_neg (), &
            self%result%evt_weight > 0.)
       self%result%evt_weight_excess = 0._default
       if (self%result%evt_weight > max_abs_f) then
          self%result%evt_weight_excess = self%result%evt_weight / max_abs_f - 1._default
          exit generate
       end if
       call rng%generate (r)
       ! Do not use division, because max_abs_f could be zero.
       if (event_rescale * max_abs_f * r <= abs(self%result%evt_weight)) then
          exit generate
       end if
    end do generate
  contains
    subroutine prepare_event ()
      integer :: i
      self%event_prepared = .false.
      do i = 1, self%config%n_channel
         func%wi(i) = self%weight(i)
         func%grids(i) = self%integrator(i)%get_grid ()
      end do
      if (any (self%integrator%get_max_abs_f () > 0)) then
         self%event_weight = self%weight * self%integrator%get_max_abs_f ()
      else
         self%event_weight = self%weight
      end if
      self%event_weight = self%event_weight / sum (self%event_weight)
      self%event_prepared = .true.
    end subroutine prepare_event

  end subroutine vamp2_generate_unweighted_event

  subroutine vamp2_write_grids (self, unit)
    class(vamp2_t), intent(in) :: self
    integer, intent(in), optional :: unit
    integer :: u
    integer :: ch
    u = given_output_unit (unit)
    write (u, descr_fmt) "begin type(vamp2_t)"
    write (u, integer_fmt) "n_channel =", self%config%n_channel
    write (u, integer_fmt) "n_dim =", self%config%n_dim
    write (u, integer_fmt) "n_calls_min_ch =", self%config%n_calls_min_per_channel
    write (u, integer_fmt) "n_calls_thres =", self%config%n_calls_threshold
    write (u, integer_fmt) "n_chains =", self%config%n_chains
    write (u, logical_fmt) "stratified =", self%config%stratified
    write (u, double_fmt) "alpha =", self%config%alpha
    write (u, double_fmt) "beta =", self%config%beta
    write (u, integer_fmt) "n_bins_max =", self%config%n_bins_max
    write (u, integer_fmt) "iterations =", self%config%iterations
    write (u, integer_fmt) "n_calls =", self%config%n_calls
    write (u, integer_fmt) "it_start =", self%result%it_start
    write (u, integer_fmt) "it_num =", self%result%it_num
    write (u, integer_fmt) "samples =", self%result%samples
    write (u, double_fmt) "sum_int_wgtd =", self%result%sum_int_wgtd
    write (u, double_fmt) "sum_wgts =", self%result%sum_wgts
    write (u, double_fmt) "sum_chi =", self%result%sum_chi
    write (u, double_fmt) "chi2 =", self%result%chi2
    write (u, double_fmt) "efficiency =", self%result%efficiency
    write (u, double_fmt) "efficiency_pos =", self%result%efficiency_pos
    write (u, double_fmt) "efficiency_neg =", self%result%efficiency_neg
    write (u, double_fmt) "max_abs_f =", self%result%max_abs_f
    write (u, double_fmt) "max_abs_f_pos =", self%result%max_abs_f_pos
    write (u, double_fmt) "max_abs_f_neg =", self%result%max_abs_f_neg
    write (u, double_fmt) "result =", self%result%result
    write (u, double_fmt) "std =", self%result%std
    write (u, descr_fmt) "begin weight"
    do ch = 1, self%config%n_channel
       write (u, double_array_fmt) ch, self%weight(ch)
    end do
    write (u, descr_fmt) "end weight"
    if (self%config%n_chains > 0) then
       write (u, descr_fmt) "begin chain"
       do ch = 1, self%config%n_channel
          write (u, integer_array_fmt) ch, self%chain(ch)
       end do
       write (u, descr_fmt) "end chain"
    end if
    write (u, descr_fmt) "begin integrator"
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%write_grid (unit)
    end do
    write (u, descr_fmt) "end integrator"
    write (u, descr_fmt) "end type(vamp2_t)"
  end subroutine vamp2_write_grids

  subroutine vamp2_read_grids (self, unit)
    class(vamp2_t), intent(out) :: self
    integer, intent(in), optional :: unit
    integer :: u
    integer :: ibuffer, jbuffer, ch
    character(len=80) :: buffer
    read (unit, descr_fmt) buffer
    read (unit, integer_fmt) buffer, ibuffer
    read (unit, integer_fmt) buffer, jbuffer
    select type (self)
    type is (vamp2_t)
       self = vamp2_t (n_channel = ibuffer, n_dim = jbuffer)
    end select
    read (unit, integer_fmt) buffer, self%config%n_calls_min_per_channel
    read (unit, integer_fmt) buffer, self%config%n_calls_threshold
    read (unit, integer_fmt) buffer, self%config%n_chains
    read (unit, logical_fmt) buffer, self%config%stratified
    read (unit, double_fmt) buffer, self%config%alpha
    read (unit, double_fmt) buffer, self%config%beta
    read (unit, integer_fmt) buffer, self%config%n_bins_max
    read (unit, integer_fmt) buffer, self%config%iterations
    read (unit, integer_fmt) buffer, self%config%n_calls
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
    read (unit, descr_fmt) buffer
    do ch = 1, self%config%n_channel
       read (unit, double_array_fmt) ibuffer, self%weight(ch)
    end do
    read (unit, descr_fmt) buffer
    if (self%config%n_chains > 0) then
       read (unit, descr_fmt) buffer
       do ch = 1, self%config%n_channel
          read (unit, integer_array_fmt) ibuffer, self%chain(ch)
       end do
       read (unit, descr_fmt) buffer
    end if
    read (unit, descr_fmt) buffer
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%read_grid (unit)
    end do
    read (unit, descr_fmt) buffer
    read (unit, descr_fmt) buffer
  end subroutine vamp2_read_grids

  subroutine vamp2_write_binary_grids (self, unit)
    class(vamp2_t), intent(in) :: self
    integer, intent(in) :: unit
    integer :: ch
    write (unit)
    write (unit) self%config%n_channel
    write (unit) self%config%n_dim
    write (unit) self%config%n_calls_min_per_channel
    write (unit) self%config%n_calls_threshold
    write (unit) self%config%n_chains
    write (unit) self%config%stratified
    write (unit) self%config%alpha
    write (unit) self%config%beta
    write (unit) self%config%n_bins_max
    write (unit) self%config%iterations
    write (unit) self%config%n_calls
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
    do ch = 1, self%config%n_channel
       write (unit) ch, self%weight(ch)
    end do
    if (self%config%n_chains > 0) then
       do ch = 1, self%config%n_channel
          write (unit) ch, self%chain(ch)
       end do
    end if
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%write_binary_grid (unit)
    end do
  end subroutine vamp2_write_binary_grids

  subroutine vamp2_read_binary_grids (self, unit)
    class(vamp2_t), intent(out) :: self
    integer, intent(in) :: unit
    integer :: ch, ibuffer, jbuffer
    read (unit)
    read (unit) ibuffer
    read (unit) jbuffer
    select type (self)
    type is (vamp2_t)
       self = vamp2_t (n_channel = ibuffer, n_dim = jbuffer)
    end select
    read (unit) self%config%n_calls_min_per_channel
    read (unit) self%config%n_calls_threshold
    read (unit) self%config%n_chains
    read (unit) self%config%stratified
    read (unit) self%config%alpha
    read (unit) self%config%beta
    read (unit) self%config%n_bins_max
    read (unit) self%config%iterations
    read (unit) self%config%n_calls
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
    do ch = 1, self%config%n_channel
       read (unit) ibuffer, self%weight(ch)
    end do
    if (self%config%n_chains > 0) then
       do ch = 1, self%config%n_channel
          read (unit) ibuffer, self%chain(ch)
       end do
    end if
    do ch = 1, self%config%n_channel
       call self%integrator(ch)%read_binary_grid (unit)
    end do
  end subroutine vamp2_read_binary_grids


end module vamp2
