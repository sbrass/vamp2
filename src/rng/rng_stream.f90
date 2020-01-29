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

module rng_stream

  use kinds, only: default
  use kinds, only: i16
  use io_units
  use format_utils, only: write_indent
  use diagnostics

  use rng_base

  implicit none
  private

  real(default), parameter :: &
       m1 = 4294967087.0_default, &
       m2 = 4294944443.0_default, &
       norm = 1.0_default / (m1 + 1.0_default)
  real(default), dimension(3, 3), parameter :: A1p0 = reshape (&
       [0.0_default, 1.0_default, 0.0_default, &
       0.0_default, 0.0_default, 1.0_default, &
       -810728.0_default, 1403580.0_default, 0.0_default], &
       shape (A1p0), order = [2, 1])
  real(default), dimension(3, 3), parameter :: A2p0 = reshape (&
       [0.0_default, 1.0_default, 0.0_default, &
       0.0_default, 0.0_default, 1.0_default, &
       -1370589.0_default, 0.0_default, 527612.0_default], &
       shape (A2p0), order = [2, 1])
  real(default), dimension(3, 3), parameter :: A1p76 = reshape (&
       [82758667.0_default, 1871391091.0_default, 4127413238.0_default, &
       3672831523.0_default, 69195019.0_default, 1871391091.0_default, &
       3672091415.0_default, 352874325.0_default, 69195019.0_default], &
       shape(A1p76), order = [2, 1])
  real(default), dimension(3, 3), parameter :: A2p76 = reshape (&
       [1511326704.0_default, 3759209742.0_default, 1610795712.0_default, &
       4292754251.0_default, 1511326704.0_default, 3889917532.0_default, &
       3859662829.0_default, 4292754251.0_default, 3708466080.0_default], &
       shape(A2p76), order = [2, 1])
real(default), dimension(3,3), parameter :: A1p127 = reshape (&
     [2427906178.0_default, 3580155704.0_default, 949770784.0_default, &
     226153695.0_default, 1230515664.0_default, 3580155704.0_default, &
     1988835001.0_default, 986791581.0_default, 1230515664.0_default], &
     shape(A1p127), order = [2, 1])
real(default), dimension(3,3), parameter :: A2p127 = reshape (&
     [1464411153.0_default, 277697599.0_default, 1610723613.0_default, &
     32183930.0_default, 1464411153.0_default, 1022607788.0_default, &
     2824425944.0_default, 32183930.0_default, 2093834863.0_default], &
     shape(A2p127), order = [2, 1])
  real(default), parameter :: &
       a12 = 1403580.0_default, &
       a13n = 810728.0_default, &
       a21 = 527612.0_default, &
       a23n = 1370589.0_default, &
       two53 = 9007199254740992.0_default, &
       two17 = 131072.0_default

  public :: rng_stream_t
  public :: rng_stream_factory_t
  private :: matmul_mod
  private :: mult_mod

  type, extends(rng_t) :: rng_stream_t
     private
     logical :: antithetic = .false.
     logical :: increased_precision = .false.
     real(default), dimension(3, 2) :: current_position = 1234._default
     real(default), dimension(3, 2) :: beginning_substream = 1234._default
     real(default), dimension(3, 2) :: initial_stream = 1234._default
   contains
     procedure, public :: write => rng_stream_write
     procedure, public :: init => rng_stream_init
     procedure, public :: final => rng_stream_final
     procedure, public :: set_initial_stream => rng_stream_set_initial_stream
     procedure, public :: generate_single => rng_stream_generate_single
     procedure, public :: generate_array => rng_stream_generate_array
     procedure, public :: reset_stream => rng_stream_reset_stream
     procedure, public :: reset_substream => rng_stream_reset_substream
     procedure, public :: next_substream => rng_stream_next_substream
     procedure, public :: advance_state => rng_stream_advance_state
  end type rng_stream_t

  type, extends(rng_factory_t) :: rng_stream_factory_t
     real(default), dimension(3, 2) :: seed = 1234._default
   contains
     procedure, public :: write => rng_stream_factory_write
     procedure, public :: init => rng_stream_factory_init
     procedure, public :: make => rng_stream_factory_make
     procedure, private :: advance_seed => rng_stream_factory_advance_seed
  end type rng_stream_factory_t


contains

  subroutine rng_stream_write (rng, unit, indent)
    class(rng_stream_t), intent(in) :: rng
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: indent
    integer :: j, u, ind
    u = given_output_unit (unit)
    ind = 0; if (present (indent)) ind = indent
    call write_indent (u, ind)
    write (u, "(A)") "RNG Stream generator"
    call write_indent (u, ind)
    write (u, "(A)") "Current position = [ "
    call write_indent (u, ind)
    write (u, "(3(F20.1,',',1X))") rng%current_position(:, 1)
    call write_indent (u, ind)
    write (u, "(3(F20.1,',',1X))") rng%current_position(:, 2)
    call write_indent (u, ind)
    write (u, "(A)") "]"
    call write_indent (u, ind)
    write (u, "(A)") "Beginning substream = [ "
    call write_indent (u, ind)
    write (u, "(3(F20.1,',',1X))") rng%beginning_substream(:, 1)
    call write_indent (u, ind)
    write (u, "(3(F20.1,',',1X))") rng%beginning_substream(:, 2)
    call write_indent (u, ind)
    write (u, "(A)") "]"
    call write_indent (u, ind)
    write (u, "(A)") "Initial stream = [ "
    call write_indent (u, ind)
    write (u, "(3(F20.1,',',1X))") rng%initial_stream(:, 1)
    call write_indent (u, ind)
    write (u, "(3(F20.1,',',1X))") rng%initial_stream(:, 2)
    call write_indent (u, ind)
    write (u, "(A)") "]"
  end subroutine rng_stream_write

  subroutine rng_stream_init (rng, seed)
    class(rng_stream_t), intent(out) :: rng
    integer, intent(in), optional :: seed
    real(default), dimension(3, 2) :: real_seed
    if (present (seed)) then
       real_seed = real(seed, default)
       call rng%set_initial_stream (real_seed)
    end if
  end subroutine rng_stream_init

  subroutine rng_stream_final (rng)
    class(rng_stream_t), intent(inout) :: rng
  end subroutine rng_stream_final

  subroutine rng_stream_set_initial_stream (rng, seed)
    class(rng_stream_t), intent(inout) :: rng
    real(default), dimension(3, 2), intent(in) :: seed
    if (any (seed(:, 1) > m1)) then
       write (msg_buffer, "(A, F20.1, 1X, A)") "ERROR: seed(:, 1) >= ", m1, ",&
            & Seed is not set."
       call msg_fatal ()
    end if
    if (any (seed(:, 2) > m2)) then
       write (msg_buffer, "(A, F20.1, 1X, A)") "ERROR: seed(:, 2) >= ", m2, ",&
            & Seed is not set."
       call msg_fatal ()
    end if
    if (any (seed(:, 1) == 0)) then
       write (msg_buffer, "(A, F20.1, 1X, A)") "ERROR: First 3 seed = 0."
       call msg_fatal ()
    end if
    if (any (seed(:, 2) == 0)) then
       write (msg_buffer, "(A, F20.1, 1X, A)") "ERROR: Last 3 seed = 0."
       call msg_fatal ()
    end if
    rng%initial_stream = seed
    call rng%reset_stream ()
  end subroutine rng_stream_set_initial_stream

  subroutine rng_stream_generate_single (rng, x)
    class(rng_stream_t), intent(inout) :: rng
    real(default), intent(out) :: x
    associate (y => rng%current_position)
      y(:, 1) = mod (matmul (A1p0, y(:, 1)), m1)
      if (y(3, 1) < 0.) y(3, 1) = y(3, 1) + m1
      y(:, 2) = mod (matmul (A2p0, y(:, 2)), m2)
      if (y(3, 2) < 0.) y(3, 2) = y(3, 2) + m2
      x = y(3, 1) - y(3, 2)
      if (x < 0.) x = x + m1
      x = x * norm
    end associate
    if (rng%antithetic) x = 1. - x
  end subroutine rng_stream_generate_single

  subroutine rng_stream_generate_array (rng, x)
    class(rng_stream_t), intent(inout) :: rng
    real(default), dimension(:), intent(out) :: x
    integer :: j
    do j = 1, size (x)
       call rng%generate_single(x(j))
    end do
  end subroutine rng_stream_generate_array

  subroutine rng_stream_reset_stream (rng)
    class(rng_stream_t), intent(inout) :: rng
    rng%current_position = rng%initial_stream
    rng%beginning_substream = rng%initial_stream
  end subroutine rng_stream_reset_stream

  subroutine rng_stream_reset_substream (rng)
    class(rng_stream_t), intent(inout) :: rng
    rng%current_position = rng%beginning_substream
  end subroutine rng_stream_reset_substream

  subroutine rng_stream_next_substream (rng)
    class(rng_stream_t), intent(inout) :: rng
    associate (x => rng%beginning_substream)
      x(:,1) = matmul_mod (A1p76, x(:,1), m1)
      x(:,2) = matmul_mod (A2p76, x(:,2), m2)
    end associate
    call rng%reset_substream ()
  end subroutine rng_stream_next_substream

  subroutine rng_stream_advance_state (rng, n)
    class(rng_stream_t), intent(inout) :: rng
    integer, intent(in) :: n
    real(default), dimension(3, 3) :: A1pn, A2pn
    associate (x => rng%current_position)
      A1pn = matpow_mod (A1p0, m1, n)
      A2pn = matpow_mod (A2p0, m2, n)
      x(:,1) = matmul_mod (A1pn, x(:,1), m1)
      x(:,2) = matmul_mod (A2pn, x(:,2), m2)
    end associate
  end subroutine rng_stream_advance_state

  function matpow_mod (a, m, n) result (c)
    real(default), dimension(3, 3), intent(in) :: a
    real(default), intent(in) :: m
    integer, intent(in) :: n
    real(default), dimension(3, 3) :: c
    real(default), dimension(3, 3) :: w
    integer :: i
    w = a
    c = 0.; forall (i = 1:3) c(i, i) = 1.
    i = n
    do while (i > 0)
       if (mod(i, 2) /= 0) c = matmat_mod (w, c, m)
       w = matmat_mod (w, w, m)
       i = i / 2
    end do
  end function matpow_mod

  function matmat_mod (a, b, m) result (c)
    real(default), dimension(3, 3), intent(in) :: a
    real(default), dimension(3, 3), intent(in) :: b
    real(default), dimension(3, 3) :: c
    real(default), intent(in) :: m
    integer :: i
    do i = 1, 3
       c(:, i) = matmul_mod (a, b(:, i), m)
    end do
  end function matmat_mod

  subroutine rng_stream_factory_write (object, unit)
    class(rng_stream_factory_t), intent(in) :: object
    integer, intent(in), optional :: unit
    integer :: u
    u = given_output_unit (unit)
    write (u, "(1x,A)") "RNG Stream factory"
    write (u, "(1x,A,6(F20.1,',',1X),A)") "Next seed = [ "
    write (u, "(1x,3(F20.1,',',1X))") object%seed(:, 1)
    write (u, "(1x,3(F20.1,',',1X))") object%seed(:, 2)
    write (u, "(1x,A)") "]"
  end subroutine rng_stream_factory_write

  subroutine rng_stream_factory_init (factory, seed)
    class(rng_stream_factory_t), intent(out) :: factory
    integer(i16), intent(in), optional :: seed
    real(default), dimension(3, 2) :: real_seed
    if (present (seed)) then
       factory%seed = real(seed, default)
    end if
  end subroutine rng_stream_factory_init

  subroutine rng_stream_factory_make (factory, rng)
    class(rng_stream_factory_t), intent(inout) :: factory
    class(rng_t), intent(out), allocatable :: rng
    allocate (rng_stream_t :: rng)
    select type (rng)
    type is (rng_stream_t)
       call rng%init ()
       call rng%set_initial_stream (factory%seed)
    end select
    call factory%advance_seed ()
  end subroutine rng_stream_factory_make

  subroutine rng_stream_factory_advance_seed (factory)
    class(rng_stream_factory_t), intent(inout) :: factory
    factory%seed(:,1) = matmul_mod (A1p127, factory%seed(:,1), m1)
    factory%seed(:,2) = matmul_mod (A2p127, factory%seed(:,2), m2)
  end subroutine rng_stream_factory_advance_seed

  function matmul_mod (a, u, m) result (v)
    real(default), dimension(:, :), intent(in) :: a
    real(default), dimension(:), intent(in) :: u
    real(default), intent(in) :: m
    real(default), dimension(size (u)) :: v
    integer :: i
    do i = 1, 3
       v(i) = mult_mod (a(i, 1), u(1), 0.0_default, m)
       v(i) = mult_mod (a(i, 2), u(2), v(i), m)
       v(i) = mult_mod (a(i, 3), u(3), v(i), m)
    end do
  end function matmul_mod

  function mult_mod (a, b, c, m) result (v)
    real(default), intent(in) :: a
    real(default), intent(in) :: b
    real(default), intent(in) :: c
    real(default), intent(in) :: m
    real(default) :: v
    integer :: a1
    real(default) :: a2
    v = a * b + c
    if (v >= two53 .or. v <= -two53) then
       a1 = int (a / two17)
       a2 = a - a1 * two17
       v = mod (a1 * b, m)
       v = v * two17 + a2 * b + c
    end if
    v = mod (v, m)
    if (v < 0.0) v = v + m
  end function mult_mod


end module rng_stream
