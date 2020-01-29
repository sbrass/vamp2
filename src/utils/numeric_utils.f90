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

module numeric_utils
  use, intrinsic :: ieee_arithmetic, only: ieee_is_nan

  use kinds, only: default
  use constants
  use format_defs

  implicit none
  private

  public :: nearly_equal
  public:: vanishes
  interface vanishes
     module procedure vanishes_real, vanishes_complex
  end interface

  interface nearly_equal
     module procedure nearly_equal_real
     module procedure nearly_equal_complex
  end interface nearly_equal

contains
  elemental function nearly_equal_real (a, b, abs_smallness, rel_smallness) result (r)
    logical :: r
    real(default), intent(in) :: a, b
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    real(default) :: abs_a, abs_b, diff, abs_small, rel_small
    abs_a = abs (a)
    abs_b = abs (b)
    diff = abs (a - b)
    ! shortcut, handles infinities and nans
    if (a == b) then
       r = .true.
       return
    else if (ieee_is_nan (a) .or. ieee_is_nan (b) .or. ieee_is_nan (diff)) then
       r = .false.
       return
    end if
    abs_small = tiny_13; if (present (abs_smallness)) abs_small = abs_smallness
    rel_small = tiny_10; if (present (rel_smallness)) rel_small = rel_smallness
    if (abs_a < abs_small .and. abs_b < abs_small) then
       r = diff < abs_small
    else
       r = diff / max (abs_a, abs_b) < rel_small
    end if
  end function nearly_equal_real

  elemental function nearly_equal_complex (a, b, abs_smallness, rel_smallness) result (r)
    logical :: r
    complex(default), intent(in) :: a, b
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    r = nearly_equal_real (real (a), real (b), abs_smallness, rel_smallness) .and. &
        nearly_equal_real (aimag (a), aimag(b), abs_smallness, rel_smallness)
  end function nearly_equal_complex

  elemental function vanishes_real (x, abs_smallness, rel_smallness) result (r)
    logical :: r
    real(default), intent(in) :: x
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    r = nearly_equal (x, zero, abs_smallness, rel_smallness)
  end function vanishes_real

  elemental function vanishes_complex (x, abs_smallness, rel_smallness) result (r)
    logical :: r
    complex(default), intent(in) :: x
    real(default), intent(in), optional :: abs_smallness, rel_smallness
    r = vanishes_real (abs (x), abs_smallness, rel_smallness)
  end function vanishes_complex
end module numeric_utils
