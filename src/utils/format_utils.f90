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

module format_utils
  use kinds, only: default
  use io_units, only: given_output_unit

  implicit none
  private

  public :: write_separator
  public :: write_indent
  public :: write_integer_array
  public :: pac_fmt

contains

  subroutine write_separator (u, mode)
    integer, intent(in) :: u
    integer, intent(in), optional :: mode
    integer :: m
    m = 1;  if (present (mode))  m = mode
    select case (m)
    case default
       write (u, "(A)")  repeat ("-", 72)
    case (1)
       write (u, "(A)")  repeat ("-", 72)
    case (2)
       write (u, "(A)")  repeat ("=", 72)
    end select
  end subroutine write_separator

  subroutine write_indent (unit, indent)
    integer, intent(in) :: unit
    integer, intent(in), optional :: indent
    if (present (indent)) then
       write (unit, "(1x,A)", advance="no")  repeat ("  ", indent)
    end if
  end subroutine write_indent

  subroutine write_integer_array (array, unit, n_max, no_skip)
    integer, intent(in), dimension(:) :: array
    integer, intent(in), optional :: unit
    integer, intent(in), optional :: n_max
    logical, intent(in), optional :: no_skip
    integer :: u, i, n
    logical :: yorn
    u = given_output_unit (unit)
    yorn = .false.; if (present (no_skip)) yorn = no_skip
    if (present (n_max)) then
       n = n_max
    else
       n = size (array)
    end if
    do i = 1, n
       if (i < n .or. yorn) then
          write (u, "(I0, A)", advance = "no") array(i), ", "
       else
          write (u, "(I0)") array(i)
       end if
    end do
  end subroutine write_integer_array

  subroutine pac_fmt (fmt, fmt_orig, fmt_pac, pacify)
    character(*), intent(in) :: fmt_orig, fmt_pac
    character(*), intent(out) :: fmt
    logical, intent(in), optional :: pacify
    logical :: pacified
    pacified = .false.
    if (present (pacify))  pacified = pacify
    if (pacified) then
       fmt = fmt_pac
    else
       fmt = fmt_orig
    end if
  end subroutine pac_fmt
end module format_utils
