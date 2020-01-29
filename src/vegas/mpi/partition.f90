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

module partition
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  implicit none

  private

  !! Define two modes used in each partition:
  !! (i) PARTITION_SINGLE allows for a one-to-one mapping between workers and
  !! resources.
  !! (ii) PARTITION_ALL allows for a all-to-one mapping between a complete set
  !! of workers to a resource, provided by that partition.
  integer, parameter :: PARTITION_SINGLE = 1, &
       PARTITION_ALL = 2

  type :: partition_t
     private
     character(:), allocatable :: name
     integer :: n_elements = 0
     integer :: mode = 0
   contains
     procedure :: init => partition_init
     procedure :: get_n_elements => partition_get_n_elements
     procedure :: get_mode => partition_get_mode
     procedure :: write => partition_write
  end type partition_t

  public :: partition_t, PARTITION_SINGLE, PARTITION_ALL
contains
  subroutine partition_init (partition, name, mode, n_elements)
    class(partition_t), intent(out) :: partition
    character(len=*), intent(in) :: name
    integer, intent(in) :: mode
    integer, intent(in) :: n_elements
    partition%name = name
    partition%mode = mode
    partition%n_elements = n_elements
  end subroutine partition_init

  integer function partition_get_n_elements (partition) result (n_elements)
    class(partition_t), intent(in) :: partition
    n_elements = partition%n_elements
  end function partition_get_n_elements

  integer function partition_get_mode (partition) result (mode)
    class(partition_t), intent(in) :: partition
    mode = partition%mode
  end function partition_get_mode

  subroutine partition_write (partition, unit)
    class(partition_t), intent(in) :: partition
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(3A)") "[", partition%name, "]"
    write (u, "(A,1X,I3)") "N_ELEMENTS", partition%n_elements
    select case (partition%mode)
    case (PARTITION_SINGLE)
       write (u, "(A)") "MODE ONE-TO-ONE"
    case (PARTITION_ALL)
       write (u, "(A)") "MODE ALL-TO-ONE"
    case default
       write (u, "(A)") "MODE UNKNOWN"
    end select
  end subroutine partition_write
end module partition

