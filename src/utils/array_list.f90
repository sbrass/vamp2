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

module array_list
  use kinds, only: default
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  implicit none

  private

  integer, parameter :: ARRAY_LIST_START_SIZE = 10
  real(default), parameter :: ARRAY_LIST_GROW_FACTOR = 1.5_default, &
       ARRAY_LIST_SHRINK_THRESHOLD = 0.3_default

  type :: array_list_t
     private
     integer, dimension(:), allocatable :: array
     !! Track the index to *current* item, to be stored.
     !! Must fulfill: 0 <= count <= size.
     integer :: count = 0
     !! size \in N.
     integer :: size = 0
   contains
     procedure :: write => array_list_write
     procedure :: init => array_list_init
     procedure :: add => array_list_add
     procedure :: add_at => array_list_add_at
     procedure :: remove => array_list_remove
     procedure :: remove_at => array_list_remove_at
     procedure :: get => array_list_get
     procedure :: get_size => array_list_get_size
     procedure :: get_count => array_list_get_count
     procedure :: grow_size => array_list_grow_size
     procedure :: shrink_size => array_list_shrink_size
     procedure :: reverse_order => array_list_reverse_order
     procedure :: sort => array_list_sort
     procedure :: is_element => array_list_is_element
     procedure :: find => array_list_find
     procedure :: clear => array_list_clear
     procedure :: is_full => array_list_is_full
     procedure :: is_empty => array_list_is_empty
     procedure :: is_index => array_list_is_index
  end type array_list_t

  public :: array_list_t
contains
  subroutine array_list_write (list, unit)
    class(array_list_t), intent(in) :: list
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A,2(1X,I3))") "COUNT / SIZE", list%count, list%size
    write (u, "(999(1X,I4))") list%array
  end subroutine array_list_write

  subroutine array_list_init (list)
    class(array_list_t), intent(out) :: list
    allocate (list%array(ARRAY_LIST_START_SIZE), source = 0)
    list%count = 0
    list%size = ARRAY_LIST_START_SIZE
  end subroutine array_list_init

  elemental function array_list_get (list, index) result (data)
    class(array_list_t), intent(in) :: list
    integer, intent(in) :: index
    integer :: data
    if (list%is_index (index)) then
       data = list%array(index)
    else
       data = 0
    end if
  end function array_list_get

  pure function array_list_get_count (list) result (count)
    class(array_list_t), intent(in) :: list
    integer :: count
    count = list%count
  end function array_list_get_count

  pure function array_list_get_size (list) result (size)
    class(array_list_t), intent(in) :: list
    integer :: size
    size = list%size
  end function array_list_get_size

  pure function array_list_is_full (list) result (flag)
    class(array_list_t), intent(in) :: list
    logical :: flag
    flag = list%count >= list%size
  end function array_list_is_full

  pure function array_list_is_empty (list) result (flag)
    class(array_list_t), intent(in) :: list
    logical :: flag
    flag = .not. list%count > 0
  end function array_list_is_empty

  pure function array_list_is_index (list, index) result (flag)
    class(array_list_t), intent(in) :: list
    integer, intent(in) :: index
    logical :: flag
    flag = 0 < index .and. index <= list%count
  end function array_list_is_index

  subroutine array_list_clear (list)
    class(array_list_t), intent(inout) :: list
    list%array = 0
    list%count = 0
    call list%shrink_size ()
  end subroutine array_list_clear

  subroutine array_list_add (list, data)
    class(array_list_t), intent(inout) :: list
    integer, intent(in) :: data
    list%count = list%count + 1
    if (list%is_full ()) then
       call list%grow_size ()
    end if
    list%array(list%count) = data
  end subroutine array_list_add

  subroutine array_list_grow_size (list)
    class(array_list_t), intent(inout) :: list
    integer, dimension(:), allocatable :: array
    integer :: new_size
    if (.not. list%is_full ()) return
    new_size = int (list%size * ARRAY_LIST_GROW_FACTOR)
    allocate (array(new_size), source = 0)
    array(:list%size) = list%array
    call move_alloc (array, list%array)
    list%size = size (list%array)
  end subroutine array_list_grow_size

  subroutine array_list_shrink_size (list)
    class(array_list_t), intent(inout) :: list
    integer, dimension(:), allocatable :: array
    integer :: new_size
    !! Apply shrink threshold on count.
    ! if (.not. list%count > 0) return
    new_size = max (list%count, ARRAY_LIST_START_SIZE)
    allocate (array(new_size), source = 0)
    !! \note We have to circumvent the allocate-on-assignment,
    !! hence, we explicitly set the array boundaries.
    array(:list%count) = list%array(:list%count)
    call move_alloc (array, list%array)
    list%size = new_size
  end subroutine array_list_shrink_size

  subroutine array_list_reverse_order (list)
    class(array_list_t), intent(inout) :: list
    list%array(:list%count) = list%array(list%count:1:-1)
  end subroutine array_list_reverse_order

  pure subroutine array_list_sort (list)
    class(array_list_t), intent(inout) :: list
    if (list%is_empty ()) return
    call quick_sort (list%array(:list%count))
  contains
    pure recursive subroutine quick_sort (array)
      integer, dimension(:), intent(inout) :: array
      integer :: pivot, tmp
      integer :: first, last
      integer i, j
      first = 1
      last = size(array)
      pivot = array(int ((first+last) / 2.))
      i = first
      j = last
      do
         do while (array(i) < pivot)
            i = i + 1
         end do
         do while (pivot < array(j))
            j = j - 1
         end do
         if (i >= j) exit
         tmp = array(i)
         array(i) = array(j)
         array(j) = tmp
         i = i + 1
         j = j - 1
      end do
      if (first < i - 1) call quick_sort(array(first:i - 1))
      if (j + 1 < last)  call quick_sort(array(j + 1:last))
    end subroutine quick_sort
  end subroutine array_list_sort

  pure function array_list_is_element (list, data) result (flag)
    class(array_list_t), intent(in) :: list
    integer, intent(in) :: data
    logical :: flag
    if (list%is_empty ()) then
       flag = .false.
    else
       flag = any (data == list%array)
    end if
  end function array_list_is_element

  function array_list_find (list, data) result (index)
    class(array_list_t), intent(inout) :: list
    integer, intent(in) :: data
    integer :: index
    if (list%is_empty () &
         .or. .not. list%is_element (data)) then
       index = 0
       return
    end if
    call list%sort () !! INTENT(INOUT)
    index = binary_search_leftmost (list%array(:list%count), data)
  contains
    pure function binary_search_leftmost (array, data) result (index)
      integer, dimension(:), intent(in) :: array
      integer, intent(in) :: data
      integer :: index
      integer :: left, right
      left = 1
      right = size (array)
      do while (left < right)
         index = floor ((left + right) / 2.)
         if (array(index) < data) then
            left = index + 1
         else
            right = index
         end if
      end do
      index = left
    end function binary_search_leftmost
  end function array_list_find

  subroutine array_list_add_at (list, index, data)
    class(array_list_t), intent(inout) :: list
    integer, intent(in) :: index
    integer, intent(in) :: data
    if (.not. list%is_index (index)) return
    if (list%is_full ()) then
       call list%grow_size ()
    end if
    list%array(index + 1:list%count + 1) = list%array(index:list%count)
    list%array(index) = data
    list%count = list%count + 1
  end subroutine array_list_add_at

  integer function array_list_remove (list) result (data)
    class(array_list_t), intent(inout) :: list
    if (list%is_empty ()) then
       data = 0
       return
    end if
    data = list%get (list%count)
    list%array(list%count) = 0
    list%count = list%count -1
  end function array_list_remove

  integer function array_list_remove_at (list, index) result (data)
    class(array_list_t), intent(inout) :: list
    integer, intent(in) :: index
    if (list%is_empty ()) then
       data = 0
       return
    end if
    data = list%get (index)
    list%array(index:list%count - 1) = list%array(index + 1:list%count)
    list%array(list%count) = 0
    list%count = list%count - 1
  end function array_list_remove_at
end module array_list
