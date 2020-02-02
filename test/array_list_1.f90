program main
  use iso_fortran_env, only: ERROR_UNIT
  use array_list

  implicit none

  type(array_list_t) :: list
  integer :: ndx, data

  write (ERROR_UNIT, "(A)") "* Test interface:"
  write (ERROR_UNIT, "(A)") "- Init array_list_t ..."
  call list%init ()
  write (ERROR_UNIT, "(A)") "- Test adding a single element..."
  call list%add (1)
  write (ERROR_UNIT, "(A)") "- Test removing a single element..."
  data = list%remove ()
  write (ERROR_UNIT, "(A)") "- Test growing (unnecessary, so just return)..."
  call list%grow_size ()
  write (ERROR_UNIT, "(A)") "- Test adding elements beyond initial capacity..."
  call test_grow_and_add (list)
  write (ERROR_UNIT, "(A)") "- Test adding at specific position..."
  call list%add_at (10, -1)
  write (ERROR_UNIT, "(A)") "- Test removing at specific position..."
  data = list%remove_at (11)
  write (ERROR_UNIT, "(A)") "- Test reverse ordering..."
  call list%reverse_order ()
  write (ERROR_UNIT, "(A)") "- Test sorting..."
  call list%sort ()
  write (ERROR_UNIT, "(A)") "- Test finding..."
  ndx = list%find (1)
  write (ERROR_UNIT, "(A)") "- Test shrinking..."
  call list%shrink_size ()
  write (ERROR_UNIT, "(A)") "- Test get procedures..."
  call test_get_procedures (list)
  write (ERROR_UNIT, "(A)") "- Test clearing list..."
  call list%clear ()
  write (ERROR_UNIT, "(A)") "* Test (more complicated) combinations:"
  write (ERROR_UNIT, "(A)") "- Test growing (necessary) during adding..."
  call test_grow_and_add (list)
  write (ERROR_UNIT, "(A)") "- Test adding random data and sorting..."
  call test_sort (list)
  write (ERROR_UNIT, "(A)") "- Test finding (before sorted)..."
  call test_find (list)
contains
  subroutine test_get_procedures (list)
    type(array_list_t), intent(in) :: list
    integer :: n
    logical :: flag
    n = list%get(1)
    n = list%get_size ()
    n = list%get_count ()
    flag = list%is_element (1)
  end subroutine test_get_procedures

  subroutine test_grow_and_add (list)
    type(array_list_t), intent(inout) :: list
    integer :: i
    do i = 1, 2 * list%get_size ()
       call list%add (i)
    end do
  end subroutine test_grow_and_add

  subroutine test_get (list)
    class(array_list_t), intent(inout) :: list
    integer :: i, data
    do i = list%get_count (), 1, -1
       data = list%get (i)
       if (data == 0) then
          write (ERROR_UNIT, *) "INDEX EMPTY", i
       end if
    end do
  end subroutine test_get

  subroutine test_sort (list)
    class(array_list_t), intent(inout) :: list
    call list%add (6)
    call list%add (2)
    call list%add (9)
    call list%add (4)
    call list%add (8)
    call list%add (7)
    call list%sort ()
  end subroutine test_sort

  subroutine test_find (list)
    class(array_list_t), intent(inout) :: list
    write (ERROR_UNIT, *) " 6 INDEX", list%find (6)
    write (ERROR_UNIT, *) "-1 INDEX", list%find (-1)
    write (ERROR_UNIT, *) " 3 INDEX", list%find (3)
    write (ERROR_UNIT, *) "26 INDEX", list%find (26)
    call list%write ()
  end subroutine test_find
end program main
