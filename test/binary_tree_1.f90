module binary_tree_object
  implicit none

  private

  type :: btree_obj_t
     integer :: i = 0
  end type btree_obj_t

  public :: btree_obj_t
end module binary_tree_object

program main
  use iso_fortran_env, only: ERROR_UNIT
  use binary_tree
  use binary_tree_object

  implicit none

  integer, dimension(10) :: ndx = [1, 2, 5, 7, 19, 23, 97, -1, -6, 0]
  class(*), pointer :: obj
  type(binary_tree_t) :: btree
  type(binary_tree_iterator_t) :: iterator

  integer :: i, key

  write (ERROR_UNIT, "(A)") "* Insert fixed number of object into tree..."
  do i = 1, size (ndx)
     call allocate_obj (i, obj)
     call btree%insert (ndx(i), obj)
  end do

  write (ERROR_UNIT, "(A)") "* Search for all added objects in tree..."
  do i = size (ndx), 1, -1
     write (ERROR_UNIT, "(A,1X,I3,1X,L1)") "- Has key", ndx(i), btree%has_key (ndx(i))
     call btree%search (ndx(i), obj)
     select type (obj)
     type is (btree_obj_t)
        write (ERROR_UNIT, "(2(A,1X,I3,1X))") "- NDX", ndx(i), "OBJ", obj%i
     end select
  end do
  call btree%write (ERROR_UNIT)

  write (ERROR_UNIT, "(A)") "* Iterate over binary tree..."
  call iterator%init (btree)
  do while (iterator%is_iterable ())
     call iterator%next (key)
     call btree%search (key, obj)
     select type (obj)
     type is (btree_obj_t)
        write (ERROR_UNIT, "(2(A,1X,I3,1X))") "- KEY", key, "OBJ", obj%i
     end select
  end do

  write (ERROR_UNIT, "(A)") "* Search for a non-existing key..."
  write (ERROR_UNIT, "(A,1X,I3,1X,L1)") "- Has key", 123, btree%has_key (123)
  call btree%search (123, obj)
  write (ERROR_UNIT, "(A,1X,L1)") "- Object found", associated (obj)

  write (ERROR_UNIT, "(A)") "* Insert a duplicate key..."
  call allocate_obj (42, obj)
  call btree%insert (1, obj)
contains
  subroutine allocate_obj (num, obj)
    integer, intent(in) :: num
    class(*), pointer, intent(out) :: obj
    allocate (btree_obj_t :: obj)
    select type (obj)
    type is (btree_obj_t)
       obj%i = num
    end select
  end subroutine allocate_obj
end program main
