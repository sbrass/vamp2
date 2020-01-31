module binary_tree
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  implicit none

  private

  type :: binary_tree_iterator_t
     type(binary_tree_t), pointer :: btree => null ()
     integer, dimension(:), allocatable :: key
     integer :: current
     !! current ∈ {1, N}.
   contains
     procedure :: init => binary_tree_iterator_init
     procedure :: is_iterable => binary_tree_iterator_is_iterable
     procedure :: next => binary_tree_iterator_next
     final :: binary_tree_iterator_final
  end type binary_tree_iterator_t

  type :: binary_tree_node_t
     integer :: height = 0
     type(binary_tree_node_t), pointer :: left => null ()
     type(binary_tree_node_t), pointer :: right => null ()
     !!
     integer :: key = 0
     class(*), pointer :: obj => null ()
   contains
     procedure :: init => binary_tree_node_init
     procedure :: write => binary_tree_node_write
     procedure :: get_balance => binary_tree_node_get_balance
     procedure :: increment_height => binary_tree_node_increment_height
     procedure :: final => binary_tree_node_final
  end type binary_tree_node_t

  type :: binary_tree_t
     integer :: n_elements = 0
     type(binary_tree_node_t), pointer :: root => null ()
   contains
     procedure :: write => binary_tree_write
     procedure :: get_n_elements => binary_tree_get_n_elements
     procedure :: insert => binary_tree_insert
     procedure, private :: insert_node => binary_tree_insert_node
     procedure, private :: balance => binary_tree_balance
     procedure :: search => binary_tree_search
     procedure :: has_key => binary_tree_has_key
     procedure, private :: rotate_left => binary_tree_rotate_left
     procedure, private :: rotate_right => binary_tree_rotate_right
     final :: binary_tree_final
  end type binary_tree_t

  public :: binary_tree_t, binary_tree_iterator_t
contains
  !! We store all keys of the binary tree in an index array.
  !! Flatten the tree O(log n), each access is then O(1).
  !! However, accessing the corresponding object costs one O(log n).
  subroutine binary_tree_iterator_init (iterator, btree)
    class(binary_tree_iterator_t), intent(inout) :: iterator
    type(binary_tree_t), target :: btree
    type(binary_tree_node_t), pointer :: node
    integer :: idx
    if (.not. btree%get_n_elements () > 0) then
       write (ERROR_UNIT, "(A)") "Error: Cannot iterate over empty binary tree."
       stop 1
    end if
    iterator%btree => btree
    allocate (iterator%key(btree%get_n_elements ()), source = 0)
    iterator%current = 1
    idx = 1
    call fill_key (idx, iterator%key, btree%root)
  contains
    recursive subroutine fill_key (idx, key, node)
      integer, intent(inout) :: idx
      integer, dimension(:), intent(inout) :: key
      type(binary_tree_node_t), pointer :: node
      if (associated (node%left)) &
         call fill_key (idx, key, node%left)
      if (associated (node%right)) &
         call fill_key (idx, key, node%right)
      key(idx) = node%key
      idx = idx + 1
    end subroutine fill_key
  end subroutine binary_tree_iterator_init

  function binary_tree_iterator_is_iterable (iterator) result (flag)
    class(binary_tree_iterator_t), intent(in) :: iterator
    logical :: flag
    flag = iterator%current <= size (iterator%key)
  end function binary_tree_iterator_is_iterable

  subroutine binary_tree_iterator_next (iterator, key, obj)
    class(binary_tree_iterator_t), intent(inout) :: iterator
    integer, intent(out) :: key
    class(*), pointer, intent(out) :: obj
    if (.not. iterator%is_iterable ()) then
       key = 0
       obj => null ()
    end if
    associate (current => iterator%current)
      key = iterator%key(current)
      call iterator%btree%search (key, obj)
      current = current + 1
    end associate
  end subroutine binary_tree_iterator_next

  subroutine binary_tree_iterator_final (iterator)
    type(binary_tree_iterator_t), intent(inout) :: iterator
    nullify (iterator%btree)
    if (allocated (iterator%key)) deallocate (iterator%key)
  end subroutine binary_tree_iterator_final

  subroutine binary_tree_node_init (btree_node, key, obj)
    class(binary_tree_node_t), intent(inout) :: btree_node
    integer, intent(in) :: key
    class(*), pointer :: obj
    btree_node%height = 1
    btree_node%left => null ()
    btree_node%right => null ()
    btree_node%key = key
    btree_node%obj => obj
  end subroutine binary_tree_node_init

  recursive subroutine binary_tree_node_write (btree_node, unit, level)
    class(binary_tree_node_t), intent(in) :: btree_node
    integer, intent(in) :: unit
    integer, intent(in) :: level
    character(len=18) :: fmt
    if (level > 0) then
       write (fmt, "(A,I3,A)") "(", 3 * level, "X,I3,1X,I3)"
    else
       fmt = "(I3,1X,I3)"
    end if
    write (unit, fmt) btree_node%key, btree_node%height
    ! write (unit, fmt) btree_node%key, btree_node%get_balance ()
    if (associated (btree_node%right)) &
         call btree_node%right%write (unit, level = level + 1)
    if (associated (btree_node%left)) &
         call btree_node%left%write (unit, level = level + 1)
  end subroutine binary_tree_node_write

  integer function binary_tree_node_get_balance (btree_node) result (balance)
    class(binary_tree_node_t), intent(in) :: btree_node
    integer :: leftHeight, rightHeight
    leftHeight = 0
    rightHeight = 0
    if (associated (btree_node%left)) leftHeight = btree_node%left%height
    if (associated (btree_node%right)) rightHeight = btree_node%right%height
    balance = leftHeight - rightHeight
  end function binary_tree_node_get_balance

  subroutine binary_tree_node_increment_height (btree_node)
    class(binary_tree_node_t), intent(inout) :: btree_node
    integer :: leftHeight, rightHeight
    leftHeight = 0
    rightHeight = 0
    if (associated (btree_node%left)) leftHeight = btree_node%left%height
    if (associated (btree_node%right)) rightHeight = btree_node%right%height
    btree_node%height = max (leftHeight, rightHeight) + 1
  end subroutine binary_tree_node_increment_height

  recursive subroutine binary_tree_node_final (btree_node)
    class(binary_tree_node_t), intent(inout) :: btree_node
    if (associated (btree_node%left)) then
       call btree_node%left%final ()
       deallocate (btree_node%left)
    end if
    if (associated (btree_node%right)) then
       call btree_node%right%final ()
       deallocate (btree_node%right)
    end if
    nullify (btree_node%obj)
  end subroutine binary_tree_node_final

  subroutine binary_tree_write (btree, unit)
    class(binary_tree_t), intent(in) :: btree
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A,1X,I3)") "Number of elements", btree%n_elements
    if (associated (btree%root)) then
       call btree%root%write (unit, level = 0)
    else
       write (u, "(A)") "Binary tree is empty."
    end if
  end subroutine binary_tree_write

  subroutine binary_tree_final (btree)
    type(binary_tree_t), intent(inout) :: btree
    if (associated (btree%root)) then
       call btree%root%final ()
    end if
  end subroutine binary_tree_final

  integer function binary_tree_get_n_elements (btree) result (n)
    class(binary_tree_t), intent(in) :: btree
    n = btree%n_elements
  end function binary_tree_get_n_elements

  subroutine binary_tree_insert (btree, key, obj)
    class(binary_tree_t), intent(inout) :: btree
    integer, intent(in) :: key
    class(*), pointer, intent(in) :: obj
    type(binary_tree_node_t), pointer :: node
    allocate (node)
    call node%init (key, obj)
    btree%n_elements = btree%n_elements + 1
    if (.not. associated (btree%root)) then
       btree%root => node
    else
       call btree%insert_node (btree%root, node)
    end if
  end subroutine binary_tree_insert

  recursive subroutine binary_tree_insert_node (btree, parent, node)
    class(binary_tree_t), intent(in) :: btree
    type(binary_tree_node_t), intent(inout), pointer :: parent
    type(binary_tree_node_t), intent(in), pointer :: node
    !! Choose left or right, if associated descend recursively into subtree,
    !! else insert node.
    if (node%key > parent%key) then
       if (associated (parent%right)) then
          call btree%insert_node (parent%right, node)
       else
          parent%right => node
       end if
    else if (node%key < parent%key) then
       if (associated (parent%left)) then
          call btree%insert_node (parent%left, node)
       else
          parent%left => node
       end if
    else
       write (ERROR_UNIT, "(A,1X,I0)") "Error: MUST not insert duplicate key", node%key
       stop 1
    end if
    call parent%increment_height ()
    call btree%balance (parent, node%key)
  end subroutine binary_tree_insert_node

  !! Subtree: root of subtree (which is unbalance, refer to A in diagrams.)
  subroutine binary_tree_balance (btree, subtree, key)
    class(binary_tree_t), intent(in) :: btree
    type(binary_tree_node_t), intent(inout), pointer :: subtree
    integer, intent(in) :: key
    type(binary_tree_node_t), pointer :: node, newNode
    integer :: balance
    balance = subtree%get_balance ()
    node => subtree
    newNode => null ()
    !! balance ≔ h_left - h_right.
    !! Proof: balance > 0 ⇒ too many elements on the left side of the subtree.
    !! Proof: balance < 0 ⇒ too many elements on the right side of the subtree.
    if (balance > 1) then
       !! ⇒ left-side of subtree
       !!      A3(2)         B2(1)
       !!     /             /    \
       !!    B2(1)         C1(0)  A1(0)
       !!   /
       !!  C1(0)
       !!
       !!    A3(3)           A1(2)           C2(1)
       !!   /               /               /    \
       !!  B1(1)  LEFT     C2(1)    RIGHT  B1(0) A3(0)
       !!   \             /
       !!    C2(0)       B1(0)
       if (subtree%left%key > key) then !! rotate right
          call btree%rotate_right (node, newNode)
       else !! subtree%left%key < key, rotate left, then right.
          call btree%rotate_left (node, newNode)
          node => newNode
          call btree%rotate_right (node, newNode)
       end if
    else if (balance < -1) then
       !! ⇒ right-side of subtree
       !!   A0(2)           B1(1)
       !!    \             /    \
       !!     B1(1)       A1(0)  C3(0)
       !!      \
       !!       C3(0)*
       !!
       !!   A1(2)           A1(2)               C2(1)
       !!    \                 \               /    \
       !!     B3(1)  RIGHT     C2(1)    LEFT  A1(0) B3(0)
       !!    /                   \
       !!   C2(0)                B3(0)
       if (subtree%right%key < key) then !! rotate left
          call btree%rotate_left (node, newNode)
       else !! subtree%right%key > key, rotate right, then left.
          call btree%rotate_right (node, newNode)
          node => newNode
          call btree%rotate_left (node, newNode)
       end if
    end if
    if (associated (newNode)) subtree => newNode
  end subroutine binary_tree_balance

  subroutine binary_tree_search (btree, key, obj)
    class(binary_tree_t), intent(in) :: btree
    integer, intent(in) :: key
    class(*), pointer, intent(out) :: obj
    type(binary_tree_node_t), pointer :: current
    current => btree%root
    obj => null ()
    if (.not. associated (current)) return
    do while (current%key /= key)
       if (current%key > key) then
          current => current%left
       else
          current => current%right
       end if
       if (.not. associated (current)) then
          !! Key not found.
          exit
       end if
    end do
    if (associated (current)) obj => current%obj
  end subroutine binary_tree_search

  function binary_tree_has_key (btree, key) result (flag)
    class(binary_tree_t), intent(in) :: btree
    integer, intent(in) :: key
    logical :: flag
    type(binary_tree_node_t), pointer :: current
    current => btree%root
    flag = .false.
    if (.not. associated (current)) return
    do while (current%key /= key)
       if (current%key > key) then
          current => current%left
       else
          current => current%right
       end if
       if (.not. associated (current)) then
          !! Key not found.
          return
       end if
    end do
    flag = .true.
  end function binary_tree_has_key

  !!      A     Move B to A.
  !!     / \
  !!    B   E   1. Split B from A%left.
  !!   / \      2. Temporarily pointer to D.
  !!  C   D     3. Replace pointer to D by pointer to A - E.
  !!            4. Set temporary pointer to D to A%left.
  !!
  !! 1.+2. B       T => D    A
  !!      /                   \
  !!     C                     E
  !!
  !! 3.    B       T => D
  !!      / \
  !!     C   A
  !!          \
  !!           E
  !!
  !! 4.    B
  !!      / \
  !!     C   A
  !!        / \
  !!       D   E
  !!
  !! \param[inout] root Root/parent root (A).
  !! \param[out] new_root New root/parent root (B).
  subroutine binary_tree_rotate_right (btree, root, new_root)
    class(binary_tree_t), intent(in) :: btree
    type(binary_tree_node_t), pointer, intent(inout) :: root
    type(binary_tree_node_t), pointer, intent(out) :: new_root
    type(binary_tree_node_t), pointer :: tmp
    new_root => root%left
    tmp => new_root%right
    new_root%right => root
    root%left => tmp
    call root%increment_height ()
    call new_root%increment_height ()
  end subroutine binary_tree_rotate_right

  !!      A      Move B to A.
  !!     / \
  !!    E   B    1. Split B from A%left.
  !!       / \   2. Temporarily pointer to C.
  !!      C   D  3. Replace pointer to C by pointer to A - E.
  !!             4. Set temporary pointer to C to A%right.
  !!
  !! 1.+2. B       T => C    A
  !!        \               /
  !!         D             E
  !!
  !! 3.    B       T => C
  !!      / \
  !!     A   D
  !!    /
  !!   E
  !!
  !! 4.    B
  !!      / \
  !!     A   D
  !!    / \
  !!   E   C
  subroutine binary_tree_rotate_left (btree, root, new_root)
    class(binary_tree_t), intent(in) :: btree
    type(binary_tree_node_t), pointer, intent(inout) :: root
    type(binary_tree_node_t), pointer, intent(out) :: new_root
    type(binary_tree_node_t), pointer :: tmp
    new_root => root%right
    tmp => new_root%left
    new_root%left => root
    root%right => tmp
    call root%increment_height ()
    call new_root%increment_height ()
  end subroutine binary_tree_rotate_left
end module binary_tree
