module resources
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use array_list

  use diagnostics

  implicit none

  private

  type :: resource_t
     integer :: resource_id = 0
     integer :: n_assigned_workers = 0
   contains
     procedure :: write => resource_write
  end type resource_t

  integer, parameter :: STATE_SINGLE = 1, &
       STATE_ALL = 2

  type :: resource_state_t
     integer :: n_workers = 0
     integer :: mode = 0
     type(array_list_t) :: resource_stack
     type(array_list_t) :: finished_stack
   contains
     procedure :: write => resource_state_write
     procedure :: init => resource_state_init
     procedure :: add_resource => resource_state_add_resource
     procedure :: freeze => resource_state_freeze
     procedure :: clear => resource_state_clear
     procedure :: has_resource => resource_state_has_resource
     procedure :: assign_resource => resource_state_assign_resource
     procedure :: free_resource => resource_state_free_resource
  end type resource_state_t

  public :: resource_t, resource_state_t
contains
  subroutine resource_write (resource, unit)
    class(resource_t), intent(in) :: resource
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A,1X,I3,1X,A,1X,I3)") &
         "RESOURCE_ID", resource%resource_id, &
         "N_ASSIGNED_WORKERS", resource%n_assigned_workers
  end subroutine resource_write

  subroutine resource_state_write (state, unit)
    class(resource_state_t), intent(in) :: state
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A,1X,I0)") "N_STATE_WORKERS", state%n_workers
    select case (state%mode)
    case (STATE_SINGLE)
       write (u, "(A)") "MODE ONE-TO-ONE"
    case (STATE_ALL)
       write (u, "(A)") "MODE ALL-TO-ONE"
    case default
       write (u, "(A)") "UNSUPPORTED MODE"
    end select
    write (u, "(A)") "RESOURCE"
    call state%resource_stack%write (u)
    write (u, "(A)") "FINISHED"
    call state%finished_stack%write (u)
  end subroutine resource_state_write

  subroutine resource_state_init (state, mode, n_workers)
    class(resource_state_t), intent(out) :: state
    integer, intent(in) :: mode
    integer, intent(in) :: n_workers
    state%mode = mode
    state%n_workers = n_workers
    call state%resource_stack%init ()
    call state%finished_stack%init ()
  end subroutine resource_state_init

  subroutine resource_state_add_resource (state, i_resource)
    class(resource_state_t), intent(inout) :: state
    integer, intent(in) :: i_resource
    call state%resource_stack%add (i_resource)
  end subroutine resource_state_add_resource

  subroutine resource_state_freeze (state)
    class(resource_state_t), intent(inout) :: state
    call state%resource_stack%sort ()
    call state%resource_stack %reverse_order ()
  end subroutine resource_state_freeze

  subroutine resource_state_clear (state)
    class(resource_state_t), intent(inout) :: state
    call state%resource_stack%clear ()
    call state%finished_stack%clear ()
  end subroutine resource_state_clear

  elemental function resource_state_has_resource (state) result (flag)
    class(resource_state_t), intent(in) :: state
    logical :: flag
    flag = .not. state%resource_stack%is_empty ()
  end function resource_state_has_resource

  function resource_state_assign_resource (state) result (i_resource)
    class(resource_state_t), intent(inout) :: state
    integer :: i_resource
    if (state%resource_stack%is_empty ()) then
       i_resource = 0
       call msg_bug ("Error: No leftover resource on stack.")
       return
    end if
    i_resource = state%resource_stack%remove () !! Pop last element from stack.
  end function resource_state_assign_resource

  subroutine resource_state_free_resource (state, i_resource)
    class(resource_state_t), intent(inout) :: state
    integer, intent(in) :: i_resource
    if (state%resource_stack%is_element (i_resource)) then
       call msg_bug ("Error: Cannot free resource, still on resource stack.")
    end if
    call state%finished_stack%add (i_resource)
  end subroutine resource_state_free_resource
end module resources
