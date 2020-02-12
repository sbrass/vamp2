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

  type :: resource_state_t
     type(array_list_t) :: unassigned
     type(array_list_t) :: started
     type(array_list_t) :: finished
   contains
     procedure :: write => resource_state_write
     procedure :: init => resource_state_init
     procedure :: add_resource => resource_state_add_resource
     procedure :: freeze => resource_state_freeze
     procedure :: has_unassigned_resource => resource_state_has_unassigned_resource
     procedure :: start_resource => resource_state_start_resource
     procedure :: has_started_resource => resource_state_has_started_resource
     procedure :: is_started_resource => resource_state_is_started_resource
     procedure :: finish_resource => resource_state_finish_resource
     procedure :: is_finished_resource => resource_state_is_finished_resource
     procedure :: all_finished_resources => resource_state_all_finished_resources
  end type resource_state_t

  public :: resource_t, resource_state_t
contains
  subroutine resource_write (resource, unit)
    class(resource_t), intent(in) :: resource
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A,1X,I3,1X,A,1X,F7.4,1X,2(A,1X,I3,1X),2(A,1X,L1,1X))") &
         "RESOURCE_ID", resource%resource_id, &
         "WEIGHT", resource%weight, &
         "N_ASSIGNED_WORKERS", resource%n_assigned_workers
  end subroutine resource_write

  subroutine resource_state_write (state, unit)
    class(resource_state_t), intent(in) :: state
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A)") "UNASSIGNED"
    call state%unassigned%write (u)
    write (u, "(A)") "STARTED"
    call state%started%write (u)
    write (u, "(A)") "FINISHED"
    call state%finished%write (u)
  end subroutine resource_state_write

  subroutine resource_state_init (state)
    class(resource_state_t), intent(out) :: state
    call state%unassigned%init ()
    call state%started%init ()
    call state%finished%init ()
  end subroutine resource_state_init

  subroutine resource_state_add_resource (state, i_resource)
    class(resource_state_t), intent(inout) :: state
    integer, intent(in) :: i_resource
    call state%unassigned%add (i_resource)
  end subroutine resource_state_add_resource

  subroutine resource_state_freeze (state)
    class(resource_state_t), intent(inout) :: state
    call state%unassigned%sort ()
    call state%unassigned%reverse_order ()
  end subroutine resource_state_freeze

  pure function resource_state_has_unassigned_resource (state) result (flag)
    class(resource_state_t), intent(in) :: state
    logical :: flag
    flag = .not. state%unassigned%is_empty ()
  end function resource_state_has_unassigned_resource

  function resource_state_start_resource (state) result (i_resource)
    class(resource_state_t), intent(inout) :: state
    integer :: i_resource
    if (.not. state%has_unassigned_resource ()) then
       i_resource = 0
       return
    end if
    i_resource = state%unassigned%remove ()
    call state%started%add (i_resource)
  end function resource_state_start_resource

  pure function resource_state_has_started_resource (state) result (flag)
    class(resource_state_t), intent(in) :: state
    logical :: flag
    flag = .not. state%started%is_empty ()
  end function resource_state_has_started_resource

  pure function resource_state_is_started_resource (state, i_resource) result (flag)
    class(resource_state_t), intent(in) :: state
    integer, intent(in) :: i_resource
    logical :: flag
    flag = state%started%is_element (i_resource)
  end function resource_state_is_started_resource

  subroutine resource_state_finish_resource (state, i_resource)
    class(resource_state_t), intent(inout) :: state
    integer, intent(in) :: i_resource
    integer :: ndx, element !! INDEX
    if (.not. state%has_started_resource ()) return
    ndx = state%started%find (i_resource)
    if (ndx > 0) then
       !! ATOMIC BEGIN
       element = state%started%remove_at (ndx)
       call state%finished%add (element)
       !! ATOMIC END
    else
       write (msg_buffer, "(A,1X,I3,1X,A)") "RESOURCE", i_resource, "NOT FOUND IN STARTED."
       call msg_bug ()
    end if
  end subroutine resource_state_finish_resource

  pure function resource_state_is_finished_resource (state, i_resource) result (flag)
    class(resource_state_t), intent(in) :: state
    integer, intent(in) :: i_resource
    logical :: flag
    flag = state%finished%is_element (i_resource)
  end function resource_state_is_finished_resource

  elemental function resource_state_all_finished_resources (state) result (flag)
    class(resource_state_t), intent(in) :: state
    logical :: flag
    flag = state%unassigned%is_empty () .and. state%started%is_empty ()
  end function resource_state_all_finished_resources
end module resources

