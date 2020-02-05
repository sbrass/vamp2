module iterator
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  implicit none

  private

  !! Forward
  type :: iterator_t
     integer :: current = 0
     integer :: begin = 0
     integer :: end = 0
     integer :: step = 1
   contains
     procedure :: write => iterator_write
     procedure :: init => iterator_init
     procedure :: at_begin => iterator_at_begin
     procedure :: at_end => iterator_at_end
     procedure :: is_iterable => iterator_is_iterable
     procedure :: next => iterator_next
     procedure :: next_step => iterator_next_step
     procedure :: get_current => iterator_get_current
  end type iterator_t

  public :: iterator_t
contains
  subroutine iterator_write (iter, unit)
    class(iterator_t), intent(in) :: iter
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(3(A,1X,I3,1X))") "CURRENT", iter%current, &
         "BEGIN", iter%begin, "END", iter%end
    flush (u)
  end subroutine iterator_write

  !! Proof: step > 0, begin < end.
  !! Proof: step < 0, begin > end.
  !! Proof: step /= 0.
  subroutine iterator_init (iter, begin, end, step)
    class(iterator_t), intent(inout) :: iter
    integer, intent(in) :: begin
    integer, intent(in) :: end
    integer, intent(in), optional :: step
    iter%begin = begin
    iter%end = end
    iter%step = 1; if (present (step)) iter%step = step
    if (abs (iter%step) > 0) then
       iter%current = iter%begin
    else
       write (ERROR_UNIT, "(A)") "ERROR: Step size MUST be unequal to zero."
       stop 1
    end if
  end subroutine iterator_init

  pure function iterator_at_begin (iter) result (flag)
    class(iterator_t), intent(in) :: iter
    logical :: flag
    flag = iter%current == iter%begin
  end function iterator_at_begin

  pure function iterator_at_end (iter) result (flag)
    class(iterator_t), intent(in) :: iter
    logical :: flag
    flag = iter%current == iter%end
  end function iterator_at_end

  !! Proof: begin < current < end
  pure function iterator_is_iterable (iter) result (flag)
    class(iterator_t), intent(in) :: iter
    logical :: flag
    if (iter%step > 0) then
       flag = iter%current <= iter%end
    else if (iter%step < 0) then
       flag = iter%current >= iter%end
    else
       flag = .false.
    end if
  end function iterator_is_iterable

  subroutine iterator_next_step (iter)
    class(iterator_t), intent(inout) :: iter
    if (.not. iter%is_iterable ()) return
    iter%current = iter%current + iter%step
  end subroutine iterator_next_step

  !! Proof: begin ≤ current ≤ end.
  !! However, after applying the step, this does not need to be true..
  function iterator_next (iter) result (ndx)
    class(iterator_t), intent(inout) :: iter
    integer :: ndx
    if (.not. iter%is_iterable ()) then
       ndx = 0
       return
    end if
    ndx = iter%current
    iter%current = iter%current + iter%step
  end function iterator_next

  pure function iterator_get_current (iter) result (ndx)
    class(iterator_t), intent(in) :: iter
    integer :: ndx
    if (.not. iter%is_iterable ()) then
       ndx = 0
       return
    end if
    ndx = iter%current
  end function iterator_get_current
end module iterator

