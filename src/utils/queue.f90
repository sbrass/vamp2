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

module queue
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  implicit none

  private

  integer, parameter :: QUEUE_SIZE = 10, &
       QUEUE_START = 0, &
       QUEUE_END = QUEUE_SIZE

  type :: queue_t
     private
     integer, dimension(QUEUE_SIZE) :: item
     integer :: front = 0
     integer :: rear = 0
   contains
     procedure :: enqueue => queue_enqueue
     procedure :: dequeue => queue_dequeue
     procedure :: is_full => queue_is_full
     procedure :: is_empty => queue_is_empty
     procedure :: peek => queue_peek
     procedure :: write => queue_write
  end type queue_t

  public :: queue_t
contains
  elemental logical function queue_is_full (queue) result (flag)
    class(queue_t), intent(in) :: queue
    flag = queue%front == 1 .and. queue%rear == QUEUE_END
  end function queue_is_full

  elemental logical function queue_is_empty (queue) result (flag)
    class(queue_t), intent(in) :: queue
    flag = queue%front == QUEUE_START
  end function queue_is_empty

  subroutine queue_enqueue (queue, item)
    class(queue_t), intent(inout) :: queue
    integer, intent(in) :: item
    if (queue%is_full ()) then
       !! Do something. 
    else
       if (queue%front == QUEUE_START) queue%front = 1
       queue%rear = queue%rear + 1
       queue%item(queue%rear) = item
    end if
  end subroutine queue_enqueue

  integer function queue_dequeue (queue) result (item)
    class(queue_t), intent(inout) :: queue
    if (queue%is_empty ()) then
       item = 0
    else
       item = queue%item(queue%front)
       if (queue%front >= queue%rear) then
          queue%front = QUEUE_START
          queue%rear = QUEUE_START
          !! Q has only one element,
          !! so we reset the queue after deleting it.
       else
          queue%front = queue%front + 1
       end if
    end if
  end function queue_dequeue

  integer function queue_peek (queue) result (item)
    class(queue_t), intent(in) :: queue
    if (queue%is_empty ()) then
       item = 0
    else
       item = queue%item(queue%front)
    end if
  end function queue_peek

  subroutine queue_write (queue, unit)
    class(queue_t), intent(in) :: queue
    integer, intent(in), optional :: unit
    integer :: u, i
    u = ERROR_UNIT; if (present (unit)) u = unit
    if (queue%is_empty ()) then
       write (u, *) "Empty Queue."
    else
       write (u, *) "Front →", queue%front
       write (u, *) "Items →"
       do i = 1, queue%rear
          write (u, *) queue%item(i)
       end do
       write (u, *) "Rear →", queue%rear
    end if
  end subroutine queue_write
end module queue
