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

module diagnostics
  use, intrinsic :: iso_fortran_env, only: output_unit, error_unit
  use kinds, only: default
  use io_units

  ! use system_defs, only: BUFFER_SIZE, MAX_ERRORS

  implicit none
  private

  public :: msg_buffer
  public :: msg_terminate
  public :: msg_bug, msg_fatal, msg_error, msg_warning
  public :: msg_message

  integer, parameter :: TERMINATE=-2, BUG=-1, FATAL=1, &
       ERROR=2, WARNING=3, MESSAGE=4, RESULT=5, &
       DEBUG=6, DEBUG2=7

  integer, parameter :: BUFFER_SIZE = 512

  character(len=BUFFER_SIZE), save :: msg_buffer = " " 

contains
  subroutine message_print (level, string, unit)
    integer, intent(in) :: level
    character(len=*), intent(in), optional :: string
    integer, intent(in), optional :: unit
    character(:), allocatable :: prep, aux, head_footer
    logical :: severe, is_error
    integer :: u
    severe = .false.
    head_footer  = "******************************************************************************"
    prep = ""
    aux = ""
    is_error = .false.
    select case (level)
    case (TERMINATE)
       prep = ""
    case (BUG)
       prep   = "*** BUG: "
       aux    = "***              "
       severe = .true.
       is_error = .true.
    case (FATAL)
       prep   = "*** FATAL ERROR: "
       aux    = "***              "
       severe = .true.
       is_error = .true.
    case (ERROR)
       prep = "*** ERROR: "
       aux  = "***        "
       is_error = .true.
    case (WARNING)
       prep = "Warning: "
    case (MESSAGE)
       prep = "| "
    case default
       prep = ""
    end select
    if (present(string))  msg_buffer = string
    u = output_unit; if (present(unit)) u = unit
    if (severe) write (u, "(A)") head_footer
    if (is_error) write (u, "(A)") head_footer
    write (u, "(A,A,A)") prep, trim(msg_buffer)
    if (is_error) write (u, "(A)") head_footer
    if (severe) write (u, "(A)") head_footer
    flush (u)
    msg_buffer = " "
  end subroutine message_print

  subroutine msg_terminate (string, unit, quit_code)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    integer, intent(in), optional :: quit_code
    ! integer(c_int) :: return_code
    integer :: return_code
    ! call release_term_signals ()
    if (present (quit_code)) then
       return_code = quit_code
    else
       return_code = 0
    end if
    if (present (string)) &
         call message_print (MESSAGE, string, unit=unit)
    stop 1
    ! if (return_code /= 0) then
    !    call exit (return_code)
    ! else
    !    !!! Should implement WHIZARD exit code (currently only via C)
    !    call exit (0)
    ! end if
  end subroutine msg_terminate

  subroutine msg_bug (string, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    call message_print (BUG, string, unit = unit)
    call msg_terminate ()
    ! call message_print (TERMINATE, "Abort run...", unit=unit)
    ! stop "Abort run..."
  end subroutine msg_bug

  recursive subroutine msg_fatal (string, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    call message_print (FATAL, string, unit)
    call msg_terminate ()
  end subroutine msg_fatal

  subroutine msg_error (string, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    call message_print (ERROR, string, unit)
    call msg_fatal ("HARD ERROR HANDLING.")
  end subroutine msg_error

  subroutine msg_warning (string, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    call message_print (level = WARNING, string = string, unit = unit)
  end subroutine msg_warning

  subroutine msg_message (string, unit)
    integer, intent(in), optional :: unit
    character(len=*), intent(in), optional :: string
    call message_print (level = MESSAGE, &
       string = string, unit = unit)
  end subroutine msg_message
end module diagnostics
