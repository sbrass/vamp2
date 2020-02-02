module test_utils
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  implicit none

  private

  public :: commandline_is_gdb_attach
contains
  logical function commandline_is_gdb_attach () result (flag)
    integer :: i
    character(len=256) :: arg
    flag = .false.
    do i = 1, command_argument_count ()
       call get_command_argument(i, arg)
       select case (trim(arg))
       case ("--gdb-attach", "-g")
          flag = .true.
       case ("--help", "-h")
          call print_help ()
       end select
    end do
  contains
    subroutine print_help ()
      write (ERROR_UNIT, "(A)") "--gdb-attach (-g): Halt program and print PID."
      write (ERROR_UNIT, "(A)") "--help (-h): Print this help message."
    end subroutine print_help
  end function commandline_is_gdb_attach
end module test_utils
