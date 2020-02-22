module test_utils
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  use diagnostics
  use iterator

  implicit none

  private

!! Example: --gdb-attach=[rank], -g=[rank]

  type :: commandline_t
     logical :: gdb_attach = .false.
     integer :: gdb_attach_rank = 0
     character(:), allocatable :: method
   contains
     procedure :: write => commandline_write
     procedure :: parse => commandline_parse
  end type commandline_t

  public :: commandline_is_gdb_attach, commandline_t
contains
  subroutine commandline_write (cmd, unit)
    class(commandline_t), intent(in) :: cmd
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (ERROR_UNIT, "(A,1X,L1)") "GDB_ATTACH", cmd%gdb_attach
    write (ERROR_UNIT, "(A,1X,I0)") "GDB_ATTACH_RANK", cmd%gdb_attach_rank
  end subroutine commandline_write

  subroutine commandline_parse (cmd)
    class(commandline_t), intent(out) :: cmd
    type(iterator_t) :: cmd_iter
    integer :: arg_len, arg_status
    character(len=256) :: arg
    character(:), allocatable :: arg_value
    call cmd_iter%init (1, command_argument_count ())
    SCAN_CMD: do while (cmd_iter%is_iterable ())
       call get_command_argument (number = cmd_iter%get_current (), &
            value = arg, length = arg_len)
       select case (arg(:arg_len))
       case ("--gdb-attach", "-g")
          cmd%gdb_attach = .true.
          call cmd_iter%next_step ()
          if (cmd_iter%is_iterable ()) then
             call get_command_argument (number = cmd_iter%get_current (), &
                  value = arg, length = arg_len)
             select case (arg(1:1))
             case ("-")
                cycle SCAN_CMD
             end select
             read (arg(:arg_len), *) cmd%gdb_attach_rank
          end if
       case ("--method", "-m")
          call cmd_iter%next_step ()
          if (cmd_iter%is_iterable ()) then
             call get_command_argument (number = cmd_iter%get_current (), &
                  value = arg, length = arg_len)
             select case (arg(1:1))
             case ("-")
                cycle SCAN_CMD
             end select
             select case (arg(:arg_len))
             case ("simple", "Simple", "SIMPLE")
                cmd%method = "simple"
             case ("load", "Load", "LOAD")
                cmd%method = "load"
             end select
          end if
       end select
       call cmd_iter%next_step ()
    end do SCAN_CMD
    if (.not. allocated (cmd%method)) cmd%method = "simple"
  end subroutine commandline_parse

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
