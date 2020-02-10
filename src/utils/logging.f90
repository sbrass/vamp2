module logging
  use iso_fortran_env, only: error_unit, output_unit

  use mpi_f08

  implicit none

  private

  type :: logging_t
     character(:), allocatable :: filename
     character(:), allocatable :: msg
     integer :: unit
   contains
     procedure :: init => logging_init
     procedure :: clear => logging_clear
     procedure :: flush => logging_flush
     procedure :: append => logging_append
     procedure, private :: print => logging_print
     final :: logging_final
  end type logging_t

  character(len=256),  public, save :: LOG_BUFFER = ""
  public :: string
  public :: logging_t
contains

  function string (char_string) result (alloc_string)
    character(len=*), intent(in) :: char_string
    character(:), allocatable :: alloc_string
    alloc_string = trim (char_string)
  end function string

  subroutine logging_init (log, filename, unit, suffix)
    class(logging_t), intent(out) :: log
    character(:), allocatable, intent(in), optional :: filename
    integer, intent(in), optional :: unit
    character(:), allocatable, intent(in), optional :: suffix
    if (present (filename)) then
       log%filename = filename
       call uniquify_file (log%filename)
       if (present (suffix)) then
          log%filename = log%filename // suffix
       else
          log%filename = log%filename // ".log"
       end if
       write (OUTPUT_UNIT, "(A,1X,A)") "=> Logging to", log%filename
       call open_file (log%filename, log%unit)
    end if
  contains
    subroutine uniquify_file (filename)
      character(:), allocatable, intent(inout) :: filename
      integer :: rank
      character(len=15) :: ibuf
      call MPI_COMM_RANK (MPI_COMM_WORLD, rank)
      write (ibuf, "(I0)") rank
      filename = filename // "." // trim (ibuf)
    end subroutine uniquify_file

    subroutine open_file (filename, unit)
      character(len=*), intent(in) :: filename
      integer, intent(out) :: unit
      integer :: status
      character(:), allocatable :: msg
      open (newunit = unit, file = filename, action = "write", &
           status = "replace", iostat = status, iomsg = msg)
      if (status /= 0) then
         write (ERROR_UNIT, "(A)") trim (msg)
         stop 1
      end if
    end subroutine open_file
  end subroutine logging_init

  subroutine logging_clear (log)
    class(logging_t), intent(inout) :: log
    log%msg = ""
  end subroutine logging_clear

  subroutine logging_append (log, msg)
    class(logging_t), intent(inout) :: log
    character(:), allocatable, intent(in) :: msg
    call log%print ()
    log%msg = msg
  end subroutine logging_append

  subroutine logging_print (log)
    class(logging_t), intent(inout) :: log
    write (log%unit, "(A)") log%msg
    if (log%unit /= OUTPUT_UNIT) then
       write (OUTPUT_UNIT, "(A)") log%msg
    end if
    call log%clear ()
  end subroutine logging_print

  subroutine logging_flush (log)
    class(logging_t), intent(inout) :: log
    call log%print ()
    flush (log%unit)
  end subroutine logging_flush

  subroutine logging_final (log)
    type(logging_t), intent(inout) :: log
    integer :: status
    character(:), allocatable :: msg
    if (log%unit /= ERROR_UNIT &
         .and. log%unit /= OUTPUT_UNIT) then
       close (unit = log%unit, iostat = status, iomsg = msg)
       if (status /= 0) then
          write (ERROR_UNIT, "(A)") trim (msg)
          stop 1
       end if
    end if
  end subroutine logging_final
end module logging
