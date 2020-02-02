!> /file signal.f90
module signal
#include "c_signal.h"
  use, intrinsic :: iso_c_binding
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  implicit none

  private

  !! We expose our own signal definitions using c_signal.h.
  !! Our definitions are then converted in signal.c.
  integer, public, parameter :: SIGINT = C_SIGINT

  interface
     subroutine c_signal (sig, c_handler) bind (C)
       import :: c_int
       integer(c_int), intent(in), value :: sig
       interface
          subroutine c_handler (sig) bind (C)
            import :: c_int
            integer(c_int), intent(in), value :: sig
          end subroutine c_handler
       end interface
     end subroutine c_signal

     function c_raise (sig) bind (C) result (status)
       import :: c_int
       integer(c_int), intent(in), value :: sig
       integer(c_int) :: status
     end function c_raise

     function c_getpid () bind(C, name="getpid") result (pid)
       import :: c_int
       integer(c_int) :: pid
     end function c_getpid

     function c_sleep (seconds) bind(C, name="sleep") result (seconds_left)
       import :: c_int
       integer(c_int), intent(in), value :: seconds
       integer(c_int) :: seconds_left
     end function c_sleep

     integer(c_int) function c_gethostname (name, len) bind(C, name="gethostname") result (status)
       import :: c_int, c_char, c_size_t
       character(kind=c_char), dimension(*) :: name
       integer(kind=c_size_t), value :: len
     end function c_gethostname
  end interface

  public :: signal_signal, &
       signal_raise, &
       signal_getpid, &
       signal_sleep, &
       signal_get_hostname, &
       c_signal_handler_interrupt, &
       signal_print_pid_and_wait
contains
  !! We expose c_signal through a fortran procedure without the necessity to expose C types to our infrastructure.
  subroutine signal_signal (sig, handler)
    integer, intent(in) :: sig
    interface
       subroutine handler (sig) bind (C)
         import :: c_int
         integer(c_int), intent(in), value :: sig
       end subroutine handler
    end interface
    integer(c_int) :: c_sig
    c_sig = sig
    call c_signal (c_sig, handler)
  end subroutine signal_signal

  !! We expose c_raise through a fortran procedure without the necessity to expose C types to our infrastructure.
  !! Furthermore, raising of a signal more a kind of a subroutine than a function.
  !! We handle possible appearing error statuses locally.
  subroutine signal_raise (sig)
    integer, intent(in) :: sig
    integer(c_int) :: c_status
    integer(c_int) :: c_sig
    c_sig = sig
    c_status = c_raise (c_sig)
    if (c_status /= 0) then
       write (ERROR_UNIT, "(A,1X,I3)") "raise () returned status", c_status
    end if
  end subroutine signal_raise

  subroutine signal_sleep (seconds)
    integer, intent(in) :: seconds
    integer(c_int) :: c_seconds
    c_seconds = seconds
    c_seconds = c_sleep (c_seconds)
    if (c_seconds /= 0) then
       write (ERROR_UNIT, "(A,1X,I12)") "Sleep ended earlier", c_seconds
    end if
  end subroutine signal_sleep

  integer function signal_getpid () result (pid)
    pid = c_getpid ()
  end function signal_getpid

  subroutine signal_get_hostname (hostname)
    character(:), allocatable, intent(out) :: hostname
    character(len=256,kind=c_char) :: c_hostname
    integer(c_size_t) :: c_size
    integer(c_int) :: c_status
    c_hostname = ""
    c_size = 0
    c_status = c_gethostname (c_hostname, c_size)
    hostname = trim (c_hostname)
  end subroutine signal_get_hostname

  subroutine c_signal_handler_interrupt (signal) bind (C)
    integer(c_int), intent(in), value :: signal
    !! Get pid and print it.
    integer(c_int) :: pid
    write (ERROR_UNIT, "(A)") "HANDLER: SIGNAL INTERRUPT"
    write (ERROR_UNIT, "(A,1X,I2)") "Raised signal", signal
    pid = getpid ()
    write (ERROR_UNIT, "(A,1X,I8)") "Current PID", pid
    flush (ERROR_UNIT)
  end subroutine c_signal_handler_interrupt

  subroutine signal_print_pid_and_wait ()
    integer :: i, pid
    character(:), allocatable :: hostname
    call signal_get_hostname (hostname)
    pid = signal_getpid ()
    write (ERROR_UNIT, "(A,1X,I8,3(1X,A))") "PID", pid, "on", hostname, "ready for attach..."
    flush (ERROR_UNIT)
    i = 0
    do while (0 == i)
       call sleep (5)
    end do
  end subroutine signal_print_pid_and_wait
end module signal
