program main
  use iso_fortran_env, only: ERROR_UNIT
  use kinds, only: default
  use format_defs, only: FMT_12

  use rng_base
  use rng_stream

  use request_base

  use test_utils
  use signal
  use benchmark_func
  use cputime
  use diagnostics

  use vamp2

  use mpi_f08

  implicit none

  integer, parameter :: n_dim = 5, &
       n_channels = 100

  type(vamp2_t) :: mc
  class(rng_t), allocatable :: rng
  class(vamp2_func_t), allocatable :: func
  real(default), dimension(n_dim), parameter :: x_lower = 0, &
       x_upper = 1
  real(default) :: result, abserr

  type(timer_t) :: timer

  call MPI_INIT ()

  call identify_process ()
  call parse_cmdline ()
  call timer%start ()

  allocate (rng_stream_t :: rng)
  call rng%init ()

  allocate (benchmark_func_t :: func)
  call func%init (n_dim = n_dim, n_channel = n_channels)

  mc = vamp2_t (n_channel = n_channels, n_dim = n_dim)

  call mc%allocate_request (method = "load")

  call mc%set_limits (x_lower, x_upper)
  call mc%set_calls (10000)

  call mc%integrate (func, rng, 6, verbose = .true., result=result, abserr=abserr)
  write (ERROR_UNIT, "(A," // FMT_12 // ",A," // FMT_12 // ")") "Result:", result, "±", abserr

  call mc%set_calls (10000)
  call mc%integrate (func, rng, 4, refine_grids = .false., adapt_weights = .false., verbose = .true., &
       result=result, abserr=abserr)
  write (ERROR_UNIT, "(A," // FMT_12 // ",1X,A,1X," // FMT_12 // ")") "Result:", result, "±", abserr

  call timer%stop ()
  call timer%write ()

  call mc%final ()
  call rng%final ()

  call MPI_FINALIZE ()
contains
  subroutine identify_process ()
    integer :: rank
    call MPI_COMM_RANK (MPI_COMM_WORLD, rank)
    write (ERROR_UNIT, "(A,1X,I0,1X,A,1X,I0)") &
         "RANK", rank, "PID", signal_getpid ()
  end subroutine identify_process

  subroutine parse_cmdline ()
    integer :: n_workers, rank
    type(commandline_t) :: cmd
    call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
    call MPI_COMM_RANK (MPI_COMM_WORLD, rank)
    call cmd%parse ()
    if (cmd%gdb_attach) then
       if (cmd%gdb_attach_rank >= n_workers) &
            call msg_fatal ("Cannot attach to rank outside of communicator.")
       if (cmd%gdb_attach_rank == rank) then
          call signal_print_pid_and_wait ()
       end if
       call MPI_BARRIER (MPI_COMM_WORLD)
    end if
  end subroutine parse_cmdline
end program main
