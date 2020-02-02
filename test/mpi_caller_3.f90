!> Setup of request caller, add load balancer and add handler.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use request_caller
  use request_balancer
  use request_callback, only: request_handler_t
  use result_handler

  use mpi_f08

  implicit none

  type(request_caller_t) :: caller
  type(request_t) :: request

  integer, parameter :: N_CHANNELS = 10

  integer :: i, request_rank, n_workers

  type(result_t), dimension(N_CHANNELS), target :: result

  call MPI_INIT ()

  call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
  call caller%init (MPI_COMM_WORLD)
  if (caller%is_master ()) then
     call add_balancer ()
     do i = 1, N_CHANNELS
        call add_handler (caller, i, result(i))
     end do
     call caller%handle_workload ()
     call caller%write (ERROR_UNIT)
  else
     do
        call caller%request_workload (request)
        if (request%terminate) exit
        !! Do some nasty work.
        if (request%group) then
           call MPI_COMM_RANK (request%comm, request_rank)
        else
          request_rank = 0
        end if
        if (request_rank == 0) then
           !! Provide callback for result.
           call add_handler (caller, request%handler_id, result(request%handler_id))
           call caller%handler_and_release_workload (request)
        else
           call caller%release_workload (request)
        end if
     end do
     call caller%write (ERROR_UNIT)
  end if
  call MPI_FINALIZE ()
contains
  subroutine add_balancer ()
    class(request_balancer_t), allocatable :: balancer
    allocate (balancer)
    call balancer%init (n_workers - 1, N_CHANNELS)
    call caller%add_balancer (balancer)
  end subroutine add_balancer

  subroutine add_handler (caller, handler_id, result)
    type(request_caller_t), intent(inout) :: caller
    integer, intent(in) :: handler_id
    type(result_t), intent(in), target :: result
    class(request_handler_t), pointer :: handler
    allocate (result_handler_t :: handler)
    select type (handler)
    type is (result_handler_t)
       call handler%init (result, result%get_n_requests ())
    end select
    call caller%add_handler (handler_id, handler)
  end subroutine add_handler
end program main
