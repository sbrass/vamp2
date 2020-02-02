!> Setup of request caller and add channel balancer.
program main
  use iso_fortran_env, only: ERROR_UNIT, &
       r64 => REAL64

  use request_caller
  use request_balancer
  use request_callback, only: request_handler_t
  use channel_balancer
  use result_handler

  use mpi_f08

  implicit none

  type(request_caller_t) :: caller
  type(request_t) :: request

  integer, parameter :: N_CHANNELS = 10

  integer :: i, request_rank, n_workers

  type(result_t), dimension(N_CHANNELS), target :: result
  real(r64), dimension(N_CHANNELS) :: channel_weight = 0
  logical, dimension(N_CHANNELS) :: parallel_grid = .false.

  call MPI_INIT ()

  !! Retrieve resource weight from n_box_parallel and channel weights.
  !! That's our prior allowing different resources to be greedy.
  channel_weight(1) = 4
  channel_weight(2) = 1
  channel_weight(3) = 10
  channel_weight(4) = 1
  channel_weight(5) = 1
  channel_weight(6) = 3
  channel_weight(7) = 1
  channel_weight(8) = 1
  channel_weight(9) = 5
  channel_weight(10) = 1
  parallel_grid = channel_weight > 1
  channel_weight = channel_weight / sum (channel_weight)

  call MPI_COMM_SIZE (MPI_COMM_WORLD, n_workers)
  call caller%init (MPI_COMM_WORLD)
  if (caller%is_master ()) then
     call add_channel_balancer ()
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
  end if
  call MPI_FINALIZE ()
contains
  subroutine add_channel_balancer ()
    class(request_balancer_t), allocatable :: balancer
    allocate (channel_balancer_t :: balancer)
    select type (balancer)
    type is (channel_balancer_t)
       call balancer%init (n_workers - 1, N_CHANNELS)
       call balancer%add_parallel_grid (parallel_grid)
       call balancer%add_channel_weight (channel_weight)
       call balancer%write (ERROR_UNIT)
    end select
    call caller%add_balancer (balancer)
  end subroutine add_channel_balancer

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
