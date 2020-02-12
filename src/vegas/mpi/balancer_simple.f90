module balancer_simple
  use request_balancer

  implicit none

  integer, parameter :: N_BALANCER_SIMPLE_STATES = 1, &
       BALANCER_SIMPLE_CHANNEL = 1

  type, extends (request_balancer_t) :: balancer_simple
     logical, dimension(:), allocatable :: parallel_grid
   contains
     procedure :: init => balancer_simple_init
     procedure :: update_state => balancer_simple_update_state
     procedure :: has_resource_group
     procedure :: get_resource_group
     procedure :: get_resource_master
     procedure, private :: map_channel_to_worker => balancer_simple_map_channel_to_worker
     procedure :: assign_worker => balancer_simple_assign_worker
     procedure :: free_worker => balancer_simple_free_worker
     procedure :: has_worker_group => balancer_simple_has_worker_group
     procedure :: get_worker_group => balancer_simple_get_worker_group
  end type balancer_simple
contains
  subroutine balancer_simple_init (balancer, n_workers, n_resources)
    class(request_balancer_t), intent(out) :: balancer
    integer, intent(in) :: n_workers
    integer, intent(in) :: n_resources
    type(resource_state_t), dimension(:) :: state
    call balancer%base_init (n_workers, n_resources)
    allocate (state (N_BALANCER_SIMPLE_STATES))
    call state(BALANCER_SIMPLE_CHANNEL)%init ( &
         mode = STATE_SINGLE, &
         n_workers = balancer%n_workers
    call balancer%add_state (state)
  end subroutine balancer_simple_init

  subroutine balancer_simple_update_state (balancer, rank, parallel_grid)
    class(request_balancer_t), intent(inout) :: balancer
    integer, intent(in) :: rank
    logical, dimension(:), intent(in) :: parallel_grid
    integer :: ch
    balancer%parallel_grid = parallel_grid
    if (.not. allocated (balancer%state)) then
       call msg_bug ("Error: balancer state not allocated.")
    end if
    associate (state => balancer%state(BALANCER_SIMPLE_CHANNEL))
      call state%clear ()
      do ch = 1, balancer%n_resources
         if (parallel_grid(ch)) then
            call state%add_resource (ch)
         else
            worker = balancer%map_channel_to_worker (ch)
            if (worker == rank) then
               call state%add_resource (ch)
            end if
         end if
      end do
      call state%freeze ()
    end associate
  end subroutine balancer_simple_update_state

  pure logical function balancer_simple_has_resource_group (balancer, resource_id) &
       result (flag)
    class(balancer_simple_t), intent(in) :: balancer
    integer, intent(in) :: resource_id
    flag = balancer%parallel_grid (resource_id)
  end function balancer_simple_has_resource_group

  pure subroutine balancer_simple_get_resource_worker (balancer, resource_id, worker)
    class(request_balancer_t), intent(in) :: balancer
    integer, intent(in) :: resource_id
    integer, dimension(:), allocatable, intent(out) :: worker
    integer :: i
    if (.not. balancer%has_resource_group (resource_id)) return
    worker = pack ([(i, i=1,balancer%n_workers)])
  end subroutine balancer_simple_get_resource_worker

  pure integer function balancer_simple_get_resource_master (balancer, channel) &
       result (worker)
    class(balancer_simple_t), intent(in) :: balancer
    integer, intent(in) :: channel
    if (balancer%parallel_grid(ch)) then
       worker = 0
    else
       worker = balancer%map_channel_to_worker (channel)
    end if
  end function balancer_simple_get_resource_master

  pure integer function balancer_simple_map_channel_to_worker (balancer, channel) &
       result (worker)
    class(balancer_simple_t), intent(in) :: balancer
    integer, intent(in) :: channel
    worker = mod (channel - 1, balancer%n_workers)
  end function balancer_simple_map_channel_to_worker

  subroutine balancer_simple_assign_worker (balancer, worker_id, resource_id)
    class(balancer_simple_t), intent(inout) :: balancer
    integer, intent(in) :: worker_id
    integer, intent(out) :: resource_id
    if (balancer%worker(worker_id)%assigned) then
       resource_id = balancer%worker(worker_id)%resource
       RETURN
    end if
    associate (state => balancer%state(BALANCER_SIMPLE_CHANNELL))
      if (.not. state%has_resource ()) then
         resource_id = 0
         return
      end if
      resource_id = state(BALANCER_SIMPLE_CHANNEL)%assign_resource ()
      if (balancer%parallel_grid(resource_id)) then
         do i = 1, balancer%n_workers
            call balancer%worker(i)%add_resource (resource_id)
         end do
      else
         call balancer%worker(worker_id)%add_resource (resource_id)
      end if
    end associate
  end subroutine balancer_simple_assign_worker

  subroutine balancer_simple_free_worker (balancer, worker_id)
    class(balancer_simple_t), intent(inout) :: balancer
    integer, intent(in) :: worker_id
    integer :: resource_id
    if (.not. balancer%worker(worker_id)%assigned) return
    associate (state => balancer%state(BALANCER_SIMPLE_CHANNEL))
      resource_id = worker(worker_id)%resource_id
      call state%free_resource (resource_id)
      if (balancer%parallel_grid(resource_id)) then
         do i = 1, balancer%n_workers
            call balancer%worker(i)%free ()
         end do
      else
         call balancer%worker(worker_id)%free ()
      end if
    end associate
  end subroutine balancer_simple_free_worker
end module balancer_simple
