module channel_balancer
  use kinds, only: default
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT

  use balancer_base

  use diagnostics

  implicit none

  private

  real(default), parameter :: BETA = 1.5_default

  integer, parameter :: N_CHANNEL_BALANCER_STATE = 2, &
       CHANNEL_STATE = 1, &
       GRID_STATE = 2

  type, extends(balancer_base_t) :: channel_balancer_t
     private
     integer :: n_parallel_grids = 0
     integer :: n_parallel_channels = 0
     integer :: n_grid_workers = 0
     integer :: n_channel_workers = 0
     logical, dimension(:), allocatable :: parallel_grid
   contains
     procedure :: init => channel_balancer_init
     procedure :: write => channel_balancer_write
     procedure :: update_state => channel_balancer_update_state
     procedure :: has_resource_group => channel_balancer_has_resource_group
     procedure :: get_resource_group => channel_balancer_get_resource_group
     procedure :: get_resource_master => channel_balancer_get_resource_master
     procedure :: assign_worker => channel_balancer_assign_worker
     procedure :: free_worker => channel_balancer_free_worker
  end type channel_balancer_t

  public :: channel_balancer_t
contains
  subroutine channel_balancer_init (balancer, n_workers, n_resources)
    class(channel_balancer_t), intent(out), target :: balancer
    integer, intent(in) :: n_workers
    integer, intent(in) :: n_resources
    call balancer%base_init (n_workers, n_resources)
    allocate (balancer%parallel_grid(n_resources), source = .false.)
  end subroutine channel_balancer_init

  subroutine channel_balancer_write (balancer, unit)
    class(channel_balancer_t), intent(in) :: balancer
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A)") "Channel Balancer."
    write (u, "(A,1X,I3)") "Parallel grids: ", balancer%n_parallel_grids
    write (u, "(A,1X,I3)") "Parallel channels: ", balancer%n_parallel_channels
    write (u, "(A,1X,I3)") "Grid workers: ", balancer%n_grid_workers
    write (u, "(A,1X,I3)") "Channel workers: ", balancer%n_channel_workers
    write (u, *) balancer%parallel_grid
    call balancer%base_write (u)
  end subroutine channel_balancer_write

  subroutine channel_balancer_update_state (balancer, weight, parallel_grid)
    class(channel_balancer_t), intent(inout) :: balancer
    real(default), dimension(:), intent(in) :: weight
    logical, dimension(:), intent(in) :: parallel_grid
    real(default) :: min_parallel_weight
    balancer%parallel_grid = parallel_grid
    min_parallel_weight = balancer%n_resources**(1._default - 1_default / BETA) &
         / balancer%n_workers**BETA
    balancer%parallel_grid = &
         balancer%parallel_grid .and. (weight >= min_parallel_weight)
    if (balancer%n_resources >= balancer%n_workers) then
       !! Apply full multi-channel parallelization.
       balancer%n_parallel_grids = 0
       balancer%n_parallel_channels = balancer%n_resources
       balancer%parallel_grid = .false.
       balancer%n_grid_workers = 0
       balancer%n_channel_workers = balancer%n_workers
    else
       if (count (balancer%parallel_grid) == balancer%n_resources) then
          !! Apply full VEGAS parallelization.
          balancer%n_parallel_grids = balancer%n_resources
          balancer%n_parallel_channels = 0
          balancer%n_grid_workers = balancer%n_workers
          balancer%n_channel_workers = 0
       else
          !! Apply mixed mode.
          balancer%n_parallel_grids = count (balancer%parallel_grid)
          balancer%n_parallel_channels = balancer%n_resources - balancer%n_parallel_grids
          call compute_mixed_mode (weight)
       end if
    end if
    if(allocated (balancer%state)) then
       deallocate (balancer%state)
    end if
    call allocate_state ()
  contains
    subroutine compute_mixed_mode (weight)
      real(default), dimension(:), intent(in) :: weight
      real(default) :: weight_parallel_grids, &
           ratio_weight, &
           ratio_n_channels, &
           ratio
      !! Apply mixed mode.
      weight_parallel_grids = sum (weight, balancer%parallel_grid)
      !! Overall normalization of weight, \f$\alpha_{\text{grids}} +
      !! \alpha_{\text{channels}} = 1\f$.
      !! \f$\alpha_{\text{channels}} = 1 - \alpha_{\text{grids}}\f$
      ratio_weight = weight_parallel_grids / (1 - weight_parallel_grids)
      ratio_n_channels = real (balancer%n_parallel_grids, default) &
           / (balancer%n_resources - balancer%n_parallel_grids)
      !! The average computation of channel is proportional to its weight.
      !! Reweight number of channels (per mode) by their summed weights.
      !! R = w * N / (w * N + w' * N'); primed refer to parallel grid entities.
      !!   = 1 / (1 + w' / w * N' / N)
      ratio = 1 / (1  + ratio_weight * ratio_n_channels)
      ratio = min (max (ratio, 0.0_default), 1.0_default) !! Safe-guard ratio computation.
      !! In the case of small numbers of workers and a very small ratio,
      !! nint can assign no worker to channel/grid parallelization,
      !! which is still requested by n_parallel_channels/grids.
      !! In that case, we have to enforce: n_worker = n_channel_worker + n_grid_worker
      balancer%n_channel_workers = nint (ratio * balancer%n_workers)
      balancer%n_grid_workers = nint ((1 - ratio) * balancer%n_workers)
      !! In the case of small numbers of workers and a very small ratio,
      !! nint can assign no worker to channel/grid parallelization,
      !! which is still requested by n_parallel_channels/grids.
      !! In that case, we have to enforce: n_worker = n_channel_worker + n_grid_worker
      if (balancer%n_workers >= 2 &
           .AND. (balancer%n_parallel_channels > 0 .and. balancer%n_channel_workers < 1)) then
         balancer%n_channel_workers = 1
         balancer%n_grid_workers = balancer%n_grid_workers - 1
      end if
      !! The grid resources will only be increased to N = 2
      !! if more than 3 workers are present.
      if (balancer%n_workers >= 3 &
           .AND. (balancer%n_parallel_grids > 0 .and. balancer%n_grid_workers < 2)) then
         balancer%n_grid_workers = 2
         balancer%n_channel_workers = balancer%n_channel_workers - 2
      end if
    end subroutine compute_mixed_mode

    subroutine allocate_state ()
      type(resource_state_t), dimension(:), allocatable :: state
      integer :: ch
      allocate (state(N_CHANNEL_BALANCER_STATE))
      call state(CHANNEL_STATE)%init ( &
           mode = STATE_SINGLE, &
           n_workers = balancer%n_channel_workers)
      call state(GRID_STATE)%init ( &
           mode = STATE_ALL, &
           n_workers = balancer%n_grid_workers)
      do ch = 1, balancer%n_resources
         if (balancer%parallel_grid(ch)) then
            call state(GRID_STATE)%add_resource (ch)
         else
            call state(CHANNEL_STATE)%add_resource (ch)
         end if
      end do
      call balancer%add_state (state)
    end subroutine allocate_state
  end subroutine channel_balancer_update_state

  pure function channel_balancer_has_resource_group (balancer, resource_id) &
       result (flag)
    class(channel_balancer_t), intent(in) :: balancer
    integer, intent(in) :: resource_id
    logical :: flag
    flag = balancer%parallel_grid(resource_id)
  end function channel_balancer_has_resource_group

  pure subroutine channel_balancer_get_resource_group (balancer, resource_id, group)
    class(channel_balancer_t), intent(in) :: balancer
    integer, intent(in) :: resource_id
    integer, dimension(:), allocatable, intent(out) :: group
    integer :: i
    if (.not. balancer%has_resource_group (resource_id)) return
    group = pack ([(i, i = 1, balancer%n_workers)], &
         mask = balancer%worker%resource == resource_id)
  end subroutine channel_balancer_get_resource_group

  pure integer function channel_balancer_get_resource_master (balancer, resource_id) &
       result (worker_id)
    class(channel_balancer_t), intent(in) :: balancer
    integer, intent(in) :: resource_id
    integer :: i
    !! Linear search.
    !! First element in worker group is defined as master.
    associate (worker => balancer%worker)
      do i = 1, balancer%n_workers
         if (worker(i)%resource == resource_id) then
            worker_id = i
            exit
         end if
      end do
    end associate
  end function channel_balancer_get_resource_master

  subroutine channel_balancer_assign_worker (balancer, worker_id, resource_id)
    class(channel_balancer_t), intent(inout) :: balancer
    integer, intent(in) :: worker_id
    integer, intent(out) :: resource_id
    integer :: i_state
    if (balancer%worker(worker_id)%assigned) then
       resource_id = balancer%worker(worker_id)%resource
       return
    end if
    associate (state => balancer%state)
      i_state = balancer%worker(worker_id)%partition
      if (.not. state(i_state)%has_resource ()) then
         resource_id = 0
         return
      end if
      resource_id = state(i_state)%assign_resource ()
      select case (state(i_state)%mode)
      case (STATE_SINGLE)
         call balancer%worker(worker_id)%add_resource (resource_id)
         call balancer%resource(resource_id)%set_active (n_workers = 1)
      case (STATE_ALL)
         call fill_resource_group (i_state, resource_id)
      end select
    end associate
  contains
    subroutine fill_resource_group (i_state, resource_id)
      integer, intent(in) :: i_state
      integer, intent(in) :: resource_id
      integer :: i, n_workers
      n_workers = 0
      do i = 1, balancer%n_workers
         if (.not. balancer%worker(i)%partition == i_state) cycle
         if (balancer%is_worker_pending (i)) then
            write (msg_buffer, "(A,1X,I0,1X,A,1X,I0,1X,A)") "WORKER", i, "STATE", i_state, "ASSIGNED"
            call msg_bug ()
         end if
         call balancer%worker(i)%add_resource (resource_id)
         n_workers = n_workers + 1
      end do
      if (n_workers /= balancer%state(i_state)%n_workers) then
         call msg_bug ("Number of assigned workers unequal to number of state workers.")
      end if
      call balancer%resource(resource_id)%set_active (n_workers = n_workers)
    end subroutine fill_resource_group
  end subroutine channel_balancer_assign_worker

  subroutine channel_balancer_free_worker (balancer, worker_id)
    class(channel_balancer_t), intent(inout) :: balancer
    integer, intent(in) :: worker_id
    integer :: i, i_state, resource_id
    if (.not. balancer%worker(worker_id)%assigned) return
    associate (state => balancer%state)
      i_state = balancer%worker(worker_id)%partition
      resource_id = balancer%worker(worker_id)%resource
      call balancer%resource(resource_id)%set_inactive ()
      call state(i_state)%free_resource (resource_id)
      select case (state(i_state)%mode)
      case (STATE_SINGLE)
         call balancer%worker(worker_id)%free ()
      case (STATE_ALL)
         do i = 1, balancer%n_workers
            if (.not. balancer%worker(i)%partition == i_state) cycle
            call balancer%worker(i)%free ()
         end do
      end select
    end associate
  end subroutine channel_balancer_free_worker
end module channel_balancer
