module balancer_base
  use kinds, only: default
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use array_list

  use diagnostics

  implicit none

  private

  type :: worker_t
     integer :: resource = 0
     integer :: partition = 0
     integer :: n_resources = 0
     logical :: assigned = .false.
   contains
     procedure :: write => worker_write
     procedure :: add_resource => worker_add_resource
     procedure :: free => worker_free
  end type worker_t

  type :: resource_t
     private
     integer :: resource_id = 0
     logical :: active = .false.
     integer :: n_assigned_workers = 0
   contains
     procedure :: write => resource_write
     procedure :: is_active => resource_is_active
     procedure :: set_active => resource_set_active
     procedure :: set_inactive => resource_set_inactive
  end type resource_t

  integer, parameter, public :: STATE_SINGLE = 1, &
       STATE_ALL = 2

  type :: resource_state_t
     integer :: n_workers = 0
     integer :: mode = 0
     type(array_list_t) :: resource_stack
     type(array_list_t) :: finished_stack
   contains
     procedure :: write => resource_state_write
     procedure :: init => resource_state_init
     procedure :: add_resource => resource_state_add_resource
     procedure :: freeze => resource_state_freeze
     procedure :: clear => resource_state_clear
     procedure :: has_resource => resource_state_has_resource
     procedure :: assign_resource => resource_state_assign_resource
     procedure :: free_resource => resource_state_free_resource
  end type resource_state_t

  !> Dynamic load balancer.
  !!
  !! We organize resources and workers in a transparent way using indices.
  !! These indices replace pointer magic.
  !!
  !! The balancer aggregates a dynamic state, however, we allow the state by
  !! the use of a pointer, to access the static fields of the balancer.
  type, abstract :: balancer_base_t
     integer :: n_workers = 0
     integer :: n_resources = 0
     integer :: n_states = 0
     type(worker_t), dimension(:), allocatable :: worker
     type(resource_t), dimension(:), allocatable :: resource
     type(resource_state_t), dimension(:), allocatable :: state
   contains
     procedure :: base_write => balancer_base_write
     procedure(balancer_base_deferred_write), deferred :: write
     procedure :: base_init => balancer_base_base_init
     procedure :: add_state => balancer_base_add_state
     procedure :: link_worker_and_state => balancer_base_link_worker_and_state
     procedure :: is_assignable => balancer_base_is_assignable
     procedure :: is_worker_pending => balancer_base_is_worker_pending
     procedure :: is_pending => balancer_base_is_pending
     procedure(balancer_base_has_resource_group), deferred :: has_resource_group
     procedure(balancer_base_get_resource_group), deferred :: get_resource_group
     procedure(balancer_base_get_resource_master), deferred :: get_resource_master
     procedure(balancer_base_assign_worker), deferred :: assign_worker
     procedure(balancer_base_free_worker), deferred :: free_worker
  end type balancer_base_t

  abstract interface
     subroutine balancer_base_deferred_write (balancer, unit)
       import :: balancer_base_t
       class(balancer_base_t), intent(in) :: balancer
       integer, intent(in), optional :: unit
     end subroutine balancer_base_deferred_write

     !> Pure forbids any MPI intrusion!!
     pure logical function balancer_base_has_resource_group (balancer, resource_id) &
          result (flag)
       import :: balancer_base_t
       class(balancer_base_t), intent(in) :: balancer
       integer, intent(in) :: resource_id
     end function balancer_base_has_resource_group

     pure subroutine balancer_base_get_resource_group (balancer, resource_id, group)
       import :: balancer_base_t
       class(balancer_base_t), intent(in) :: balancer
       integer, intent(in) :: resource_id
       integer, dimension(:), allocatable, intent(out) :: group
     end subroutine balancer_base_get_resource_group

     pure integer function balancer_base_get_resource_master (balancer, resource_id) &
          result (worker)
       import :: balancer_base_t
       class(balancer_base_t), intent(in) :: balancer
       integer, intent(in) :: resource_id
     end function balancer_base_get_resource_master

     !> Assign resource to a given worker or retrieve current assigned resource.
     !!
     !! If worker has already a resource assigned, return resource.
     !! If worker has not assigned a resource, retrieve new resource from state.
     subroutine balancer_base_assign_worker (balancer, worker_id, resource_id)
       import :: balancer_base_t
       class(balancer_base_t), intent(inout) :: balancer
       integer, intent(in) :: worker_id
       integer, intent(out) :: resource_id
     end subroutine balancer_base_assign_worker

     !> Free assignment of worker.
     !!
     !! If worker is not assigned, this procedure is idempotent.
     !! If worker is assigned, alter state correspondingly.
     subroutine balancer_base_free_worker (balancer, worker_id)
       import :: balancer_base_t
       class(balancer_base_t), intent(inout) :: balancer
       integer, intent(in) :: worker_id
     end subroutine balancer_base_free_worker
  end interface

  public :: balancer_base_t, resource_state_t
contains
  subroutine worker_write (worker, unit)
    class(worker_t), intent(in) :: worker
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(3(A,1X,I3,1X),A,1X,L1)") "RESOURCE", worker%resource, &
         "PARTITION", worker%partition, &
         "N_RESOURCES", worker%n_resources, &
         "ASSIGNED", worker%assigned
  end subroutine worker_write

  elemental subroutine worker_add_resource (worker, resource_id)
    class(worker_t), intent(inout) :: worker
    integer, intent(in) :: resource_id
    worker%n_resources = worker%n_resources + 1
    worker%assigned = .true.
    worker%resource = resource_id
  end subroutine worker_add_resource

  elemental subroutine worker_free (worker)
    class(worker_t), intent(inout) :: worker
    worker%assigned = .false.
    worker%resource = 0
  end subroutine worker_free

  subroutine resource_write (resource, unit)
    class(resource_t), intent(in) :: resource
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A,1X,I3,1X,A,1X,L1,1X,A,1X,I3)") &
         "RESOURCE_ID", resource%resource_id, &
         "ACTIVE", resource%active, &
         "N_ASSIGNED_WORKERS", resource%n_assigned_workers
  end subroutine resource_write

  elemental function resource_is_active (resource) result (flag)
    class(resource_t), intent(in) :: resource
    flag = resource%active
  end function resource_is_active

  subroutine resource_set_active (resource, n_workers)
    class(resource_t), intent(inout) :: resource
    integer, intent(in) :: n_workers
    resource%active = .true.
    resource%n_assigned_workers = n_workers
  end subroutine resource_set_active

  subroutine resource_set_inactive (resource)
    class(resource_t), intent(inout) :: resource
    resource%active = .false.
  end subroutine resource_set_inactive

  subroutine resource_state_write (state, unit)
    class(resource_state_t), intent(in) :: state
    integer, intent(in), optional :: unit
    integer :: u
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A,1X,I0)") "N_STATE_WORKERS", state%n_workers
    select case (state%mode)
    case (STATE_SINGLE)
       write (u, "(A)") "MODE ONE-TO-ONE"
    case (STATE_ALL)
       write (u, "(A)") "MODE ALL-TO-ONE"
    case default
       write (u, "(A)") "UNSUPPORTED MODE"
    end select
    write (u, "(A)") "RESOURCE"
    call state%resource_stack%write (u)
    write (u, "(A)") "FINISHED"
    call state%finished_stack%write (u)
  end subroutine resource_state_write

  subroutine resource_state_init (state, mode, n_workers)
    class(resource_state_t), intent(out) :: state
    integer, intent(in) :: mode
    integer, intent(in) :: n_workers
    state%mode = mode
    state%n_workers = n_workers
    call state%resource_stack%init ()
    call state%finished_stack%init ()
  end subroutine resource_state_init

  subroutine resource_state_add_resource (state, i_resource)
    class(resource_state_t), intent(inout) :: state
    integer, intent(in) :: i_resource
    call state%resource_stack%add (i_resource)
  end subroutine resource_state_add_resource

  subroutine resource_state_freeze (state)
    class(resource_state_t), intent(inout) :: state
    call state%resource_stack%sort ()
    call state%resource_stack %reverse_order ()
  end subroutine resource_state_freeze

  subroutine resource_state_clear (state)
    class(resource_state_t), intent(inout) :: state
    call state%resource_stack%clear ()
    call state%finished_stack%clear ()
  end subroutine resource_state_clear

  elemental function resource_state_has_resource (state) result (flag)
    class(resource_state_t), intent(in) :: state
    logical :: flag
    flag = .not. state%resource_stack%is_empty ()
  end function resource_state_has_resource

  function resource_state_assign_resource (state) result (i_resource)
    class(resource_state_t), intent(inout) :: state
    integer :: i_resource
    if (state%resource_stack%is_empty ()) then
       i_resource = 0
       call msg_bug ("Error: No leftover resource on stack.")
       return
    end if
    i_resource = state%resource_stack%remove () !! Pop last element from stack.
  end function resource_state_assign_resource

  subroutine resource_state_free_resource (state, i_resource)
    class(resource_state_t), intent(inout) :: state
    integer, intent(in) :: i_resource
    if (state%resource_stack%is_element (i_resource)) then
       call msg_bug ("Error: Cannot free resource, still on resource stack.")
    end if
    call state%finished_stack%add (i_resource)
  end subroutine resource_state_free_resource

  subroutine balancer_base_write (balancer, unit)
    class(balancer_base_t), intent(in) :: balancer
    integer, intent(in), optional :: unit
    integer :: u, i
    u = ERROR_UNIT; if (present (unit)) u = unit
    write (u, "(A)") "[REQUEST BALANCER]"
    write (u, "(3(A,1X,I3,1X))") "N_WORKERS", balancer%n_workers, &
         "N_RESOURCES", balancer%n_resources, &
         "N_STATES", balancer%n_states
    write (u, "(A)") "[WORKER]"
    do i = 1, balancer%n_workers
       call balancer%worker(i)%write (u)
    end do
    write (u, "(A)") "[RESOURCE]"
    do i = 1, balancer%n_resources
       call balancer%resource(i)%write (u)
    end do
    write (u, "(A)") "[STATES]"
    do i = 1, balancer%n_states
       call balancer%state(i)%write (u)
    end do
  end subroutine balancer_base_write

  subroutine balancer_base_base_init (balancer, n_workers, n_resources)
    class(balancer_base_t), intent(out) :: balancer
    integer, intent(in) :: n_workers
    integer, intent(in) :: n_resources
    balancer%n_workers = n_workers
    balancer%n_resources = n_resources
    allocate (balancer%worker (n_workers))
    allocate (balancer%resource (n_resources))
    call init_resource ()
  contains
    subroutine init_resource ()
      integer :: i
      do i = 1, balancer%n_resources
         balancer%resource(i)%resource_id = i
      end do
    end subroutine init_resource
  end subroutine balancer_base_base_init

  !> Add partition of workers and link with workers.
  !! We move the allocated partition object into the balancer.
  !! We then assign each partition its respective number of workers in a incrementing linear fashion.
  !! However, we postpone the linking of the resources to the partition, which can be either done dynamically with the balancer state or directly with the appropriate type-bound procedure.
  subroutine balancer_base_add_state (balancer, state)
    class(balancer_base_t), intent(inout) :: balancer
    type(resource_state_t), dimension(:), allocatable, intent(inout) :: state
    balancer%n_states = size (state)
    call move_alloc (state, balancer%state)
    call balancer%link_worker_and_state ()
  end subroutine balancer_base_add_state

  subroutine balancer_base_link_worker_and_state (balancer)
    class(balancer_base_t), intent(inout) :: balancer
    integer :: i, j, i_worker
    if (.not. allocated (balancer%state)) &
         call msg_bug ("Error: resource state not allocated.")
    !! Link worker to a state.
    i_worker = 1
    do i = 1, balancer%n_states
       do j = 1, balancer%state(i)%n_workers
          if (i_worker > balancer%n_workers) then
             call msg_bug ("Balancer: Number of state workers&
                  & exceeding global number of workers")
          end if
          balancer%worker(i_worker)%partition = i
          i_worker = i_worker + 1
       end do
    end do
  end subroutine balancer_base_link_worker_and_state

  !> Is a worker and has he a assignable resource.
  !!
  !! The answer depends on two factors:
  !! (i) Is there still work in the associated partition?
  !! (ii) Is the worker already assigned? E.g. as part of a group and needs to retrieve its resources?
  !! Is either one of the cases true, the worker has an assignable resource.
  pure logical function balancer_base_is_assignable (balancer, worker_id) result (flag)
    class(balancer_base_t), intent(in) :: balancer
    integer, intent(in) :: worker_id
    integer :: partition_id
    partition_id = balancer%worker(worker_id)%partition
    flag = balancer%worker(worker_id)%assigned .or. &
         balancer%state(partition_id)%has_resource ()
  end function balancer_base_is_assignable

  !> Is a worker still pending.
  !!
  !! Test worker assignment, and if there is a (valid) resource and if it is still active.
  pure logical function balancer_base_is_worker_pending (balancer, worker_id) result (flag)
    class(balancer_base_t), intent(in) :: balancer
    integer, intent(in) :: worker_id
    integer :: resource_id
    flag = balancer%worker(worker_id)%assigned
    if (flag) then
       resource_id = balancer%worker(worker_id)%resource
       flag = balancer%resource(resource_id)%active
    end if
  end function balancer_base_is_worker_pending

  logical function balancer_base_is_pending (balancer) result (flag)
    class(balancer_base_t), intent(in) :: balancer
    flag = all (balancer%state%has_resource ())
  end function balancer_base_is_pending

end module balancer_base

