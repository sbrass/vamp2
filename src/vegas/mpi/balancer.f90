module request_balancer
  use kinds, only: default
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use array_list
  use resources

  use diagnostics

  implicit none

  private

  integer, parameter :: N_REQUEST_BALANCER_PARTITIONS = 1, &
       REQUEST_BALANCER_SINGLE_PARTITION = 1

  integer, parameter :: REQUEST_BALANCER_SINGLE = 1, &
       REQUEST_BALANCER_GROUP = 2

  type :: worker_t
     private
     integer :: resource = 0
     integer :: partition = 0
     integer :: n_resources = 0
     logical :: assigned = .false.
   contains
     procedure :: write => worker_write
     procedure :: add_resource => worker_add_resource
     procedure :: free => worker_free
  end type worker_t

  type :: worker_state_t
     type(array_list_t) :: unassigned
     type(array_list_t) :: assigned
  end type worker_state_t

  !> Dynamic load balancer.
  !!
  !! We organize resources and workers in a transparent way using indices.
  !! These indices replace pointer magic.
  !!
  !! The balancer aggregates a dynamic state, however, we allow the state by
  !! the use of a pointer, to access the static fields of the balancer.
  type, abstract :: request_balancer_t
     private
     integer :: n_workers = 0
     integer :: n_resources = 0
     integer :: n_states = 0
     type(worker_t), dimension(:), allocatable :: worker
     type(resource_t), dimension(:), allocatable :: resource
     type(resource_state_t), dimension(:), allocatable :: state
   contains
     procedure :: write => request_balancer_write
     procedure :: base_init => request_balancer_base_init
     procedure :: add_state => request_balancer_add_state
     procedure :: is_assignable => request_balancer_is_assignable
     procedure :: is_pending => request_balancer_is_pending
     procedure(request_balancer_has_resource_group), deferred :: has_resource_group
     procedure(request_balancer_get_resource_group), deferred :: get_resource_group
     procedure(request_balancer_get_resource_master), deferred :: get_resource_master
     procedure(request_balancer_assign_worker), deferred :: assign_worker
     procedure(request_balancer_free_worker), deferred :: free_worker
  end type request_balancer_t

  abstract interface
     !> Pure forbids any MPI intrusion!!
     pure logical function request_balancer_has_resource_group (balancer, resource_id) &
          result (flag)
       import :: request_balancer_t
       class(request_balancer_t), intent(in) :: balancer
       integer, intent(in) :: resource_id
     end function request_balancer_has_resource_group

     pure subroutine request_balancer_get_resource_group (balancer, resource_id, group)
       import :: request_balancer_t
       class(request_balancer_t), intent(in) :: balancer
       integer, intent(in) :: resource_id
       integer, dimension(:), allocatable, intent(out) :: group
     end subroutine request_balancer_get_resource_group

     pure integer function request_balancer_get_resource_master (balancer, resource_id) &
          result (worker)
       import :: request_balancer_t
       class(request_balancer_t), intent(in) :: balancer
       integer, intent(in) :: resource_id
     end function request_balancer_get_resource_master

     !> Assign resource to a given worker or retrieve current assigned resource.
     !!
     !! If worker has already a resource assigned, return resource.
     !! If worker has not assigned a resource, retrieve new resource from state.
     subroutine request_balancer_assign_worker (balancer, worker_id, resource_id)
       import :: request_balancer_t
       class(request_balancer_t), intent(inout) :: balancer
       integer, intent(in) :: worker_id
       integer, intent(out) :: resource_id
     end subroutine request_balancer_assign_worker

     !> Free assignment of worker.
     !!
     !! If worker is not assigned, this procedure is idempotent.
     !! If worker is assigned, alter state correspondingly.
     subroutine request_balancer_free_worker (balancer, worker_id)
       import :: request_balancer_t
       class(request_balancer_t), intent(inout) :: balancer
       integer, intent(in) :: worker_id
     end subroutine request_balancer_free_worker
  end interface

  public :: request_balancer_t
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

  subroutine worker_add_resource (worker, resource_id)
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

  subroutine request_balancer_write (balancer, unit)
    class(request_balancer_t), intent(in) :: balancer
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
  end subroutine request_balancer_write

  subroutine request_balancer_base_init (balancer, n_workers, n_resources)
    class(request_balancer_t), intent(out) :: balancer
    integer, intent(in) :: n_workers
    integer, intent(in) :: n_resources
    balancer%n_workers = n_workers
    balancer%n_resources = n_resources
    allocate (balancer%worker (n_workers))
    allocate (balancer%resource (n_resources))
  end subroutine request_balancer_base_init

  !> Add partition of workers and link with workers.
  !! We move the allocated partition object into the balancer.
  !! We then assign each partition its respective number of workers in a incrementing linear fashion.
  !! However, we postpone the linking of the resources to the partition, which can be either done dynamically with the balancer state or directly with the appropriate type-bound procedure.
  subroutine request_balancer_add_state (balancer, state)
    class(request_balancer_t), intent(inout) :: balancer
    type(resource_state_t), dimension(:), allocatable, intent(inout) :: state
    integer :: i, j, i_worker
    balancer%n_states = size (state)
    call move_alloc (state, balancer%state)
    !! Link worker to a state.
    do i = 1, balancer%n_states
       i_worker = 1
       do j = 1, balancer%state(i)%n_workers
          if (i_worker > balancer%n_workers) then
             call msg_bug ("Balancer: Number of state workers&
                  & exceeding global number of workers")
          end if
          balancer%worker(i_worker)%partition = i
          i_worker = i_worker + 1
       end do
    end do
  end subroutine request_balancer_add_state

  ! integer function request_balancer_get_resource_mode (balancer, worker_id) result (mode)
  !   class(request_balancer_t), intent(in) :: balancer
  !   integer, intent(in) :: worker_id
  !   integer :: partition_id, resource_id
  !   partition_id = balancer%worker(worker_id)%partition
  !   resource_id = balancer%worker(worker_id)%resource
  !   select case (balancer%partition(partition_id)%get_mode ())
  !   case (PARTITION_SINGLE)
  !      mode = REQUEST_BALANCER_SINGLE
  !   case (PARTITION_ALL)
  !      mode = REQUEST_BALANCER_GROUP
  !   case default
  !      call msg_bug ("Balancer: Unkown partition mode.")
  !   end select
  ! end function request_balancer_get_resource_mode

  !> Is a worker and has he a assignable resource.
  !!
  !! The answer depends on two factors:
  !! (i) Is there still work in the associated partition?
  !! (ii) Is the worker already assigned? E.g. as part of a group and needs to retrieve its resources?
  !! Is either one of the cases true, the worker has an assignable resource.
  pure logical function request_balancer_is_assignable (balancer, worker_id) result (flag)
    class(request_balancer_t), intent(in) :: balancer
    integer, intent(in) :: worker_id
    integer :: partition_id
    partition_id = balancer%worker(worker_id)%partition
    flag = balancer%worker(worker_id)%assigned .or. &
         balancer%state(partition_id)%has_resource ()
  end function request_balancer_is_assignable

  logical function request_balancer_is_pending (balancer) result (flag)
    class(request_balancer_t), intent(in) :: balancer
    flag = .not. all (balancer%state%has_resource ())
  end function request_balancer_is_pending

  !> Assign resource to a given worker or retrieve current assigned resource.
  !!
  !! If worker has already a resource assigned, return resource.
  !! If worker has not assigned a resource, retrieve new resource from state
  !! and apply to worker.
  ! subroutine request_balancer_assign_worker (balancer, worker_id, resource_id)
  !   class(request_balancer_t), intent(inout) :: balancer
  !   integer, intent(in) :: worker_id
  !   integer, intent(out) :: resource_id
  !   integer :: partition_id
  !   associate (worker => balancer%worker(worker_id))
  !     if (worker%assigned) then
  !        resource_id = worker%resource
  !     else
  !        call balancer%assign_resource (worker_id, resource_id)
  !        partition_id = worker%partition
  !        select case (balancer%partition(partition_id)%get_mode ())
  !        case (PARTITION_SINGLE)
  !           call worker%add_resource (resource_id)
  !        case (PARTITION_ALL)
  !           call balancer%assign_worker_group (worker_id, resource_id)
  !        end select
  !     end if
  !   end associate
  ! end subroutine request_balancer_assign_worker

  ! subroutine request_balancer_assign_worker_group (balancer, worker_id, resource_id)
  !   class(request_balancer_t), intent(inout) :: balancer
  !   integer, intent(in) :: resource_id
  !   integer, intent(in) :: worker_id
  !   integer, dimension(:), allocatable :: indices
  !   integer :: i, n_free_workers
  !   associate (resource => balancer%resource(resource_id))
  !     !! 1. Assign requesting worker.
  !     call balancer%worker(worker_id)%add_resource (resource_id)
  !     !! 2. Assign all remaining (n - 1) workers.
  !     !! Indexing over all free workers.
  !     indices = pack([(i, i=1, balancer%n_workers)], &
  !          .not. balancer%worker%assigned .and. balancer%worker%partition == balancer%worker(worker_id)%partition)
  !     !! \note One worker is already assigned, go for the other \f$(N - 1)\f$ workers.
  !     n_free_workers = min (size (indices), resource%n_assigned_workers - 1)
  !     !! \todo{Replace with some less wasting algorithm.}
  !     do i = 1, n_free_workers
  !        call balancer%worker(indices(i))%add_resource (resource_id)
  !     end do
  !     !! Correct number of assigned workers.
  !     resource%n_assigned_workers = n_free_workers + 1
  !   end associate
  ! end subroutine request_balancer_assign_worker_group

  ! !> Idempotent.
  ! subroutine request_balancer_free_worker (balancer, worker_id)
  !   class(request_balancer_t), intent(inout) :: balancer
  !   integer, intent(in) :: worker_id
  !   integer :: resource_id, partition_id
  !   if (.not. balancer%worker(worker_id)%assigned) return
  !   resource_id = balancer%worker(worker_id)%resource
  !   partition_id = balancer%worker(worker_id)%partition
  !   if (balancer%state(partition_id)%is_finished_resource (resource_id)) return
  !   call balancer%state(partition_id)%finish_resource (resource_id)
  !   select case (balancer%partition(partition_id)%get_mode ())
  !   case (PARTITION_SINGLE)
  !      call balancer%worker(worker_id)%free ()
  !   case (PARTITION_ALL)
  !      call balancer%free_worker_group (resource_id)
  !   end select
  ! end subroutine request_balancer_free_worker

  ! subroutine request_balancer_free_worker_group (balancer, resource_id)
  !   class(request_balancer_t), intent(inout) :: balancer
  !   integer, intent(in) :: resource_id
  !   integer, dimension(:), allocatable :: indices
  !   integer :: i
  !   indices = pack([(i, i=1, balancer%n_workers)], balancer%worker%resource == resource_id)
  !   do i = 1, size (indices)
  !      call balancer%worker(indices(i))%free ()
  !   end do
  ! end subroutine request_balancer_free_worker_group
end module request_balancer

