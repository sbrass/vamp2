! WHIZARD 2.8.3 Oct 24 2019
!
! Copyright (C) 1999-2019 by
!     Wolfgang Kilian <kilian@physik.uni-siegen.de>
!     Thorsten Ohl <ohl@physik.uni-wuerzburg.de>
!     Juergen Reuter <juergen.reuter@desy.de>
!
!     with contributions from
!     cf. main AUTHORS file
!
! WHIZARD is free software; you can redistribute it and/or modify it
! under the terms of the GNU General Public License as published by
! the Free Software Foundation; either version 2, or (at your option)
! any later version.
!
! WHIZARD is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License
! along with this program; if not, write to the Free Software
! Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! This file has been stripped of most comments.  For documentation, refer
! to the source 'whizard.nw'

module request_balancer
  use kinds, only: default
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use array_list
  use partition
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
  type :: request_balancer_t
     private
     integer :: n_workers = 0
     integer :: n_resources = 0
     integer :: n_partitions = 0
     type(worker_t), dimension(:), allocatable :: worker
     type(resource_t), dimension(:), allocatable :: resource
     type(partition_t), dimension(:), allocatable :: partition
     !! SIZE(resource_state) = N_PARTITIONS
     type(resource_state_t), dimension(:), allocatable :: partition_state
   contains
     procedure :: write => request_balancer_write
     procedure :: init => request_balancer_init
     procedure, private :: init_partition => request_balancer_init_partition
     procedure, private :: init_partition_state => request_balancer_init_partition_state
     procedure :: add_resource_weight => request_balancer_add_resource_weight
     procedure :: add_partition => request_balancer_add_partition
     procedure :: get_n_workers => request_balancer_get_n_workers
     procedure :: get_n_resources => request_balancer_get_n_resources
     procedure :: get_resource_mode => request_balancer_get_resource_mode
     procedure :: get_resource_worker => request_balancer_get_resource_worker
     procedure :: is_assignable => request_balancer_is_assignable
     procedure :: is_pending => request_balancer_is_pending
     procedure :: assign_worker => request_balancer_assign_worker
     procedure :: free_worker => request_balancer_free_worker
     procedure, private :: assign_resource => request_balancer_assign_resource
     procedure, private :: assign_worker_group => request_balancer_assign_worker_group
     procedure, private :: free_worker_group => request_balancer_free_worker_group
  end type request_balancer_t

  public :: request_balancer_t, &
       REQUEST_BALANCER_SINGLE, &
       REQUEST_BALANCER_GROUP
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
         "N_PARTITIONS", balancer%n_partitions
    write (u, "(A)") "[WORKER]"
    do i = 1, balancer%n_workers
       call balancer%worker(i)%write (u)
    end do
    write (u, "(A)") "[RESOURCE]"
    do i = 1, balancer%n_resources
       call balancer%resource(i)%write (u)
    end do
    write (u, "(A)") "[PARTITION]"
    do i = 1, balancer%n_partitions
       call balancer%partition(i)%write (u)
       call balancer%partition_state(i)%write (u)
    end do
  end subroutine request_balancer_write

  subroutine request_balancer_init (balancer, n_workers, n_resources, skip_partition)
    class(request_balancer_t), intent(out), target :: balancer
    integer, intent(in) :: n_workers
    integer, intent(in) :: n_resources
    logical, intent(in), optional :: skip_partition
    balancer%n_workers = n_workers
    balancer%n_resources = n_resources
    allocate (balancer%worker (n_workers))
    allocate (balancer%resource (n_resources))
    call balancer%init_partition (skip_partition)
  end subroutine request_balancer_init

  subroutine request_balancer_init_partition (balancer, skip_partition)
    class(request_balancer_t), intent(inout) :: balancer
    logical, intent(in), optional :: skip_partition
    logical :: opt_skip_partition
    type(partition_t), dimension(:), allocatable :: partition
    integer, dimension(:), allocatable :: map_resource_to_partition
    opt_skip_partition = .false.; if (present(skip_partition)) opt_skip_partition = skip_partition
    if (opt_skip_partition) then
       call msg_message ("Balancer: Allocation of partition post-poned")
       return
    end if
    call msg_message ("Balancer: Allocate single partition")
    allocate (partition(N_REQUEST_BALANCER_PARTITIONS))
    call partition(REQUEST_BALANCER_SINGLE_PARTITION)%init &
         ("Request Balancer Partition", PARTITION_SINGLE, balancer%n_workers)
    allocate (map_resource_to_partition (balancer%n_resources), &
         source = REQUEST_BALANCER_SINGLE_PARTITION)
    call balancer%add_partition (partition, map_resource_to_partition)
  end subroutine request_balancer_init_partition

  subroutine request_balancer_init_partition_state (balancer, map_resource_to_partition)
    class(request_balancer_t), intent(inout) :: balancer
    integer, dimension(:), intent(in) :: map_resource_to_partition
    integer :: i_partition, i_resource, i_worker
    integer :: i, j
    !! Allocate a resource state for each partition, hence, a partition's resource state.
    allocate (balancer%partition_state (balancer%n_partitions))
    do i_partition = 1, balancer%n_partitions
       call balancer%partition_state(i_partition)%init ()
    end do
    !! Link worker to a partition.
    i_worker = 1
    do i = 1, balancer%n_partitions
       !! Sum of all elements of each partition must equal number of resources, ∑ n_i = n.
       do j = 1, balancer%partition(i)%get_n_elements ()
          if (i_worker > balancer%n_workers) then
             call msg_bug ("Balancer: Number of overall partitions workers&
                & exceeding global number of workers")
          end if
          balancer%worker(i_worker)%partition = i
          i_worker = i_worker + 1
       end do
    end do
    !! Fill partition states with resources.
    do i_resource = 1, balancer%n_resources
       i_partition = map_resource_to_partition (i_resource)
       call balancer%partition_state(i_partition)%add_resource (i_resource)
    end do
    !! Freeze partition state.
    do i_partition = 1, balancer%n_partitions
       call balancer%partition_state(i_partition)%freeze ()
    end do
  end subroutine request_balancer_init_partition_state

  !> Add resource weights.
  !! The resource weights have to be positive real numbers.
  !! We normalize the weights and apply them as prior to the load balancing.
  subroutine request_balancer_add_resource_weight (balancer, resource_weight)
    class(request_balancer_t), intent(inout) :: balancer
    real(default), dimension(:), intent(in) :: resource_weight
    if (size (resource_weight) /= balancer%n_resources) then
       call msg_bug ("Balancer: Mismatched size of resource weights")
    end if
    balancer%resource%weight = (resource_weight / sum (resource_weight))
  end subroutine request_balancer_add_resource_weight

   !> Add partition of workers and link with workers.
  !! We move the allocated partition object into the balancer.
  !! We then assign each partition its respective number of workers in a incrementing linear fashion.
  !! However, we postpone the linking of the resources to the partition, which can be either done dynamically with the balancer state or directly with the appropriate type-bound procedure.
  subroutine request_balancer_add_partition (balancer, partition, map_resource_to_partition)
    class(request_balancer_t), intent(inout) :: balancer
    type(partition_t), dimension(:), allocatable, intent(inout) :: partition
    integer, dimension(:), intent(in) :: map_resource_to_partition
    balancer%n_partitions = size (partition)
    call move_alloc (partition, balancer%partition)
    !! \todo sanitize map_resource_to_partition.
    call balancer%init_partition_state (map_resource_to_partition)
  end subroutine request_balancer_add_partition

  integer function request_balancer_get_n_workers (balancer) result (n_workers)
    class(request_balancer_t), intent(in) :: balancer
    n_workers = balancer%n_workers
  end function request_balancer_get_n_workers

  integer function request_balancer_get_n_resources (balancer) result (n_resources)
    class(request_balancer_t), intent(in) :: balancer
    n_resources = balancer%n_resources
  end function request_balancer_get_n_resources

  integer function request_balancer_get_resource_mode (balancer, worker_id) result (mode)
    class(request_balancer_t), intent(in) :: balancer
    integer, intent(in) :: worker_id
    integer :: partition_id, resource_id
    partition_id = balancer%worker(worker_id)%partition
    resource_id = balancer%worker(worker_id)%resource
    select case (balancer%partition(partition_id)%get_mode ())
    case (PARTITION_SINGLE)
       mode = REQUEST_BALANCER_SINGLE
    case (PARTITION_ALL)
       mode = REQUEST_BALANCER_GROUP
    case default
       call msg_bug ("Balancer: Unkown partition mode.")
    end select
  end function request_balancer_get_resource_mode

  subroutine request_balancer_get_resource_worker (balancer, resource_id, worker)
    class(request_balancer_t), intent(in) :: balancer
    integer, intent(in) :: resource_id
    integer, dimension(:), allocatable, intent(out) :: worker
    integer :: i
    worker = pack ([(i, i=1,balancer%n_workers)], balancer%worker%resource == resource_id)
  end subroutine request_balancer_get_resource_worker

  !> Find next resource and number of workers.
  !!
  subroutine request_balancer_assign_resource (balancer, worker_id, resource_id)
    class(request_balancer_t), intent(inout) :: balancer
    integer, intent(in) :: worker_id
    integer, intent(out) :: resource_id
    integer :: partition_id
    partition_id = balancer%worker(worker_id)%partition
    associate (partition_state => balancer%partition_state(partition_id))
      resource_id = partition_state%start_resource ()
      select case (balancer%partition(partition_id)%get_mode ())
      case (PARTITION_SINGLE)
         balancer%resource(resource_id)%n_assigned_workers = 1
      case (PARTITION_ALL)
         balancer%resource(resource_id)%n_assigned_workers = &
              balancer%partition(partition_id)%get_n_elements ()
      end select
    end associate
  end subroutine request_balancer_assign_resource

  pure logical function request_balancer_is_assignable (balancer, worker_id) result (flag)
    class(request_balancer_t), intent(in) :: balancer
    integer, intent(in) :: worker_id
    integer :: partition_id
    partition_id = balancer%worker(worker_id)%partition
    flag = balancer%partition_state(partition_id)%has_unassigned_resource ()
  end function request_balancer_is_assignable

  logical function request_balancer_is_pending (balancer) result (flag)
    class(request_balancer_t), intent(in) :: balancer
    flag = .not. all (balancer%partition_state%all_finished_resources ())
  end function request_balancer_is_pending

  !> Assign resource to a given worker.
  !!
  !! If worker has already a resource assigned, return resource.
  !! If worker has not assigned a resource, retrieve new resource from state
  !! and apply to worker.
  subroutine request_balancer_assign_worker (balancer, worker_id, resource_id)
    class(request_balancer_t), intent(inout) :: balancer
    integer, intent(in) :: worker_id
    integer, intent(out) :: resource_id
    integer :: partition_id
    associate (worker => balancer%worker(worker_id))
      if (worker%assigned) then
         resource_id = worker%resource
      else
         call balancer%assign_resource (worker_id, resource_id)
         partition_id = worker%partition
         select case (balancer%partition(partition_id)%get_mode ())
         case (PARTITION_SINGLE)
            call worker%add_resource (resource_id)
         case (PARTITION_ALL)
            call balancer%assign_worker_group (worker_id, resource_id)
         end select
      end if
    end associate
  end subroutine request_balancer_assign_worker

  subroutine request_balancer_assign_worker_group (balancer, worker_id, resource_id)
    class(request_balancer_t), intent(inout) :: balancer
    integer, intent(in) :: resource_id
    integer, intent(in) :: worker_id
    integer, dimension(:), allocatable :: indices
    integer :: i, n_free_workers
    associate (resource => balancer%resource(resource_id))
      !! 1. Assign requesting worker.
      call balancer%worker(worker_id)%add_resource (resource_id)
      !! 2. Assign all remaining (n - 1) workers.
      !! Indexing over all free workers.
      indices = pack([(i, i=1, balancer%n_workers)], &
           .not. balancer%worker%assigned .and. balancer%worker%partition == balancer%worker(worker_id)%partition)
      !! \note One worker is already assigned, go for the other \f$(N - 1)\f$ workers.
      n_free_workers = min (size (indices), resource%n_assigned_workers - 1)
      !! \todo{Replace with some less wasting algorithm.}
      do i = 1, n_free_workers
         call balancer%worker(indices(i))%add_resource (resource_id)
      end do
      !! Correct number of assigned workers.
      resource%n_assigned_workers = n_free_workers + 1
    end associate
  end subroutine request_balancer_assign_worker_group

  !> Idempotent.
  subroutine request_balancer_free_worker (balancer, worker_id)
    class(request_balancer_t), intent(inout) :: balancer
    integer, intent(in) :: worker_id
    integer :: resource_id, partition_id
    if (.not. balancer%worker(worker_id)%assigned) return
    resource_id = balancer%worker(worker_id)%resource
    partition_id = balancer%worker(worker_id)%partition
    if (balancer%partition_state(partition_id)%is_finished_resource (resource_id)) return
    call balancer%partition_state(partition_id)%finish_resource (resource_id)
    select case (balancer%partition(partition_id)%get_mode ())
    case (PARTITION_SINGLE)
       call balancer%worker(worker_id)%free ()
    case (PARTITION_ALL)
       call balancer%free_worker_group (resource_id)
    end select
  end subroutine request_balancer_free_worker

  subroutine request_balancer_free_worker_group (balancer, resource_id)
    class(request_balancer_t), intent(inout) :: balancer
    integer, intent(in) :: resource_id
    integer, dimension(:), allocatable :: indices
    integer :: i
    indices = pack([(i, i=1, balancer%n_workers)], balancer%worker%resource == resource_id)
    do i = 1, size (indices)
       call balancer%worker(indices(i))%free ()
    end do
  end subroutine request_balancer_free_worker_group
end module request_balancer

