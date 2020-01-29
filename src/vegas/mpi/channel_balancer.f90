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

module channel_balancer
  use kinds, only: default
  use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
  use partition
  use request_balancer

  use diagnostics

  implicit none

  private

  real(default), parameter :: BETA = 1.5_default

  integer, parameter :: N_CHANNEL_BALANCER_PARTITIONS = 2, &
       CHANNEL_PARTITION = 1, &
       GRID_PARTITION = 2

  type, extends(request_balancer_t) :: channel_balancer_t
     private
     integer :: n_parallel_grids = 0
     integer :: n_parallel_channels = 0
     integer :: n_grid_workers = 0
     integer :: n_channel_workers = 0
     logical, dimension(:), allocatable :: parallel_grid
   contains
     procedure :: init => channel_balancer_init
     procedure :: write => channel_balancer_write
     procedure :: add_parallel_grid => channel_balancer_add_parallel_grid
     procedure :: add_channel_weight => channel_balancer_add_channel_weight
     procedure, private :: compute_mixed_resource_weight => &
          channel_balancer_compute_mixed_resource_weight
  end type channel_balancer_t

  public :: channel_balancer_t
contains
  subroutine channel_balancer_init (balancer, n_workers, n_resources, skip_partition)
    class(channel_balancer_t), intent(out), target :: balancer
    integer, intent(in) :: n_workers
    integer, intent(in) :: n_resources
    logical, intent(in), optional :: skip_partition !! ignore option
    !! Basic init.
    call balancer%request_balancer_t%init (n_workers, n_resources, &
         skip_partition = .true.)
    allocate (balancer%parallel_grid(n_resources), source = .false.)
    if (present (skip_partition)) then
       call msg_bug ("Balancer: Skip partition option not supported.")
    end if
    !! Post-pone partition initialisation → add_channel_weight.
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
    call balancer%request_balancer_t%write (u)
  end subroutine channel_balancer_write

  subroutine channel_balancer_add_parallel_grid (balancer, parallel_grid)
    class(channel_balancer_t), intent(inout) :: balancer
    logical, dimension(:), intent(in) :: parallel_grid
    if (size (parallel_grid) /= balancer%get_n_resources ()) then
       call msg_bug ("Balancer: mismatch in size of parallel grid.")
    end if
    balancer%parallel_grid = parallel_grid
  end subroutine channel_balancer_add_parallel_grid

  !> Load balancing.
  subroutine channel_balancer_add_channel_weight (balancer, weight)
    class(channel_balancer_t), intent(inout) :: balancer
    real(default), dimension(:), intent(in) :: weight
    real(default) :: min_parallel_weight
    real(default), dimension(:), allocatable :: resource_weight
    min_parallel_weight = balancer%get_n_resources ()**(1._default - 1_default / BETA) &
         / balancer%get_n_workers ()**BETA
    balancer%parallel_grid = &
         balancer%parallel_grid .and. (weight >= min_parallel_weight)
    allocate (resource_weight(balancer%get_n_resources ()), source = 0._default)
    if (balancer%get_n_resources () >= balancer%get_n_workers ()) then
       !! Apply full multi-channel parallelization.
       call msg_message ("Balancer: Apply full multi-channel parallelization")
       balancer%n_parallel_grids = 0
       balancer%n_parallel_channels = balancer%get_n_resources ()
       balancer%parallel_grid = .false.
       balancer%n_grid_workers = 0
       balancer%n_channel_workers = balancer%get_n_workers ()
       resource_weight = 1
       call balancer%add_resource_weight (resource_weight)
    else
       if (count (balancer%parallel_grid) == balancer%get_n_resources ()) then
          !! Apply full VEGAS parallelization.
          balancer%n_parallel_grids = balancer%get_n_resources ()
          call msg_message ("Balancer: Apply full VEGAS parallelization")
          balancer%n_parallel_channels = 0
          balancer%n_grid_workers = balancer%get_n_workers ()
          balancer%n_channel_workers = 0
          resource_weight = weight
          call balancer%add_resource_weight (resource_weight)
       else
          call msg_message ("Balancer: Apply mixed parallelization")
          balancer%n_parallel_grids = count (balancer%parallel_grid)
          balancer%n_parallel_channels = balancer%get_n_resources () - balancer%n_parallel_grids
          call balancer%compute_mixed_resource_weight (weight)
          resource_weight = weight / sum(weight, balancer%parallel_grid)
          where (balancer%parallel_grid)
             resource_weight = resource_weight * balancer%n_grid_workers
          elsewhere
             resource_weight = 1._default
          end where
          call balancer%add_resource_weight (resource_weight)
       end if
    end if
    call apply_partition ()
  contains
    subroutine apply_partition ()
      type(partition_t), dimension(:), allocatable :: partition
      integer, dimension(:), allocatable :: map_channel_to_partition
      call msg_message ("Allocate channel/grid partitions.")
      allocate (partition(N_CHANNEL_BALANCER_PARTITIONS))
      call partition(CHANNEL_PARTITION)%init ("Channel partition", PARTITION_SINGLE, balancer%n_channel_workers)
      call partition(GRID_PARTITION)%init ("Grid partition", PARTITION_ALL, balancer%n_grid_workers)
      map_channel_to_partition = merge (GRID_PARTITION, CHANNEL_PARTITION, balancer%parallel_grid)
      call balancer%add_partition (partition, map_channel_to_partition)
    end subroutine apply_partition
  end subroutine channel_balancer_add_channel_weight

  subroutine channel_balancer_compute_mixed_resource_weight (balancer, weight)
    class(channel_balancer_t), intent(inout) :: balancer
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
         / (balancer%get_n_resources () - balancer%n_parallel_grids)
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
    balancer%n_channel_workers = nint (ratio * balancer%get_n_workers ())
    balancer%n_grid_workers = nint ((1 - ratio) * balancer%get_n_workers ())
    !! In the case of small numbers of workers and a very small ratio,
    !! nint can assign no worker to channel/grid parallelization,
    !! which is still requested by n_parallel_channels/grids.
    !! In that case, we have to enforce: n_worker = n_channel_worker + n_grid_worker
    if (balancer%get_n_workers () >= 2 &
         .AND. (balancer%n_parallel_channels > 0 .and. balancer%n_channel_workers < 1)) then
       balancer%n_channel_workers = 1
       balancer%n_grid_workers = balancer%n_grid_workers - 1
    end if
    !! The grid resources will only be increased to N = 2
    !! if more than 3 workers are present.
    if (balancer%get_n_workers () >= 3 &
         .AND. (balancer%n_parallel_grids > 0 .and. balancer%n_grid_workers < 2)) then
       balancer%n_grid_workers = 2
       balancer%n_channel_workers = balancer%n_channel_workers - 2
    end if
  end subroutine channel_balancer_compute_mixed_resource_weight
end module channel_balancer

