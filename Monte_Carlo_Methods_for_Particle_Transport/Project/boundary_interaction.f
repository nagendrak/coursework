      subroutine boundary_interaction(par_current)

      use data_all
      use random

      implicit none

      type (particle), intent(inout), pointer :: par_current
      integer :: apply_bc_flag, i_dir, i_dir_flag, my_dom, my_dir,
     1    my_dir_flag, min_time_loc(2), alt_dir_flag, vel_bin_idx
      real :: time_to_bound(3,2), coord_prev(3), dt_rem, distance,
     1    distance_prev, vel_max, vel_mag, vel_old(3)
      logical :: par_bounds_flag

c---------------------------------------------------------------------
      interface

      subroutine advection(time_step,par_current)
      use data_all
      real, intent(in) :: time_step
      type (particle), pointer, intent(inout) :: par_current
      end subroutine advection

      end interface
c---------------------------------------------------------------------

d     write(8,*) 'boundary_interaction in debug mode'

      vel_max = 3.0*dsqrt(2.0*boltzmann_k*temp_init/mol_mass)
      my_dom = par_current%dom
      coord_prev(:) = par_current%coord_nm1(:)
      dt_rem = dt

      call check_par_out_of_bounds(par_current,par_bounds_flag)

c     if (par_bounds_flag .eq. .false.) write(8,*), ''

c Apply boundary conditions till all the boundaries have been that
c have been passed are processed
      do while (.not. par_bounds_flag)

c     write(8,*), 'par out of bounds', par_current%glb_num
c     write(8,*), 'coordinates', par_current%coord(:)
c     write(8,*), 'old coordinates', coord_prev(:)
c     write(8,*), 'velocities', par_current%vel(:)
c     write(8,*), 'applying boundary conditions'
c     write(8,*), 'time remaining', dt_rem

      apply_bc_flag = 0
      time_to_bound = 1.0E+10

      do i_dir = 1, 3

      do i_dir_flag = 1, 2

      distance = par_current%coord(i_dir)
     1    - dom_array(my_dom)%coord(i_dir,i_dir_flag)
      distance_prev = coord_prev(i_dir)
     1    - dom_array(my_dom)%coord(i_dir,i_dir_flag)
c If the distance to the boundary before and after the advection
c changed signs, that means the particle crossed that boundary.
      if (distance*distance_prev .lt. 0.0) then
      time_to_bound(i_dir,i_dir_flag) = abs(distance_prev
     1    /par_current%vel(i_dir))
      apply_bc_flag = 1
c     print*, 'bound crossed:', i_dir, i_dir_flag
      end if
c end if (distance*distance_nm1 .lt. 0.0) then

      end do
c end do i_dir_flag = 1, 2

      end do
c end do i_dir = 1, 3

      if (apply_bc_flag .eq. 1) then

c Locate the boundary which will be approached first
      min_time_loc = minloc(time_to_bound)
      my_dir = min_time_loc(1)
      my_dir_flag = min_time_loc(2)

c     print*, 'boundary', my_dir, my_dir_flag

      par_current%coord(:) = coord_prev(:)

      call advection(time_to_bound(my_dir,my_dir_flag),par_current)
      par_current%coord(my_dir) =
     1    dom_array(my_dom)%coord(my_dir,my_dir_flag)

c     write(8,*), 'after limited advection'
c     write(8,*), 'coordinates', par_current%coord(:)
c     write(8,*), 'velocities', par_current%vel(:)
c     write(8,*), 'time utilized', time_to_bound(my_dir,my_dir_flag)
c     write(8,*), time_to_bound

      dt_rem = dt_rem - time_to_bound(my_dir,my_dir_flag)

      if (dom_array(my_dom)%boundary_type(my_dir,my_dir_flag)
     1    .eq. 1) then
c Setting up periodic boundary condition

      if (my_dir_flag .eq. 1) then
      alt_dir_flag = 2
      else
      alt_dir_flag = 1
      end if

      par_current%coord(my_dir) =
     1    dom_array(my_dom)%coord(my_dir,alt_dir_flag)

      else if (dom_array(my_dom)%boundary_type(my_dir,my_dir_flag)
     1    .eq. 2) then
c Setting up diffuse thermal wall conditions

      dom_array(1)%n_wall_hits(my_dir,my_dir_flag) =
     1    dom_array(1)%n_wall_hits(my_dir,my_dir_flag) + 1
      vel_old(:) = par_current%vel(:)

      do i_dir = 1, 3

      if (i_dir .eq. my_dir) then
      par_current%vel(i_dir) = dsqrt(
     1    -2.0*boltzmann_k
     1    *dom_array(my_dom)%t_w(my_dir,my_dir_flag)
     1    *dlog(1.0-uniform_ran())/mol_mass)
      else
      par_current%vel(i_dir) = dsqrt(
     1    boltzmann_k*dom_array(my_dom)%t_w(my_dir,my_dir_flag)
     1    /mol_mass)*normal_ran(0.0,1.0)
     1    + dom_array(my_dom)%vel_w(my_dir,my_dir_flag,i_dir)
      end if

      end do
c end do i_dir = 1, 3

c Flip direction of the velocity component in the wall normal
c direction if the wall is the higher index (imax, jmax or kmax)
      if (my_dir_flag .eq. 2) par_current%vel(my_dir) =
     1    -par_current%vel(my_dir)
c     write(8,*) 'diffuse', par_current%vel(2),
c    1    vel_old(2)

      do i_dir = 1, 3
      dom_array(1)%del_vel_w(my_dir,my_dir_flag,i_dir) =
     1    dom_array(1)%del_vel_w(my_dir,my_dir_flag,i_dir) + 
     1    (par_current%vel(i_dir) - vel_old(i_dir))
      end do
c end do i_dir = 1, 3

c     write(8,*) 'velocities', par_current%vel(1), par_current%vel(2)

c  c Wall-parallel component
c        vel_mag = 0.0
c        do i_dir = 1, 3
c        if (i_dir .ne. my_dir) 
c       1    vel_mag = vel_mag + par_current%vel(i_dir)**2.0
c        end do
c  c end do i_dir = 1, 3
c        vel_mag = dsqrt(vel_mag)
c        vel_bin_idx = floor(vel_mag/vel_max*n_bins) + 1
c  c     write(8,*) 'velocity being indexed into:',vel_bin_idx
c        if (vel_bin_idx .gt. n_bins) vel_bin_idx = n_bins
c        vel_bin_test1(vel_bin_idx,1,1) = vel_bin_test1(vel_bin_idx,1,1)
c       1    + 1
c  c Wall-normal component
c        vel_mag = dabs(par_current%vel(my_dir))
c        vel_bin_idx = floor(vel_mag/vel_max*n_bins) + 1
c        if (vel_bin_idx .gt. n_bins) vel_bin_idx = n_bins
c        vel_bin_test1(vel_bin_idx,2,1) = vel_bin_test1(vel_bin_idx,2,1)
c       1    + 1

      else if (dom_array(my_dom)%boundary_type(my_dir,my_dir_flag)
     1    .eq. 3) then
c Setting up specular wall conditions
      
      par_current%vel(my_dir) = -par_current%vel(my_dir)

c  c Wall-parallel component
c        vel_mag = 0.0
c        do i_dir = 1, 3
c        if (i_dir .ne. my_dir) 
c       1    vel_mag = vel_mag + par_current%vel(i_dir)**2.0
c        end do
c  c end do i_dir = 1, 3
c        vel_mag = dsqrt(vel_mag)
c        vel_bin_idx = dint(vel_mag/vel_max*n_bins) + 1
c        if (vel_bin_idx .gt. n_bins) vel_bin_idx = n_bins
c        vel_bin_test1(vel_bin_idx,1,2) = vel_bin_test1(vel_bin_idx,1,2)
c       1    + 1
c  c Wall-normal component
c        vel_mag = dabs(par_current%vel(my_dir))
c        vel_bin_idx = dint(vel_mag/vel_max*n_bins) + 1
c        if (vel_bin_idx .gt. n_bins) vel_bin_idx = n_bins
c        vel_bin_test1(vel_bin_idx,2,2) = vel_bin_test1(vel_bin_idx,2,2)
c       1    + 1

c     write(8,*) 'specular', par_current%vel(3)

      end if
c end if (dom_array(my_dom)%boundary_type(my_dir) .eq. 1) then

c Mark the boundary as processed
      time_to_bound(:,:) = 1.0E+10

      else

c     print*, 'all boundaries considered'

      end if
c end if (apply_bc_flag .eq. 0) then

      coord_prev(:) = par_current%coord(:)
      call advection(dt_rem,par_current)
      call check_par_out_of_bounds(par_current,par_bounds_flag)

c     write(8,*), 'coord after', par_current%coord(:)
c     write(8,*), 'velocities', par_current%vel(:)

      end do
c end do while (.not. par_bounds_flag)

      end subroutine boundary_interaction
