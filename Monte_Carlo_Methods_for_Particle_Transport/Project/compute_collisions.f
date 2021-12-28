      subroutine compute_collisions

      use data_all
      use random

      implicit none

      integer :: i, j, k, max_num_coll_success, num_coll_success,
     1    pair_num, par_num_1, par_num_2, par_num_temp, i_par
      real :: max_rel_vel, sum_rel_vel_mag, rel_vel_mag,
     1    avg_rel_vel_mag, pi
      type (particle), pointer :: par_current, par_1, par_2
      type (particle_bin), pointer :: par_bin_current

c--------------------------------------------------------------------
      interface
      subroutine collide_particles(par_1,par_2)
      use data_all
      type (particle), pointer, intent(inout) :: par_1, par_2
      end subroutine collide_particles
      end interface
c--------------------------------------------------------------------

d     write(8,*) 'compute_collisions in debug mode'

      pi = dacos(-1.0)

      call bin_particles

c Go through each cell and perform collisions
      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic

c Compute an estimate for maximum relative speed. This is a
c trade-off between efficiency of the collision pair selection scheme
c and computing the actual maximum relative speed for all potential
c collision partners in each cell.
      call compute_max_rel_vel(i,j,k,max_rel_vel)

c Initial guess for max_num_coll_success
      max_num_coll_success = 100
      num_coll_success = 0
      pair_num = 0
      sum_rel_vel_mag = 0.0

      do while (num_coll_success .le. max_num_coll_success)

      pair_num = pair_num + 1

c Select one out of the total number of particles present in the cell
      par_num_1 = uniform_int_spread(1,node_array(i,j,k)%n_par)

c Select the second particle - from the total number of particles
c remaining (n_particles_bin - 1). Later adjust the number to reflect
c the actual number on the list.
      par_num_2 = uniform_int_spread(1,node_array(i,j,k)%n_par-1)
      if (par_num_2 .ge. par_num_1) then
      par_num_2 = par_num_2 + 1
      else
      par_num_temp = par_num_1
      par_num_1 = par_num_2
      par_num_2 = par_num_temp
      end if

      par_bin_current => node_array(i,j,k)%par_bin
      do i_par = 1, par_num_1
      par_bin_current => par_bin_current%next
      end do
c end do i_par = 1, par_num_1
      par_1 => par_bin_current%par
      do i_par = par_num_1+1, par_num_2
      par_bin_current => par_bin_current%next
      end do
c end do i_par = par_num_1+1, par_num_2
      par_2 => par_bin_current%par

      call calculate_magnitude((par_1%vel(:)-par_2%vel(:)),
     1    rel_vel_mag)

c Use rejection technique to determine if the collision will be
c successful
      if ((rel_vel_mag/max_rel_vel) .gt. uniform_ran()) then
      num_coll_success = num_coll_success + 1
      call collide_particles(par_1,par_2)
      end if

c Updating of average relative speed for the cell (is this correct?)
      sum_rel_vel_mag = sum_rel_vel_mag + rel_vel_mag
      avg_rel_vel_mag = sum_rel_vel_mag/pair_num

c Compute maximum number of collisions allowed in the cell
      max_num_coll_success = (node_array(i,j,k)%n_par**2.0
     1    *pi*eff_dia**2.0*max_rel_vel*n_eff*dt
     1    /2.0/node_array(i,j,k)%vol)
c    1    *(rel_vel_mag/max_rel_vel)
     1    *(avg_rel_vel_mag/max_rel_vel)

c     write(8,*) 'max coll value', max_num_coll_success,
c    1    avg_rel_vel_mag

      end do
c end do while (num_coll_success .le. max_num_coll_success)

c     write(8,*) 'max, allowed', max_num_coll_success,
c    1    num_coll_success
      n_total_coll = n_total_coll + num_coll_success

      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc

      end subroutine compute_collisions
