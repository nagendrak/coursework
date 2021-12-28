      subroutine compute_averages

      use data_all

      implicit none

      integer :: i, j, k, i_dir, i_dir_flag, my_dir
      real :: sum_mass_den, sum_mom_den(3), sum_ener_den, vel_mag
      type (particle), pointer :: par_current
      type (particle_bin), pointer :: par_bin_current

c Traverse through each cell to compute averages
      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic

      sum_mass_den = 0.0
      sum_mom_den(:) = 0.0
      sum_ener_den = 0.0

      par_bin_current => node_array(i,j,k)%par_bin%next
      do while (associated(par_bin_current))

      par_current => par_bin_current%par

      sum_mass_den = sum_mass_den + mol_mass
c     sum_mass_den = sum_mass_den + mol_mass*n_eff
      sum_mom_den(:) = sum_mom_den(:)
     1    + mol_mass*par_current%vel(:)
c    1    + mol_mass*n_eff*par_current%vel(:)
      call calculate_magnitude(par_current%vel,vel_mag)
      sum_ener_den = sum_ener_den
     1    + 0.5*mol_mass*vel_mag**2.0
c    1    + 0.5*mol_mass*n_eff*vel_mag**2.0

      par_bin_current => par_bin_current%next

      end do
c end do while (associated(par_bin_current))

      node_array(i,j,k)%mass_den = sum_mass_den/node_array(i,j,k)%vol

      node_array(i,j,k)%mom_den(:) =
     1    sum_mom_den(:)/node_array(i,j,k)%vol
      node_array(i,j,k)%vel(:) =
     1    node_array(i,j,k)%mom_den(:)/node_array(i,j,k)%mass_den
      call calculate_magnitude(node_array(i,j,k)%vel,
     1    node_array(i,j,k)%vel_mag)

      node_array(i,j,k)%ener_den = sum_ener_den/node_array(i,j,k)%vol
      node_array(i,j,k)%temp = 2.0*mol_mass/3.0/boltzmann_k
     1    *((node_array(i,j,k)%ener_den/node_array(i,j,k)%mass_den)
     1    - (0.5*(node_array(i,j,k)%vel_mag**2.0)))

      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc

c Compute the force on every wall
      do i_dir = 1, 3
      do i_dir_flag = 1, 2
      dom_array(1)%force(i_dir,i_dir_flag,:) =
     1    (dom_array(1)%del_vel_w(i_dir,i_dir_flag,:)*
     1    n_eff*mol_mass)
     1    /((dt*i_dt)*dom_array(1)%area(i_dir,i_dir_flag))
      end do
c end do i_dir_flag = 1, 2
      end do
c end do i_dir = 1, 3
      dom_array(1)%force_avg(:,:,:) = (dom_array(1)%force_avg(:,:,:)
     1    *(i_realization-1) + dom_array(1)%force(:,:,:))
     1    /i_realization

      call delete_bin_particles

      end subroutine compute_averages
