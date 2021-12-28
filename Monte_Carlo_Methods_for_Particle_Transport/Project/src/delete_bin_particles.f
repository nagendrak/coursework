      subroutine delete_bin_particles

      use data_all

      implicit none

      integer :: i, j, k, count_bin
      type (particle_bin), pointer :: par_bin_current, par_bin_temp

c Go through each cell and delete binned particles
      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic
      par_bin_current => node_array(i,j,k)%par_bin%next
      nullify(node_array(i,j,k)%par_bin%next)
      node_array(i,j,k)%n_par = 0
      do while (associated(par_bin_current))
      par_bin_temp => par_bin_current%next
      deallocate(par_bin_current)
      par_bin_current => par_bin_temp
      end do
c end do while (associated(par_bin_current))
      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc

      end subroutine delete_bin_particles
