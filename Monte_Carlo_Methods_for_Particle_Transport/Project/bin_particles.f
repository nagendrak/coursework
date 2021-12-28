      subroutine bin_particles

      use data_all

      implicit none

      integer :: i, j, k, min_particles_bin
      type (particle), pointer :: par_current
      type (particle_bin), pointer :: par_bin_current

      par_current => par_list%next

      do while (associated(par_current))

      i = par_current%ijk(1)
      j = par_current%ijk(2)
      k = par_current%ijk(3)

      par_bin_current => node_array(i,j,k)%par_bin

      call insert_par_to_bin(par_bin_current,par_current)
      node_array(i,j,k)%n_par = node_array(i,j,k)%n_par + 1

      par_current => par_current%next

      end do
c end do while (associated(par_current))

c Check if each cell has at least 30 cells, if not issue a warning.
      min_particles_bin = minval(node_array(:,:,:)%n_par)
      if (min_particles_bin .lt. 30) write(8,*) 'Warning:', i, j, k,
     1    'cell has less than 30 particles'

      end subroutine bin_particles
