      subroutine initialize_particles
      
      use data_all

      implicit none

      integer :: i_par, i_dir
      real :: rand_num
      type (particle), pointer :: par_current

      par_current => par_list

      do i_par = 1, n_particles

      call par_initialize(par_current%next)
      par_current => par_current%next

c Determine particle locations
      do i_dir = 1, 3
      call random_number(rand_num)
      par_current%coord(i_dir) = dom_array(1)%coord(i_dir,1)
     1    + rand_num*dom_array(1)%coord(i_dir,2)
      end do

c Locate particles' i,j,k's
      call locate_ijk(par_current%coord,par_current%ijk)

c Determine initial velocities for particles
      do i_dir = 1, 3
      call maxwellian_dist(temp_init,i_dir,par_current)
      end do
c end do i_dir = 1, 3

c end do i_particle = 1, n_particles
      end do

      end subroutine initialize_particles
