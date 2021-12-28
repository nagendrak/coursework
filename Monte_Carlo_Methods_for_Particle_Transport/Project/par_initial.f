      subroutine par_initial
      
      use data_all
      use random

      implicit none

c--------------------------------------------------------------------
      interface

      subroutine locate_ijk(xyz,ijk)
      integer, intent(inout) :: ijk(:)
      real, intent(in) :: xyz(:)
      end subroutine locate_ijk

      subroutine maxwellian_dist(temp,my_dir,par_current)
      use data_all
      integer, intent(in) :: my_dir
      real, intent(in) :: temp
      type (particle), intent(inout), pointer :: par_current
      end subroutine maxwellian_dist

      end interface
c--------------------------------------------------------------------

      integer :: i_par, i_dir
      real :: rand_num
      type (particle), pointer :: par_current

d     write(8,*) 'par_initial in debug mode'

      par_current => par_list

      do i_par = 1, n_particles

      call par_insert(par_current)
c     par_current => par_current%next

      par_current%glb_num = i_par

c Determine particle locations
      do i_dir = 1, 3
      par_current%coord(i_dir) = dom_array(1)%coord(i_dir,1)
     1    + uniform_ran()*(dom_array(1)%coord(i_dir,2)
     1    - dom_array(1)%coord(i_dir,1))
      end do
      par_current%dom = 1

c Locate particles' i,j,k's
      call locate_ijk(par_current%coord,par_current%ijk)

c Determine initial velocities for particles
      do i_dir = 1, 3
      call maxwellian_dist(temp_init,i_dir,par_current)
      end do
c end do i_dir = 1, 3

d     write(8,*) 'Particle:', i_par
d     write(8,*) 'location', par_current%coord(:)
d     write(8,*) 'ijk', par_current%ijk(:)
d     write(8,*) 'velocities', par_current%vel(:)

c end do i_particle = 1, n_particles
      end do

      end subroutine par_initial
