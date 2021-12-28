      subroutine compute_max_rel_vel(my_i,my_j,my_k,max_rel_vel)

c Compute the maximum of each velocity components within each cell.
c This is done by traversing through each particle present in the cell
c and finding the maximum absolute value of each of the three velocity
c components

      use data_all

      implicit none

      integer, intent(in) :: my_i, my_j, my_k
      real, intent (out) :: max_rel_vel

      integer :: i_dir, count_bin
      real :: max_vel(3)
      type (particle), pointer :: par_current
      type (particle_bin), pointer :: par_bin_current

d     write(8,*) 'compute_max_rel_vel in debug mode'

      par_bin_current => node_array(my_i,my_j,my_k)%par_bin%next
      max_vel(:) = 0.0

c       count_bin = 0
c       do while (associated(par_bin_current))
c       count_bin = count_bin + 1
c       write(8,*) 'par_number', par_bin_current%par%glb_num
c       par_bin_current => par_bin_current%next
c       end do
c c end do while (associated(par_bin_current))
c       write(8,*) 'bin particles, particles:', count_bin
c       par_bin_current => node_array(my_i,my_j,my_k)%par_bin%next

      do while (associated(par_bin_current))

      par_current => par_bin_current%par

      do i_dir = 1, 3
      max_vel(i_dir) = dmax1(max_vel(i_dir),
     1    dabs(par_current%vel(i_dir)))
      end do
c end do i_dir = 1, 3

      par_bin_current => par_bin_current%next

      end do
c end do while (associated(par_current))

c Maximum possible relative velocity is obtained when two particles
c approach each other, with velocity components that are maximum for
c the cell and are opposite of each other. Hence, each of the
c component of the max relative velocity vector will be twice the max
c velocity vector.
      call calculate_magnitude(2.0*max_vel,max_rel_vel)

      end subroutine compute_max_rel_vel
