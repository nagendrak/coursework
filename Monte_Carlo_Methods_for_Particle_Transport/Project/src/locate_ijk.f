      subroutine locate_ijk(xyz,ijk)

      use data_all

      implicit none

      integer :: i_dir, max_index
      integer, intent(inout) :: ijk(:)
      real, intent(in) :: xyz(:)

      do i_dir = 1, 3
      max_index = dom_array(1)%ijk(i_dir,2)
     1    - dom_array(1)%ijk(i_dir,1) + 1
      ijk(i_dir) = dint(
     1    ((xyz(i_dir) - dom_array(1)%coord(i_dir,1))
     1    /(dom_array(1)%coord(i_dir,2) - dom_array(1)%coord(i_dir,1))
     1    *max_index)) + 1
c     if (ijk(i_dir) .gt. dom_array(1)%ijk(i_dir,2))
c    1    ijk(i_dir) = dom_array(1)%ijk(i_dir,2)
      end do

      end subroutine locate_ijk
