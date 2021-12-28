      subroutine advection(time_step,par_current)

      use data_all

      implicit none

      real, intent(in) :: time_step
      integer :: i_par, i_dir
      type (particle), pointer, intent(inout) :: par_current

d     write(8,*) 'advection in debug mode'

      do i_dir = 1, 3
      par_current%coord(i_dir) = par_current%coord(i_dir)
     1    + par_current%vel(i_dir)*time_step
      end do
c end do i_dir = 1, 3

      end subroutine advection
