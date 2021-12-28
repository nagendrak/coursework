      subroutine free_flight

      use data_all

      implicit none

      integer :: i_par
      type (particle), pointer :: par_current

c--------------------------------------------------------------------
      interface

      subroutine boundary_interaction(par_current)
      use data_all
      type (particle), intent(inout), pointer :: par_current
      end subroutine boundary_interaction

      subroutine advection(time_step,par_current)
      use data_all
      real, intent(in) :: time_step
      type (particle), pointer, intent(inout) :: par_current
      end subroutine advection

      subroutine locate_ijk(xyz,ijk)
      integer, intent(inout) :: ijk(:)
      real, intent(in) :: xyz(:)
      end subroutine locate_ijk

      end interface
c--------------------------------------------------------------------

d     write(8,*) 'free_flight in debug mode'

      dom_array(1)%del_vel_w(:,:,:) = 0.0

      par_current => par_list%next

      do while (associated(par_current))

c     write(6,*) 'free flight particle', par_current%glb_num

      par_current%coord_nm1(:) = par_current%coord(:)

      call advection(dt,par_current)

c Compute wall interactions and new particle positions for those which
c collided with a wall
      call boundary_interaction(par_current)

      call locate_ijk(par_current%coord(:),par_current%ijk(:))

      par_current => par_current%next

      end do
c end do while (associated(par_current))

      end subroutine free_flight
