      subroutine check_par_out_of_bounds(par_current,par_bounds_flag)

      use data_all

      implicit none

      type (particle), intent(in) :: par_current
      logical, intent(out) :: par_bounds_flag
      integer :: my_dom

d     write(8,*) 'check_par_out_of_bounds in debug mode'

      my_dom = par_current%dom

      if ((par_current%coord(1) .ge. dom_array(my_dom)%coord(1,1))
     1    .and.
     1    (par_current%coord(1) .le. dom_array(my_dom)%coord(1,2))
     1    .and.
     1    (par_current%coord(2) .ge. dom_array(my_dom)%coord(2,1))
     1    .and.
     1    (par_current%coord(2) .le. dom_array(my_dom)%coord(2,2))
     1    .and.
     1    (par_current%coord(3) .ge. dom_array(my_dom)%coord(3,1))
     1    .and.
     1    (par_current%coord(3) .le. dom_array(my_dom)%coord(3,2))
     1    ) then
      par_bounds_flag = .true.
      else
      par_bounds_flag = .false.
      end if

      end subroutine check_par_out_of_bounds
