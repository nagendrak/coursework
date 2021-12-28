      subroutine apply_boundary_condition(my_par,my_dir,dir_flag)

c i_dom - Current domain number

c dir_flag - represents if the boundary is the minimum (1) or maximum
c (2) of that boundary.

      use data_all

      implicit none

      integer, intent(in) :: my_dir, dir_flag, my_par
      real, intent(inout) :: phi_xyz

      my_dom = par_list(my_par)%dom

      if (dir_flag .eq. 1) then
      time_before =
     1    (domain_list%coord_max(my_dir)
     1    - par_list(my_par)%coord(my_dir))
     1    /par_list(my_par)%vel(my_dir)
      else
      time_before =
     1    (par_list(my_par)%coord(my_dir)
     1    - domain_list%coord_min(my_dir))
     1    /par_list(my_par)%vel(my_dir)
      end if
      time_after = dt - time_before

      if (boundary_type(my_dir) .eq. 1) then
c Setting up periodic boundary condition

      if (dir_flag .eq. 1) then
      par_list(i_par)%coord(my_dir) = par_list(my_par)%coord(my_dir)
     1    - (domain_list(my_dom)%coord_max(my_dir)
     1     - domain_list(my_dom)%coord_min(my_dir))
      else if (dir_flag .eq. 2) then
      par_list(my_par)%coord(my_dir) = par_list(my_par)%coord(my_dir)
     1    + (domain_list(my_dom)%coord_max(my_dir)
     1     - domain_list(my_dom)%coord_min(my_dir))
      end if

      else if (boundary_type(my_dir) .eq. 2) then
c Setting up diffuse thermal wall conditions

      par_list(my_par)%coord(:) = par_list(my_par)%coord(:)
     1    + par_list(my_par)%vel(:)*time_before
      
      do i_dir = 1, 3

      if (i_dir .eq. my_dir)
      call random_number(uniform_rn)
      par_list(my_par)%vel(i_dir) = dsqrt(
     1    -2.0*boltzmann_k
     1    *domain_list(my_dom)%temp_w(my_dir,dir_flag)
     1    *dlog(uniform_rand_num)/par_list(my_par)%mass)
      else
      call normal_random_number(normal_rn)
      par_list(my_par)%vel(i_dir) = dsqrt(
     1    boltzmann_k*domain_list(my_dom)%temp_w(my_dir,dir_flag)
     1    /par_list(my_par)%mass)*normal_rn
     1    + domain_list(my_dom)%vel_w(my_dir,dir_flag)
      end if

      end do
c end do i_dir = 1, 3

      end if

      end subroutine apply_boundary_condition
