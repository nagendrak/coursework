      subroutine maxwellian_dist(temp,my_dir,par_current)

      use data_all
      use random

      implicit none

      integer, intent(in) :: my_dir
      real, intent(in) :: temp
      type (particle), intent(inout), pointer :: par_current

      par_current%vel(my_dir) = dsqrt(boltzmann_k*temp
     1    /mol_mass)*normal_ran(0.0,1.0)

      end subroutine maxwellian_dist
