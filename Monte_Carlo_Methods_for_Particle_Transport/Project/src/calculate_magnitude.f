      subroutine calculate_magnitude(vec,vec_mag)

      implicit none

      real, intent(in) :: vec(:)
      real, intent(out) :: vec_mag

      vec_mag = dsqrt(vec(1)**2.0 + vec(2)**2.0 + vec(3)**2.0)

      end subroutine calculate_magnitude
