      module random
c module contains three functions
c ran1 returns a uniform random number between 0-1
c spread returns random number between min - max
c normal returns a normal distribution

      contains

c returns random number between 0 - 1
      function uniform_ran()  

      implicit none

      real :: uniform_ran, x

      call random_number(x)
      uniform_ran = x

      end function uniform_ran

c returns random number between min - max
      function uniform_spread(min_val,max_val)

      implicit none

      real :: uniform_spread
      real :: min_val, max_val

      uniform_spread = (max_val - min_val) * uniform_ran() + min_val

      end function uniform_spread

c returns random number (integer) between min - max
      function uniform_int_spread(min_val,max_val)

      implicit none

      integer :: uniform_int_spread
      integer, intent(in) :: min_val, max_val

      uniform_int_spread = dint((max_val - min_val) * uniform_ran()) +
     1    min_val

      end function uniform_int_spread


c returns a normal distribution
      function normal_ran(mean,sigma)

      implicit none

      real :: normal_ran, tmp, fac, gsave, rsq, r1, r2
      real :: mean, sigma
      integer :: flag

      save flag, gsave

      data flag /0/

      if (flag.eq.0) then
      rsq=2.0
      do while ((rsq .ge. 1.0) .or. (rsq .eq. 0.0))
      r1=2.0*uniform_ran()-1.0
      r2=2.0*uniform_ran()-1.0
      rsq=r1*r1+r2*r2
      enddo
      fac=sqrt(-2.0*log(rsq)/rsq)
      gsave=r1*fac
      tmp=r2*fac
      flag=1
      else
      tmp=gsave
      flag=0
      endif
      normal_ran=tmp*sigma+mean
      end function normal_ran

      end module random
