      subroutine write_tecplot(i_plot)

c i_plot = 1 - velocity distribution
c        = 2 - instantaneous solution
c        = 3 - averaged solution

      use data_all

      implicit none

      integer, intent(in) :: i_plot
      integer :: i, j, k, idt, vel_bin_idx, i_bin, vel_bin(n_bins)
      real :: vel_mag, vel_max
      type (particle), pointer :: par_current

      select case (i_plot)

      case (1)

      vel_bin(:) = 0
      vel_max = 3.0*dsqrt(2.0*boltzmann_k*temp_init/mol_mass)

      par_current => par_list%next

      do while (associated(par_current))

      call calculate_magnitude(par_current%vel,vel_mag)
      vel_bin_idx = dint(vel_mag/vel_max*n_bins) + 1
      if (vel_bin_idx .gt. n_bins) vel_bin_idx = n_bins
      vel_bin(vel_bin_idx) = vel_bin(vel_bin_idx) + 1
      par_current => par_current%next
      end do
c end do while (associated(par_current))

      open (unit = 101, file = 'speed_dist.dat', action = 'write',
     1    status = 'replace', position = 'rewind')

      do i_bin = 1, n_bins
      write(101,504) (i_bin-0.5)/n_bins*vel_max, vel_bin(i_bin)
      end do
c end do i_bin = 1, n_bins

      close (101)

      open (unit = 102, file = 'speed_dist_specular_wall.dat',
     1    action = 'write', status = 'replace', position = 'rewind')

      do i_bin = 1, n_bins
      write(102,505) (i_bin-0.5)/n_bins*vel_max,
     1    vel_bin_test1(i_bin,1,2), vel_bin_test1(i_bin,2,2)
      end do
c end do i_bin = 1, n_bins

      close (102)

      open (unit = 103, file = 'speed_dist_diffuse_wall.dat',
     1    action = 'write', status = 'replace', position = 'rewind')

      do i_bin = 1, n_bins
      write(103,505) (i_bin-0.5)/n_bins*vel_max,
     1    vel_bin_test1(i_bin,1,1), vel_bin_test1(i_bin,2,1)
      end do
c end do i_bin = 1, n_bins

      close (103)


      case (2)

      open (unit = 102, file = 'solution.dat', action = 'write',
     1    status = 'replace', position = 'rewind')

      write(102,501) 'Instataneous solution', time
      write(102,502) 1, nic, njc, nkc

      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic
      write(102,503) node_array(i,j,k)%coord(:),
     1    node_array(i,j,k)%mass_den,
     1    node_array(i,j,k)%vel(:), node_array(i,j,k)%temp
      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc

      close (102)

      case (3)

      open (unit = 103, file = 'solution_avg.dat', action = 'write',
     1    status = 'replace', position = 'rewind')

      write(103,501) 'Average solution', time
      write(103,502) 1, nic, njc, nkc

      idt = ndt
      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic
      write(103,503) node_array(i,j,k)%coord(:),
     1    node_avg(i,j,k,idt)%mass_den,
     1    node_avg(i,j,k,idt)%vel(:), node_avg(i,j,k,idt)%temp
      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc

      close (103)

      end select

 501  format ('TITLE = "',A70, 1pe12.5, '"',/, 'VARIABLES = "x"',/,
     1    '"y"',/, '"z"',/,'"rho"',/,'"u"',/,'"v"',/,'"w"',/,'"t"')
 502  format (1x, 'ZONE T = "zone', I3, '",', 'I = ', I3, ',',
     1    'J = ', I3, ',', 'K = ', I3, ',', 'ZONETYPE = Ordered, ',/,
     1    'DATAPACKING = POINT')
 503  format (1x, 8(1pe12.5,2x))
 504  format (1x, 1(1pe12.5,2x), I9)
 505  format (1x, 1(1pe12.5,2x), 2(I9))

      end subroutine write_tecplot
