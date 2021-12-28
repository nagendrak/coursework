      subroutine store_averages

      use data_all

      implicit none

      integer :: i, j, k, i_dir

c Traverse through each cell to compute averages
      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic
      node_avg(i,j,k,i_dt)%mass_den =
     1    (node_avg(i,j,k,i_dt)%mass_den*(i_realization-1)
     1    + node_array(i,j,k)%mass_den)/i_realization
      do i_dir = 1, 3
      node_avg(i,j,k,i_dt)%vel(i_dir) =
     1    (node_avg(i,j,k,i_dt)%vel(i_dir)*(i_realization-1)
     1    + node_array(i,j,k)%vel(i_dir))/i_realization
      end do
c end do i_dir = 1, 3
      node_avg(i,j,k,i_dt)%temp =
     1    (node_avg(i,j,k,i_dt)%temp*(i_realization-1)
     1    + node_array(i,j,k)%temp)/i_realization
      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc

      end subroutine store_averages
