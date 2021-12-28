      subroutine generate_grid

      use data_all

      implicit none

      integer :: i, j, k
      real :: max_del_z, del_x, del_y, del_z

c Set the cell-size in the z-direction to be 75% of the
c mean-free-path.
      max_del_z = 0.75*mfp

      del_z = (dom_array(1)%coord(3,2)-dom_array(1)%coord(3,1))/nkc

      if (del_z .gt. max_del_z) then
      write(8,*) 'Warning: Grid size in z-direction more than 0.75mfp'
      write(8,*) 'Current, max allowable:', del_z, max_del_z
      end if

c Set cell-size in the other two directions. Since one-dimensional
c flow is being considered their values will not have limit based on
c mfp
      del_x = (dom_array(1)%coord(1,2)-dom_array(1)%coord(1,1))/nic
      del_y = (dom_array(1)%coord(2,2)-dom_array(1)%coord(2,1))/njc

c Setting number of faces
      nif = nic + 1; njf = njc + 1; nkf = nkc + 1

c Compute the vertex x,y,z's
      do k = 1, nkf
      do j = 1, njf
      do i = 1, nif
      x(i,j,k) = dom_array(1)%coord(1,1) + (i-1)*del_x
      y(i,j,k) = dom_array(1)%coord(2,1) + (j-1)*del_y
      z(i,j,k) = dom_array(1)%coord(3,1) + (k-1)*del_z
      end do
c end do i = 1, nif
      end do
c end do j = 1, njf
      end do
c end do k = 1, nkf

c Compute the nodal x,y,z's
      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic

      node_array(i,j,k)%coord(1) = 0.125*
     1    ( x(i,j,k) + x(i+1,j,k) + x(i,j+1,k) + x(i+1,j+1,k)
     1    + x(i,j,k+1) + x(i+1,j,k+1) + x(i,j+1,k+1) + x(i+1,j+1,k+1))
      node_array(i,j,k)%coord(2) = 0.125*
     1    ( y(i,j,k) + y(i+1,j,k) + y(i,j+1,k) + y(i+1,j+1,k)
     1    + y(i,j,k+1) + y(i+1,j,k+1) + y(i,j+1,k+1) + y(i+1,j+1,k+1))
      node_array(i,j,k)%coord(3) = 0.125*
     1    ( z(i,j,k) + z(i+1,j,k) + z(i,j+1,k) + z(i+1,j+1,k)
     1    + z(i,j,k+1) + z(i+1,j,k+1) + z(i,j+1,k+1) + z(i+1,j+1,k+1))

      allocate(node_array(i,j,k)%par_bin)
      nullify(node_array(i,j,k)%par_bin%next)

c Assume cartesian grid
      node_array(i,j,k)%vol = (x(i+1,j,k) - x(i,j,k))*
     1    (y(i,j+1,k) - y(i,j,k)) * (z(i,j,k+1) - z(i,j,k))

      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc

c Compute the nodal volumes
      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic

      node_array(i,j,k)%coord(1) = 0.125*
     1    ( x(i,j,k) + x(i+1,j,k) + x(i,j+1,k) + x(i+1,j+1,k)
     1    + x(i,j,k+1) + x(i+1,j,k+1) + x(i,j+1,k+1) + x(i+1,j+1,k+1))
      node_array(i,j,k)%coord(2) = 0.125*
     1    ( y(i,j,k) + y(i+1,j,k) + y(i,j+1,k) + y(i+1,j+1,k)
     1    + y(i,j,k+1) + y(i+1,j,k+1) + y(i,j+1,k+1) + y(i+1,j+1,k+1))
      node_array(i,j,k)%coord(3) = 0.125*
     1    ( z(i,j,k) + z(i+1,j,k) + z(i,j+1,k) + z(i+1,j+1,k)
     1    + z(i,j,k+1) + z(i+1,j,k+1) + z(i,j+1,k+1) + z(i+1,j+1,k+1))

      allocate(node_array(i,j,k)%par_bin)
      nullify(node_array(i,j,k)%par_bin%next)

      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc

c Traverse through each cell to compute averages
      do k = 1, nkc
      do j = 1, njc
      do i = 1, nic
      node_avg(i,j,k,:)%mass_den = 0.0
      node_avg(i,j,k,:)%vel(1) = 0.0
      node_avg(i,j,k,:)%vel(2) = 0.0
      node_avg(i,j,k,:)%vel(3) = 0.0
      node_avg(i,j,k,:)%temp = 0.0
      end do
c end do i = 1, nic
      end do
c end do j = 1, njc
      end do
c end do k = 1, nkc


      end subroutine generate_grid
