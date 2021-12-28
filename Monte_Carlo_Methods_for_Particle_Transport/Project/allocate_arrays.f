      subroutine allocate_arrays

      use data_all

      implicit none

      allocate(x(nif,njf,nkf), y(nif,njf,nkf), z(nif,njf,nkf))
      allocate(node_avg(nic,njc,nkc,ndt))

      allocate(vel_bin_test1(n_bins,2,2))

      end subroutine allocate_arrays
