      subroutine open_inout_files

      use data_all

      implicit none

      open(unit=8, file='dsmc.out', status='replace',
     1    form='formatted', access='sequential',
     1    action='write')

      end subroutine open_inout_files
