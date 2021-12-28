      subroutine initialize

      use data_all

      implicit none

      integer :: i, j, k
      type (particle), pointer :: par_current

      call random_seed()

      if (.not. associated(par_list)) then
      allocate(par_list)
      else
      do while (associated(par_list%next))
      par_current => par_list%next
      par_list%next => par_list%next%next
      deallocate(par_current)
      end do
      nullify(par_list%next)
      end if
      call par_initialize(par_list)

      end subroutine initialize
