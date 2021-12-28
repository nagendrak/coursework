      module data_all

c mfp - mean free path of a molecule in a DILUTE GAS
c eff_dia - effective diameter of the molecule
c num_den - number density
c nic, njc, nkc - number of cells in each direction
c nif, njf, nkf - number of faces in each direction (one plus the
c     number of cells in that direction)
c t_w - temperature of the wall

      implicit none

c General flow properties
      real :: mfp, num_den, n_eff, eff_dia, temp_init, mol_mass,
     1    gas_den, mft, mol_wt, Kn, mpv

c Simulation parameters
      integer :: n_particles, i_dt, ndt, n_realizations,
     1    i_realization, n_total_coll
      real :: dt, time

c Global variables
      real :: boltzmann_k, avagadro_num
      integer :: n_bins
      integer, allocatable :: vel_bin_test1(:,:,:)

c Particle related datastructure
      type particle
        integer :: ijk(3), dom, glb_num
        real :: coord(3), vel(3), coord_nm1(3)
        type (particle), pointer :: next
      end type particle
      type (particle), pointer :: par_list

      type particle_bin
        type (particle), pointer :: par
        type (particle_bin), pointer :: next
      end type particle_bin

c Domain related datastructure
c boundary_type = 1 - periodic boundary
c               = 2 - thermal diffuse wall
c               = 3 - specular wall
      type domain
        integer :: boundary_type(3,2), ijk(3,2), n_wall_hits(3,2)
        real :: coord(3,2), t_w(3,2), vel_w(3,2,3), vol,
     1    del_vel_w(3,2,3), area(3,2), force(3,2,3),
     1    force_avg(3,2,3)
      end type domain
      type (domain), allocatable :: dom_array(:)
      integer :: n_domains

      type cell
        integer :: n_par
        real :: vol, mass_den, mom_den(3), vel(3),
     1    ener_den, temp, vel_mag, coord(3)
        type (particle_bin), pointer :: par_bin
      end type cell
      type (cell), allocatable :: node_array(:,:,:),
     1    node_avg(:,:,:,:)
      real, allocatable :: x(:,:,:), y(:,:,:), z(:,:,:)
      integer :: nic, njc, nkc, nif, njf, nkf

      contains

      subroutine insert_par_to_bin(bin_list,par_current)

      implicit none

      type (particle), pointer, intent(in) :: par_current
      type (particle_bin), pointer, intent(inout) :: bin_list
      type (particle_bin), pointer :: par_bin_temp, par_bin_current

      par_bin_temp => bin_list%next
      allocate(par_bin_current)
      nullify(par_bin_current%next)
      bin_list%next => par_bin_current
      if (associated(par_bin_temp)) then
      par_bin_current%next => par_bin_temp
      end if
      par_bin_current%par => par_current

      end subroutine insert_par_to_bin


      subroutine par_initialize(par_current)

      implicit none

      type (particle), intent(inout), pointer :: par_current

      par_current%ijk(:) = 0
      par_current%coord(:) = 0.0
      par_current%vel(:) = 0.0
      nullify(par_current%next)

      end subroutine par_initialize


      subroutine par_insert(par_current)

      implicit none

      type (particle), intent(inout), pointer :: par_current
      type (particle), pointer :: par_temp

      if (associated(par_current%next)) then
      par_temp => par_current%next
      allocate(par_current%next)
      par_current%next%next => par_temp
      else
      allocate(par_current%next)
      end if

      par_current => par_current%next
      call par_initialize(par_current)

      end subroutine par_insert


      subroutine calculate_magnitude(vec,vec_mag)

      implicit none

      real, intent(in) :: vec(:)
      real, intent(out) :: vec_mag

      vec_mag = dsqrt(vec(1)**2.0 + vec(2)**2.0 + vec(3)**2.0)

      end subroutine calculate_magnitude


      end module data_all
