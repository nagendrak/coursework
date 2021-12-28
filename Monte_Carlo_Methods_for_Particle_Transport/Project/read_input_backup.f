      subroutine read_input

      use data_all

      implicit none

      real :: pi

d     write(8,*) 'read_input in debug mode'

      nic = 1; njc = 1; nkc = 50
      ndt = 10
      n_realizations = 500
      n_particles = 50000
      temp_init = 300.0

      n_domains = 1

      allocate(dom_array(n_domains), node_array(nic,njc,nkc))

      dom_array(1)%coord(1,1) = 0.0; dom_array(1)%coord(1,2) = 1.0e-9
      dom_array(1)%coord(2,1) = 0.0; dom_array(1)%coord(2,2) = 1.0e-9
      dom_array(1)%coord(3,1) = 0.0; dom_array(1)%coord(3,2) = 0.5e-9

      dom_array(1)%vol = 
     1    (dom_array(1)%coord(1,2)-dom_array(1)%coord(1,1))*
     1    (dom_array(1)%coord(2,2)-dom_array(1)%coord(2,1))*
     1    (dom_array(1)%coord(3,2)-dom_array(1)%coord(3,1))

      dom_array(1)%ijk(1,1) = 1; dom_array(1)%ijk(1,2) = nic
      dom_array(1)%ijk(2,1) = 1; dom_array(1)%ijk(2,2) = njc
      dom_array(1)%ijk(3,1) = 1; dom_array(1)%ijk(3,2) = nkc

      dom_array(1)%boundary_type(1,1) = 1
      dom_array(1)%boundary_type(1,2) = 1
      dom_array(1)%boundary_type(2,1) = 1
      dom_array(1)%boundary_type(2,2) = 1
      dom_array(1)%boundary_type(3,1) = 2
      dom_array(1)%boundary_type(3,2) = 3

      dom_array(1)%t_w(:,:) = 0.0
      dom_array(1)%t_w(3,1) = 600.0

      dom_array(1)%vel_w(:,:,:) = 0.0
      dom_array(1)%vel_w(3,1,2) = 00.0

      pi = dacos(-1.0)

c This mean free path formulation is for an equilibrium gas - refer to
c Bird (1976) pp 17.
      boltzmann_k = 1.3806503e-23

c Number density of any gas under standard conditions (1 atm pressure
c and 0 C).
      gas_den = 1.01
      mol_wt = 29.0E-3
      avagadro_num = 6.0221415E23
      n_eff = gas_den*avagadro_num*dom_array(1)%vol/mol_wt/n_particles
c     num_den = 2.68699e+25
      Kn = 0.05

c Effective diameter for air is obtained by substituing the measure
c value of the coefficient of viscosity into the theoretical result
c for hard-sphere molecules. NOTE that the mean molecular spacing
c under standard conditions is obtained to 3.3e-9, which just about
c satisfies the dilute gas condition.
      eff_dia = 3.7e-10

c     mfp = 1.0/dsqrt(2.0)/pi/eff_dia**2./num_den
      mfp = Kn*eff_dia

c     mol_mass = gas_den/num_den
      mol_mass = mol_wt/avagadro_num
c Setting mass as mass of the congregation of molecules
c     write(8,*) 'Warning: mass is mass of a particle, not molecule'
c     mol_mass = n_eff*mol_mass
      mft = mfp/dsqrt(3.0*boltzmann_k/mol_mass)
      dt = 0.01*mft

      end subroutine read_input
