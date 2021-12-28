      program dsmc_solver

      use data_all

      implicit none

      integer :: count_bin, count_par, i, j, k
      type (particle), pointer :: par_current
      type (particle_bin), pointer :: par_bin_current

      call open_inout_files

      call read_input

      call setup_indices

      call allocate_arrays

      call generate_grid

      do i_realization = 1, n_realizations

      write(8,*) 'This is realization number:', i_realization
      n_total_coll = 0
      dom_array(1)%n_wall_hits(:,:) = 0

      call initialize

      call initial

      do i_dt = 1, ndt

      time = i_dt*dt
c     write(6,*) 'This is time-step', i_dt

c Move the particles to new locations
d     write(8,*) 'Entering free_flight'
      call free_flight

c Compute collisions for the time-step
d     write(8,*) 'Entering compute_collisions'
      call compute_collisions

c Compute average to determine macroscopic properties
d     write(8,*) 'Entering compute_averages'
      call compute_averages

c Store average values for averaging over several realizations
d     write(8,*) 'Entering store_averages'
      call store_averages

      if (mod(i_dt,100) .eq. 0) then
      write(8,*) 'This is time-step', i_dt
      write(8,*) 'Total collisions so far:', 
     1    n_total_coll
      write(8,*) 'Forces on walls:', dom_array(1)%force(3,:,2)
      end if

      end do
c end do i_dt = 1, ndt

      write(8,*) 'Total collisions in realization:', i_realization,
     1    n_total_coll
      write(8,*) 'Number of wall hits:', dom_array(1)%n_wall_hits(:,:)
      write(8,*) 'Forces on walls in realization:',
     1    dom_array(1)%force(3,:,2)

      end do
c end do i_realization = 1, n_realizations

      write(8,*) 'Average forces on walls:',
     1    dom_array(1)%force_avg(3,:,2)
      write(8,*) 'Pressure on the walls:',
     1    dom_array(1)%force_avg(3,1,3)/dom_array(1)%area(3,1),
     1    dom_array(1)%force_avg(3,2,3)/dom_array(1)%area(3,2)

c Extrapolate to find fluid velocity at the diffuse wall
      open (unit = 101, file = 'fluid_vel_wall.dat', action = 'write',
     1    status = 'replace', position = 'rewind')

      do i_dt = 1, ndt
      i = 1; j = 1
      write(6,*) node_avg(i,j,1,i_dt)%vel(2),
     1    node_avg(i,j,2,i_dt)%vel(2)
      write(101,*) i_dt*dt/mft, ((node_avg(i,j,1,i_dt)%vel(2)
     1    *(node_array(i,j,2)%coord(3)-z(i,j,1))
     1    - (node_avg(i,j,2,i_dt)%vel(2)*(node_array(i,j,1)%coord(3)
     1    - z(i,j,1))))
     1    /(node_array(i,j,2)%coord(3)-node_array(i,j,1)%coord(3)))
     1    /dom_array(1)%vel_w(3,1,2)
      end do
c end do i_dt = 1, ndt

      close (101)

      call write_tecplot(1)
      call write_tecplot(2)
      call write_tecplot(3)

 501  format(6(1pe12.5,1x))

      end program dsmc_solver
