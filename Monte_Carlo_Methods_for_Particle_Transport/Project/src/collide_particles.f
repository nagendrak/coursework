      subroutine collide_particles(par_1,par_2)

      use data_all
      use random

      implicit none

      real :: vel_cm(3), phi, sin_phi, cos_phi, cos_theta, sin_theta,
     1    pi, rel_vel_mag, rel_vel(3)
      type (particle), pointer, intent(inout) :: par_1, par_2

      pi = dacos(-1.0)

c Hard-sphere model

c Center of mass velocity
      vel_cm(:) = 0.5*(par_1%vel(:) + par_2%vel(:))

c Estimation of azimuthal angle (phi)
      phi = 2.0*pi*uniform_ran()
      sin_phi = dsin(phi)
      cos_phi = dcos(phi)
      cos_theta = 2.0*uniform_ran()-1.0
      sin_theta = dsqrt(1.0 - cos_theta**2.0)

c Relative velocity after collision
      call calculate_magnitude((par_1%vel(:)-par_2%vel(:)),
     1    rel_vel_mag)
      rel_vel(1) = rel_vel_mag*(sin_theta*cos_phi)
      rel_vel(2) = rel_vel_mag*(sin_theta*sin_phi)
      rel_vel(3) = rel_vel_mag*(cos_theta)

c Post-collision velocities
      par_1%vel(:) = vel_cm(:) + 0.5*rel_vel(:)
      par_2%vel(:) = vel_cm(:) - 0.5*rel_vel(:)

      end subroutine collide_particles
