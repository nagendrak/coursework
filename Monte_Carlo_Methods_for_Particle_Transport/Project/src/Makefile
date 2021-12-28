SHELL=/bin/sh#
# This makefile was produced by /usr/bin/fmgen at 08:17:19 AM on 09/04/92
# If it is invoked by the command line
#	make -f Makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named new and
# an executable which profiles named new.prof.
#
# To remove all the object and but leave the executables use the command line
#	make -f Makefile clean
#
# To remove everything but the source files use the command line
#	make -f Makefile clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f Makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#

SRCS=\
01.data.f\
	02.random.f\
advection.f\
	allocate_arrays.f\
	bin_particles.f\
	boundary_interaction.f\
	check_par_out_of_bounds.f\
	collide_particles.f\
  compute_averages.f\
  compute_collisions.f\
	compute_max_rel_vel.f\
	delete_bin_particles.f\
  dsmc_solver.f\
  free_flight.f\
  generate_grid.f\
	initial.f\
  initialize.f\
	locate_ijk.f\
	maxwellian_dist.f\
	open_inout_files.f\
	par_initial.f\
	read_input.f\
	setup_indices.f\
	store_averages.f\
	write_tecplot.f\

OBJS= \
01.data.o\
	02.random.o\
advection.o\
	allocate_arrays.o\
	bin_particles.o\
	boundary_interaction.o\
	check_par_out_of_bounds.o\
	collide_particles.o\
  compute_averages.o\
  compute_collisions.o\
	compute_max_rel_vel.o\
	delete_bin_particles.o\
  dsmc_solver.o\
  free_flight.o\
  generate_grid.o\
	initial.o\
  initialize.o\
	locate_ijk.o\
	maxwellian_dist.o\
	open_inout_files.o\
	par_initial.o\
	read_input.o\
	setup_indices.o\
	store_averages.o\
	write_tecplot.o\


#
# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
# Location of the HDF5 1.4.2  Library
HDF5=/afs/ncsa/packages/hdf5/5-1.4.2-patch1-linux2.2.18
FORTRANLIB= $(HDF5)/lib/libhdf5_fortran.a

# Location of GZIP Library
LIBZ= /afs/.ncsa.uiuc.edu/packages/hdf/4.1r5-linux/lib

# define mpichgm home
MPIHOME=/usr/apps/tools/unsupported/mpich/gm

#FC  =            /nfs/compilers/mpich-1.2.5/bin/mpif77
FC  = 		    ifort	
#FFLAGS =        -O3 -q64 -qwarn64 -qautodbl=dbl4 -qalign=4k 
FFLAGS =        -O3 -r8 -traceback
LDFLAGS =        -O3 -r8 -traceback

DEBUG ?= 0
ifeq ($(DEBUG),1)
FFLAGS =        -O0 -r8 -check all -traceback -fpe0 -g
LDFLAGS =        -O0 -r8 -check all -traceback -fpe0 -g
else
ifeq ($(DEBUG),2)
FFLAGS =        -O0 -r8 -check all -traceback -fpe0 -g -d_lines
LDFLAGS =        -O0 -r8 -check all -traceback -fpe0 -g -d_lines
endif
endif

#FFLAGS =        -O3 -r8 -check all -traceback -fpe0
#FFLAGS =        -O3 -r8 -check all -traceback -fpe0 -g -d_lines
#FFLAGS =        -O3 -r8 -d_lines -check all
#LDFLAGS =       -O3 -q64 -qwarn64 -qautodbl=dbl4 -qalign=4k
#LDFLAGS =        -O3 -r8 -check all -traceback -fpe0
#LDFLAGS =        -O3 -r8 -check all -traceback -fpe0 -g -d_lines
#LDFLAGS =        -O3 -r8 -check all -d_lines
#LIBSHDF =      $(FORTRANLIB) $(HDF5)/lib/libhdf5.a
LIB  =          -lmpi
#INCLUDE_DIR =  -I/usr/local/vmi/mpich/include
INCLUDE_DIR  =
CMD =            dsmc.solver.x   
PROFLIB =

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build new.
#
all:            $(CMD)

$(CMD):           $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS)
# Make the profiled version of the command and call it new.prof
#
#alloc.o:        alloc.f
#	$(FC) -c -r8 -O0 alloc.f

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD)

#void:		clobber
# 	-rm -f $(SRCS) Makefile
