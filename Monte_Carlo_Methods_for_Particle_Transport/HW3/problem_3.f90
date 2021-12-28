!--------------------------------------------------------------------
! Program written by Nagendra Krishnamurthy
! Start date: 2011-09-19
! End date  : 2011-09-
! 
! Program should be compiled with ifort (Intel fortran compiler) or
! any other compiler with which the RANDOM_NUMBER and RANDOM_SEED
! functions work properly.
!--------------------------------------------------------------------

PROGRAM findSphereVolume

  IMPLICIT NONE

  INTEGER :: iter, maxIter, unitProbFile, success
  REAL :: randNum1, randNum2, randNum3, probSuccess, diameter,      &
      rad, x, y, z, sphereVolume
  CHARACTER(80) :: nameProbFile

  ! Initialization
  success = 0

  CALL RANDOM_SEED()

  PRINT*, 'Enter sphere diameter'
  READ*, diameter
  PRINT*, 'Enter max iterations'
  READ*, maxIter

  DO iter = 1, maxIter
    
    ! Generate random numbers - randNum1 and randNum2
    CALL RANDOM_NUMBER(randNum1)
    CALL RANDOM_NUMBER(randNum2)
    CALL RANDOM_NUMBER(randNum3)

    x = diameter*randNum1 - 0.5*diameter
    y = diameter*randNum2 - 0.5*diameter
    z = diameter*randNum3 - 0.5*diameter

    rad = DSQRT(x**2.+ y**2. + z**2.)

    IF (rad .LE. 0.5*diameter) success = success + 1

  END DO

  probSuccess = 1.0*success/maxIter
  sphereVolume = probSuccess*(diameter**3.0)

  unitProbFile = 101
  nameProbFile = 'sphereVolume.dat'

  OPEN (UNIT = unitProbFile, FILE = nameProbFile,                       &
      POSITION = 'append', FORM = 'formatted', ACTION = 'write')

  WRITE(unitProbFile,501) maxIter, sphereVolume

501 FORMAT (i8.8, 1X, 1(e12.5, 1X))


END PROGRAM findSphereVolume
