!--------------------------------------------------------------------
! Program written by Nagendra Krishnamurthy
! Start date: 2011-09-19
! End date  : 2011-09-20
! 
! Program should be compiled with ifort (Intel fortran compiler) or
! any other compiler with which the RANDOM_NUMBER and RANDOM_SEED
! functions work properly.
!--------------------------------------------------------------------

PROGRAM probMixing

  IMPLICIT NONE

  INTEGER :: iter, maxIter, unitProbFile, bin(100), i
  REAL :: randNum1, randNum2, pdf(100), xBin, x
  CHARACTER(80) :: nameProbFile

  ! Initialization

  CALL RANDOM_SEED()

  PRINT*, 'Enter number of samples'
  READ*, maxIter

  DO iter = 1, maxIter
    
    ! Generate random numbers - randNum1 and randNum2
    CALL RANDOM_NUMBER(randNum1)
    CALL RANDOM_NUMBER(randNum2)

    IF (randNum1 .LT. 0.8) THEN
      x = randNum2
    ELSE
      x = DSQRT(1.0 - (1.0 - randNum2)**0.5)
    END IF
    xBin = CEILING(x*100.0)

    bin(xBin) = bin(xBin) + 1

  END DO

  pdf(:) = 1.0*bin(:)/maxIter

  unitProbFile = 101
  nameProbFile = 'pdf_prob_mixing.dat'

  OPEN (UNIT = unitProbFile, FILE = nameProbFile,                       &
      POSITION = 'rewind', FORM = 'formatted', ACTION = 'write')

  DO i = 1, 100
    WRITE(unitProbFile,501) 0.01*i-0.005, pdf(i)
  END DO

501 FORMAT (2(e12.5, 1X))


END PROGRAM probMixing
