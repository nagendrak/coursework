!--------------------------------------------------------------------
! Program written by Nagendra Krishnamurthy
! Start date: 2011-09-19
! End date  : 2011-09-
! 
! Program should be compiled with ifort (Intel fortran compiler) or
! any other compiler with which the RANDOM_NUMBER and RANDOM_SEED
! functions work properly.
!--------------------------------------------------------------------

PROGRAM numericalInversion

  IMPLICIT NONE

  INTEGER :: iter, maxIter, unitProbFile, success, n, i,            &
      acceptFlag
  REAL :: pi, lowerLimit, upperLimit, delX, p_i
  REAL, ALLOCATABLE :: x(:), p(:)
  CHARACTER(80) :: nameProbFile

  ! Initialization
  CALL RANDOM_SEED()

  pi = DACOS(-1.0)

  lowerLimit = -2.0
  upperLimit = 2.0

  n = 1000

  ALLOCATE (x(0:n))
  ALLOCATE (p(0:n))

  x(0) = lowerLimit
  p(0) = DSQRT(2.0/pi)*DEXP(-x(0)*x(0)/2.0)
! p(1) = 1.0
  PRINT*, 'Point', x(0), p(0)
  DO i = 1, n
    acceptFlag = 0
    delX = (upperLimit-lowerLimit)/n
    DO WHILE (acceptFlag .EQ. 0)
      x(i) = x(i-1) + delX
      p(i) = DSQRT(2.0/pi)*DEXP(-x(i)*x(i)/2.0)
!     p(i) = 1.0
      p_i = 1.0/n/delX
      PRINT*, i, delX, p(i), p_i
      IF (((p_i .GE. p(i-1)) .AND. (p_i .LE. p(i))) .OR.            &
          ((p_i .LE. p(i-1)) .AND. (p_i .GE. p(i)))) THEN
        acceptFlag = 1
        PRINT*, 'accepted', i, p_i, p(i-1), p(i)
        PRINT*, 'Point', x(i), p(i)
      ELSE
        delX = 0.9*delX
        PRINT*, 'rejected', i, p_i, p(i-1), p(i), x(i-1), x(i)
      END IF
    END DO
  END DO

  DO i = 1, n
    PRINT*, i, x(i), p(i)
  END DO

END PROGRAM numericalInversion
