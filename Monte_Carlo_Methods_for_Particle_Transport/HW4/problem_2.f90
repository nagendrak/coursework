!--------------------------------------------------------------------
!
! Program written by Nagendra Krishnamurthy
! Start date: 2011-10-02
! End date  : 2011-10-
! 
!--------------------------------------------------------------------

PROGRAM meanPathLength

  IMPLICIT NONE

  INTEGER(KIND=8) :: maxIter, iter, multiplier, constant, seed,     &
      divisor, previousRN
  INTEGER(KIND=8), ALLOCATABLE :: randNum(:)
  INTEGER :: unitResultsFile
  REAL :: fRandNum, sigmaTotal, pathLength, avgPathLength
  CHARACTER(80) :: nameResultsFile

  maxIter = 10000000
  sigmaTotal = 1.0
  PRINT*, 'Enter multiplier, constant and seed'
  READ*, multiplier, constant, seed

  divisor = 2**31 - 1
  PRINT*, 'Divisor:', divisor

  avgPathLength = 0.0
  ALLOCATE(randNum(maxIter))
  previousRN = seed
  
  DO iter = 1, maxIter
    CALL linCongRNG(randNum(iter),previousRN,multiplier,constant,   &
        divisor)
    previousRN = randNum(iter)
    fRandNum = 1.0*randNum(iter)/divisor
    pathLength = -LOG(fRandNum)/sigmaTotal
    avgPathLength = avgPathLength + pathLength
  END DO

  avgPathLength = avgPathLength/maxIter

  unitResultsFile = 101
  nameResultsFile = 'problem_2_avgPathLength.dat'

  OPEN (UNIT = unitResultsFile, FILE = nameResultsFile,                       &
      POSITION = 'append', FORM = 'formatted', ACTION = 'write')
  WRITE(unitResultsFile,501) avgPathLength

  CLOSE(unitResultsFile)

501 FORMAT (1(e12.5, 1X))

END PROGRAM meanPathLength

SUBROUTINE linCongRNG(xkp1,xk,a,b,M)

  IMPLICIT NONE

  INTEGER(KIND=8), INTENT(IN) :: xk, a, b, M
  INTEGER(KIND=8), INTENT(OUT) :: xkp1

  xkp1 = MOD((xk*a + b),M)

END SUBROUTINE linCongRNG
