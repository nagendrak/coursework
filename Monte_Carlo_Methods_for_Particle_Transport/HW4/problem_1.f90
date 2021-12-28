!--------------------------------------------------------------------
!
! Program written by Nagendra Krishnamurthy
! Start date: 2011-10-02
! End date  : 2011-10-
! 
!--------------------------------------------------------------------

PROGRAM chi_squared_test

  IMPLICIT NONE

  INTEGER(KIND=8) :: maxIter, iter, multiplier, constant, seed,     &
      divisor, previousRN
  INTEGER(KIND=8), ALLOCATABLE :: randNum(:)
  INTEGER :: nbins, iBin, unitResultsFile
  INTEGER, ALLOCATABLE :: bin(:)
  REAL :: fRandNum, chi_squared, moment, momOrder
  CHARACTER(80) :: nameResultsFile

  PRINT*, 'Enter number of random numbers'
  READ*, maxIter
  PRINT*, 'Enter multiplier, constant and seed'
  READ*, multiplier, constant, seed

  divisor = 2**31 - 1
  divisor = divisor + 1
  PRINT*, 'Divisor:', divisor

  chi_squared = 0.0
  ALLOCATE(randNum(maxIter))
  previousRN = seed
  nBins = 10
  ALLOCATE(bin(nBins))
  bin(:) = 0
  momOrder = 5.0
  moment = 0.0
  
  DO iter = 1, maxIter
    CALL linCongRNG(randNum(iter),previousRN,multiplier,constant,   &
        divisor)
    previousRN = randNum(iter)
    fRandNum = 1.0*randNum(iter)/divisor
!   iBin = CEILING(fRandNum*1.0*nBins)
    iBin = FLOOR(fRandNum*1.0*nBins) + 1
    bin(iBin) = bin(iBin) + 1
    moment = moment + fRandNum**momOrder
  END DO

  moment = moment/maxIter

  DO iBin = 1, nBins
    chi_squared = chi_squared                                       &
        + (bin(iBin) - maxIter*1.0/nBins)**2./maxIter/(1.0/nBins)
  PRINT*, chi_squared
  END DO
  PRINT*, bin(:)

  unitResultsFile = 101
  nameResultsFile = 'problem_1_chi_squared.dat'

  OPEN (UNIT = unitResultsFile, FILE = nameResultsFile,                       &
      POSITION = 'append', FORM = 'formatted', ACTION = 'write')
  WRITE(unitResultsFile,501) chi_squared

  CLOSE(unitResultsFile)

  unitResultsFile = 102
  nameResultsFile = 'problem_1_moment.dat'

  OPEN (UNIT = unitResultsFile, FILE = nameResultsFile,                       &
      POSITION = 'append', FORM = 'formatted', ACTION = 'write')
  WRITE(unitResultsFile,501) moment

  CLOSE(unitResultsFile)

501 FORMAT (1(e12.5, 1X))

END PROGRAM chi_squared_test

SUBROUTINE linCongRNG(xkp1,xk,a,b,M)

  IMPLICIT NONE

  INTEGER(KIND=8), INTENT(IN) :: xk, a, b, M
  INTEGER(KIND=8), INTENT(OUT) :: xkp1

  xkp1 = MOD((xk*a + b),M)

END SUBROUTINE linCongRNG
