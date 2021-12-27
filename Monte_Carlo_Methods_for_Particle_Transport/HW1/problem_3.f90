PROGRAM randDist

  IMPLICIT NONE

  INTEGER :: iter, bin(30), i, maxIter, unitProbFile
  REAL :: power, randNum, x, xBin, pdf(30)
  CHARACTER(80) :: nameProbFile

  ! Start

  PRINT*, 'Enter maximum number of iterations'
  READ*, maxIter

  CALL random_seed()

  power = 1./3.

  iter = 0
  bin(:) = 0

  DO WHILE (iter .lt. maxIter)

    iter = iter + 1

    CALL RANDOM_NUMBER(randNum)

    x = 3.0*randNum**(power)
    xBin = CEILING(x*10.0)

    bin(xBin) = bin(xBin) + 1

  END DO

  ! Compute probability of flush hand
  pdf(:) = 1.0*bin(:)/maxIter

  unitProbFile = 101
  nameProbFile = 'pdf_x2.dat'

  OPEN (UNIT = unitProbFile, FILE = nameProbFile,                       &
      POSITION = 'rewind', FORM = 'formatted', ACTION = 'write')

  DO i = 1, 30
    WRITE(unitProbFile,501) i, pdf(i)
  END DO

501 FORMAT (i3.3, 1X, 1(e12.5, 1X))

END PROGRAM randDist
