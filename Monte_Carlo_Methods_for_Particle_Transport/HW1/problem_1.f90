PROGRAM sumDice

  INTEGER :: iter, maxIter, n1, n2, sum, unitPdfFile
  REAL :: ranNum_1, ranNum_2, pdfBin(2:12)
  CHARACTER(80) :: namePdfFile

  ! Start

  PRINT*, 'Enter the maximum number of iterations:'
  READ*, maxIter

  CALL random_seed()

  iter = 0

  DO WHILE (iter .le. maxIter)

  ! Increment iteration number

  iter = iter + 1

  ! Generate two random numbers
  CALL random_number(ranNum_1)
  CALL random_number(ranNum_2)

  ! Compute values for n1 and n1 based on the random numbers
  n1 = INT(6.*ranNum_1) + 1
  n2 = INT(6.*ranNum_2) + 1

  ! Computer sum of the two die values
  sum = n1 + n2

  ! Bin the sum for computation of pdf
  pdfBin(sum) = pdfBin(sum) + 1

  END DO

  ! Computer pdf by dividing the bin by total number of iterations
  pdfBin(:) = pdfBin(:)/maxIter

  unitPdfFile = 101
  namePdfFile = 'pdf.dat'

  OPEN (UNIT = unitPdfFile, FILE = namePdfFile, STATUS = 'replace',  &
      POSITION = 'rewind', FORM = 'formatted', ACTION = 'write')

  DO i = 2, 12
    WRITE(unitPdfFile,501) i, pdfBin(i)
  END DO

501 FORMAT (i3.3, 1X, 1(pe12.5, 1X))

END PROGRAM sumDice
