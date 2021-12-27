PROGRAM flushPokerHand

  IMPLICIT NONE

  TYPE card
    INTEGER :: cardNum, cardType
    TYPE (card), POINTER :: next
  END TYPE card

  INTEGER :: iter, maxIter, c1, unitProbFile, i, nCards,              &
      successCount, iCard
  REAL :: ranNum, probFlush
  CHARACTER(80) :: nameProbFile

  TYPE (card), POINTER :: cardList, cardCurrent, cardLast, handList,  &
      handCurrent, handLast, cardTemp

  ! Start

  PRINT*, 'Enter maximum number of iterations'
  READ*, maxIter

  successCount = 0
  nCards = 5
  CALL random_seed()

  ALLOCATE(cardList)
  NULLIFY(cardList%next)
  cardLast => cardList

  ALLOCATE(handList)
  NULLIFY(handList%next)
  handLast => handList

  iter = 0

  DO WHILE (iter .lt. maxIter)

300 CONTINUE

    CALL deleteList(cardList)
    cardLast => cardList
    CALL deleteList(handList)
    handLast => handList

    ! Increment iteration number
    iter = iter + 1

    ! Create a list of cards
    DO iCard = 1, 52
   
      ALLOCATE(cardLast%next)
      NULLIFY(cardLast%next%next)
   
      cardLast => cardLast%next
      cardLast%cardNum = iCard
      cardLast%cardType = CEILING(iCard/13.0)
   
    END DO

    DO i = 1, nCards
   
      ! Generate random number
      CALL random_number(ranNum)
     
      ! Compute values for c1 based on the random numbers
      c1 = INT((52.0-i+1)*ranNum) + 1
     
      ! Find the card
      cardCurrent => cardList
      DO iCard = 1, c1
        cardCurrent => cardCurrent%next
      END DO
     
      ! Append card to handList
      ALLOCATE(handLast%next)
      NULLIFY(handLast%next%next)
     
      handLast => handLast%next
      handLast%cardNum = cardCurrent%cardNum
      handLast%cardType = cardCurrent%cardType
     
      ! Remove card from cardList
      cardCurrent => cardList
      DO iCard = 1, c1-1
        cardCurrent => cardCurrent%next
      END DO
      cardTemp => cardCurrent%next
      cardCurrent%next => cardCurrent%next%next
      DEALLOCATE(cardTemp)
     
      ! Check if all cards in hand are of same suit
      handCurrent => handList
      DO WHILE (ASSOCIATED(handCurrent%next%next))
        IF (handCurrent%next%cardType .NE.                                &
            handCurrent%next%next%cardType) THEN
          GOTO 300
        END IF
        handCurrent => handCurrent%next
      END DO

    END DO

    ! Success
    successCount = successCount + 1
    handCurrent => handList
    DO WHILE (ASSOCIATED(handCurrent%next))
      handCurrent => handCurrent%next
    END DO

  END DO

  ! Compute probability of flush hand
  probFlush = 1.0*successCount/maxIter

  unitProbFile = 101
  nameProbFile = 'probFlush.dat'

  OPEN (UNIT = unitProbFile, FILE = nameProbFile,                       &
      POSITION = 'append', FORM = 'formatted', ACTION = 'write')

  WRITE(unitProbFile,501) maxIter, probFlush

501 FORMAT (i7.7, 1X, 1(e12.5, 1X))

CONTAINS

SUBROUTINE printList(list)

  IMPLICIT NONE

  TYPE (card), POINTER, INTENT(IN) :: list
  TYPE (card), POINTER :: current

  current => list
  DO WHILE (ASSOCIATED(current%next))
    current => current%next
    PRINT*, current%cardNum, current%cardType
  END DO

END SUBROUTINE printList

SUBROUTINE deleteList(list)

  IMPLICIT NONE

  TYPE (card), POINTER, INTENT(IN) :: list
  TYPE (card), POINTER :: current, previous

  current => list%next
  DO WHILE (ASSOCIATED(current))
    previous => current
    current => current%next
    DEALLOCATE(previous)
  END DO
  NULLIFY(list%next)

END SUBROUTINE deleteList

END PROGRAM flushPokerHand
