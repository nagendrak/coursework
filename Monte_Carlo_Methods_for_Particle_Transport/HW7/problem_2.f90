MODULE allData

  TYPE shield
    REAL :: sigmaTotal, sigmaAbs, xMin, xMax, absRatio, thickness,  &
        importance
  END TYPE shield

  TYPE (shield), ALLOCATABLE :: shld(:,:)

  TYPE particle
    INTEGER :: region, subRegion
    REAL :: scatAngle, xLoc, weight
    TYPE (particle), POINTER :: next
  END TYPE particle

  TYPE (particle), POINTER :: parList, parListTail

  INTEGER :: nParticles, nShields, parStatus, nParAbs, nParRef,     &
      nParLeak, currentRegion, collStatus, freeFlightFlag,          &
      nSubRegions, currentSubRegion, oldRegion, oldSubRegion
  REAL :: xInit, scatterAngleInit(3), xLocation, scatterAngle,      &
      parRef, parLeak, parAbs, parWeight, weightCutoff
  REAL :: probAbs(3), probRef(3), probLeak(3),                      &
      relativeErrorLeak(3), relativeErrorRef(3),                    &
      relativeErrorAbs(3), FOMLeak(3), FOMRef(3), FOMAbs(3),        &
      varianceLeak(3), varianceRef(3), varianceAbs(3)

END MODULE allData


PROGRAM multiRegionShield

  USE allData

  IMPLICIT NONE

  INTEGER :: iParticle, iShield, unitProbFile, iScatterAngle,       &
      rrd, flag, numParLeft
  REAL :: pi, probLeakSq, probRefSq, probAbsSq, timeIn, timeOut,    &
      totalTime, maxRelativeError
  CHARACTER(80) :: nameProbFile

  TYPE (particle), POINTER :: parCurrent, parTemp

  INTERFACE

    SUBROUTINE insertPar(parCurrent,wt)

    USE allData
    TYPE (particle), POINTER, INTENT(IN) :: parCurrent
    REAL, INTENT (IN) :: wt

    END SUBROUTINE insertPar

  END INTERFACE

  ! Initialization
  xInit = 0.0
  pi = DACOS(-1.0)

  CALL RANDOM_SEED()

  nShields = 1
  nSubRegions = 2
  ALLOCATE(shld(nShields,nSubRegions))

  ALLOCATE(parList)
  NULLIFY(parList%next)

! shld(1)%thickness = 1.0
  shld(1,1)%thickness = 1.0/nSubRegions
  shld(1,2)%thickness = 1.0/nSubRegions
! shld(1)%xMin = xInit
  shld(1,1)%xMin = xInit
  shld(1,1)%xMax = shld(1,1)%xMin + shld(1,1)%thickness
! shld(1)%xMax = shld(1)%xMin + shld(1)%thickness
  shld(1,2)%xMin = shld(1,1)%xMax
  shld(1,2)%xMax = shld(1,2)%xMin + shld(1,2)%thickness

! shld(1)%sigmaTotal = 10.0
  shld(1,1)%sigmaTotal = 10.0
  shld(1,2)%sigmaTotal = 10.0
! shld(1)%sigmaAbs = 8.0 ! for ratio Es/Et = 0.2
! shld(1,1)%sigmaAbs = 8.0 ! for ratio Es/Et = 0.2
! shld(1,2)%sigmaAbs = 8.0 ! for ratio Es/Et = 0.2
! shld(1)%sigmaAbs = 2.0 ! for ratio Es/Et = 0.8
  shld(1,1)%sigmaAbs = 2.0 ! for ratio Es/Et = 0.2
  shld(1,2)%sigmaAbs = 2.0 ! for ratio Es/Et = 0.2
! shld(1)%absRatio = shld(1)%sigmaAbs/shld(1)%sigmaTotal
  shld(1,1)%absRatio = shld(1,1)%sigmaAbs/shld(1,1)%sigmaTotal
  shld(1,2)%absRatio = shld(1,2)%sigmaAbs/shld(1,2)%sigmaTotal

  shld(1,1)%importance = 1.0
  shld(1,2)%importance = 3.4

  scatterAngleInit(1) = DCOS(0.0)
  scatterAngleInit(2) = DCOS(pi/6.0)
  scatterAngleInit(3) = DCOS(pi/3.0)

  rrd = 5
  weightCutoff = 1.0E-7
  nParticles = 10000000
! nParticles = 5

  ! Loop over the three scattering angles
  DO iScatterAngle = 1, 3
! DO iScatterAngle = 1, 1

    nParLeak = 0
    nParAbs = 0
    nParRef = 0
    parLeak = 0.0
    parRef = 0.0
    parAbs = 0.0
    probLeakSq = 0.0; probRefSq = 0.0; probAbsSq = 0.0
    maxRelativeError = 100.0

    iParticle = 0

    CALL CPU_TIME(timeIn)

    DO WHILE ((iParticle .LT. nParticles) .AND.                     &
        (maxRelativeError .GT. 0.1))
!       (maxRelativeError .GT. 0.0))

      iParticle = iParticle + 1
      IF (MOD(iParticle,1000000) .EQ. 0)                             &
          PRINT*, 'particle number:', iParticle

      xLocation = xInit
      scatterAngle = scatterAngleInit(iScatterAngle)
      parStatus = 0
      currentRegion = 1
      currentSubRegion = 1
      oldRegion = 1
      oldSubRegion = 1
      collStatus = 0
      parWeight = 1.0
      parListTail => parList
      parCurrent => parListTail
!     IF (.NOT. ASSOCIATED(parListTail)) PRINT*, 'Error'
      CALL insertPar(parCurrent,parWeight)
!     IF (.NOT. ASSOCIATED(parListTail)) PRINT*, 'Error'
      parListTail => parCurrent%next
      parCurrent => parCurrent%next
!     IF (.NOT. ASSOCIATED(parListTail)) PRINT*, 'Error'

      DO WHILE (ASSOCIATED(parCurrent))
!       PRINT*, 'New split particle'
        DO WHILE (parStatus .EQ. 0)

!         PRINT*, 'For same split particle', xLocation,             &
!             scatterAngle

!         PRINT*, 'Entering free flight calculation'
          CALL freeFlightPL
!         PRINT*, '1: particle status', parStatus, collStatus
!         PRINT*, 'New x-location', xLocation

          IF ((parStatus .EQ. 0) .AND. (collStatus .EQ. 0) .AND.    &
              (oldSubRegion .NE. currentSubRegion)) THEN
!           PRINT*, 'Entering geometric splitting', parStatus,      &
!               collStatus, oldSubRegion, currentSubRegion,         &
!               oldRegion, currentRegion
            CALL geometricSplit
          END IF

          IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1) THEN
!           PRINT*, 'Entering getInteraction'
            CALL getInteraction 
          END IF
!         PRINT*, '2: particle status', parStatus, collStatus
!         PRINT*, xLocation, scatterAngle

          IF (parWeight .lt. weightCutoff) THEN
!           PRINT*, 'Entering Russian Roulette'
            CALL russianRoulette(rrd,flag)
            IF (flag .eq. 0) THEN
              parStatus = 4
            ELSE
              parWeight = parWeight*rrd
            END IF
          END IF
!         PRINT*, '3: particle status', parStatus, collStatus
!         PRINT*, xLocation, scatterAngle

          IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1) THEN
!           PRINT*, 'Entering getScatterAngle'
            CALL getScatterAngle
!           PRINT*, 'new angle', scatterAngle
          END IF
!         PRINT*, '4: particle status', parStatus, collStatus
!         PRINT*, xLocation, scatterAngle

          IF (parStatus .EQ. 1) THEN
            probLeakSq = probLeakSq + parWeight**2.0
          ELSE IF (parStatus .EQ. 2) THEN
            probRefSq = probRefSq + parWeight**2.0
          ELSE IF (parStatus .EQ. 3) THEN
            probAbsSq = probAbsSq + parWeight**2.0
          END IF

          parTemp => parCurrent%next
          numParLeft = 0
          DO WHILE (ASSOCIATED(parTemp))
            numParLeft = numParLeft + 1
            parTemp => parTemp%next
          END DO
!         PRINT*, 'Split particles left', numParLeft

        END DO
        parCurrent => parCurrent%next
      END DO

      IF (parStatus .EQ. 1) THEN
        relativeErrorLeak(iScatterAngle) = DSQRT(                   &
            probLeakSq/parLeak**2.0 - 1.0/iParticle)
      ELSE IF (parStatus .EQ. 2) THEN
        relativeErrorRef(iScatterAngle) = DSQRT(                    &
            probRefSq/parRef**2.0 - 1.0/iParticle)
      ELSE IF (parStatus .EQ. 3) THEN
        relativeErrorAbs(iScatterAngle) = DSQRT(                    &
            probAbsSq/parAbs**2.0 - 1.0/iParticle)
      END IF

      IF ((parLeak .GT. 0.00001) .AND. (parRef .GT. 0.00001) .AND.  &
          (parAbs .GT. 0.00001)) THEN
      maxRelativeError = MAX(relativeErrorLeak(iScatterAngle),      &
          relativeErrorRef(iScatterAngle),                          &
          relativeErrorAbs(iScatterAngle))
      END IF

      CALL delParList
!     print*, parLeak, parRef, parAbs

    END DO

    CALL CPU_TIME(timeOut)
    totalTime = timeOut - timeIn
    totalTime = totalTime/60.0 ! Conversion to minutes

    probLeak(iScatterAngle) = 1.0*parLeak/iParticle
    probRef(iScatterAngle) = 1.0*parRef/iParticle
    probAbs(iScatterAngle) = 1.0*parAbs/iParticle

    varianceLeak(iScatterAngle) = probLeakSq/iParticle              &
        - probLeak(iScatterAngle)**2.0
    varianceRef(iScatterAngle) = probRefSq/iParticle                &
        - probRef(iScatterAngle)**2.0
    varianceAbs(iScatterAngle) = probAbsSq/iParticle                &
        - probAbs(iScatterAngle)**2.0

    FOMLeak(iScatterAngle) = 1.0                                    &
        /relativeErrorLeak(iScatterAngle)**2./totalTime
    FOMRef(iScatterAngle) = 1.0                                     &
        /relativeErrorRef(iScatterAngle)**2./totalTime
    FOMAbs(iScatterAngle) = 1.0                                     &
        /relativeErrorAbs(iScatterAngle)**2./totalTime

  END DO

  unitProbFile = 101
  nameProbFile = 'problem_2.dat'

  OPEN (UNIT = unitProbFile, FILE = nameProbFile,                   &
      POSITION = 'append', FORM = 'formatted', ACTION = 'write')
  DO iScatterAngle = 1, 3
    WRITE(unitProbFile,501) iScatterAngle, probLeak(iScatterAngle), &
        probRef(iScatterAngle), probAbs(iScatterAngle),             &
        FOMLeak(iScatterAngle), FOMRef(iScatterAngle),              &
        FOMAbs(iScatterAngle), relativeErrorLeak(iScatterAngle),    &
        relativeErrorRef(iScatterAngle),                            &
        relativeErrorAbs(iScatterAngle),                            &
        varianceLeak(iScatterAngle), varianceRef(iScatterAngle),    &
        varianceAbs(iScatterAngle)
  END DO
  CLOSE(unitProbFile)

501 FORMAT (1(i1.1, 1X), 12(e12.5, 1X))

END PROGRAM multiRegionShield

SUBROUTINE freeFlightPL

  USE allData

  IMPLICIT NONE

  REAL :: randNum, pathLength

  CALL RANDOM_NUMBER(randNum)

  pathLength = -LOG(randNum)                                        &
      /shld(currentRegion,currentSubRegion)%sigmaTotal
  xLocation = xLocation + pathLength*scatterAngle
! print*, 'new x:', xLocation

  IF ((xLocation .GT. shld(currentRegion,currentSubRegion)%xMax))   &
      THEN
!   PRINT*, 'Importance increasing'
    xLocation = shld(currentRegion,currentSubRegion)%xMax
    oldSubRegion = currentSubRegion
    currentSubRegion = currentSubRegion + 1
    IF (currentSubRegion .GT. nSubRegions) THEN
      currentSubRegion = 1
      oldRegion = currentRegion
      currentRegion = currentRegion + 1
      IF (currentRegion .GT. nShields) THEN
        parStatus = 1
        nParLeak = nParLeak + 1
        parLeak = parLeak + parWeight
!       print*, 'particle leaked', parWeight
      END IF
    END IF
  ELSE IF (xLocation .LT.                                           &
      shld(currentRegion,currentSubRegion)%xMin) THEN
!   PRINT*, 'Importance decreasing'
    xLocation = shld(currentRegion,currentSubRegion)%xMin
    oldSubRegion = currentSubRegion
    currentSubRegion = currentSubRegion - 1
    IF (currentSubRegion .LT. 1) THEN
      currentSubRegion = nSubRegions
      oldRegion = currentRegion
      currentRegion = currentRegion - 1
      IF (currentRegion .LT. 1) THEN
        parStatus = 2
        nParRef = nParRef + 1
        parRef = parRef + parWeight
!       print*, 'particle reflected', parWeight
      END IF
    END IF
  ELSE
    collStatus = 1
!   print*, 'collision'
  END IF

END SUBROUTINE freeFlightPL

SUBROUTINE getInteraction

  USE allData

  IMPLICIT NONE

  REAL :: randNum

  CALL RANDOM_NUMBER(randNum)

  IF (randNum .LT. shld(currentRegion,currentSubRegion)%absRatio)   &
      THEN
    parStatus = 3
    nParAbs = nParAbs + 1
    parAbs = parAbs + parWeight
!   print*, 'particle absorbed', parWeight
  END IF

END SUBROUTINE getInteraction

SUBROUTINE getScatterAngle

  USE allData

  IMPLICIT NONE

  REAL :: randNum, pi, mu_0, phi_0, mu_prime

  pi = DACOS(-1.0)

  CALL RANDOM_NUMBER(randNum)
  mu_0 = 2.0*randNum - 1.0

  CALL RANDOM_NUMBER(randNum)
  phi_0 = 2.0*pi*randNum

  mu_prime = scatterAngle*mu_0                                      &
      + DSQRT(1.0-scatterAngle**2.0)                                &
       *DSQRT(1.0-mu_0**2.0)                                        &
       *DCOS(phi_0)

  scatterAngle = mu_prime

  collStatus = 0

END SUBROUTINE getScatterAngle

SUBROUTINE russianRoulette(rrd,flag)

  USE allData

  IMPLICIT NONE

  INTEGER, INTENT(IN) :: rrd
  INTEGER, INTENT(OUT) :: flag
  REAL :: randNum

  CALL RANDOM_NUMBER(randNum)
  IF (randNum .GT. 1.0/rrd) THEN
    flag = 0
  ELSE
    flag = 1
  END IF

END SUBROUTINE russianRoulette

SUBROUTINE geometricSplit

  USE allData

  IMPLICIT NONE

  INTERFACE

    SUBROUTINE insertPar(parCurrent,wt)

    USE allData
    TYPE (particle), POINTER, INTENT(IN) :: parCurrent
    REAL, INTENT (IN) :: wt

    END SUBROUTINE insertPar

  END INTERFACE

  INTEGER :: iSplitPar, nSplitPar
  REAL :: impRatio, randNum, delta

! PRINT*, 'Entering geometricSplit', oldRegion, currentRegion,      &
!     oldSubRegion, currentSubRegion

  impRatio = shld(currentRegion,currentSubRegion)%importance        &
      /shld(oldRegion,oldSubRegion)%importance

! PRINT*, 'Set importance ratios', impRatio

  IF (impRatio .GE. 2.0) THEN
    delta = impRatio - DINT(impRatio)
    IF (delta .EQ. 0.0) THEN
      ! impRatio is an integer
      nSplitPar = DINT(impRatio)
    ELSE
      CALL RANDOM_NUMBER(randNum)
      IF (randNum .LE. 1.0-delta) THEN
        nSplitPar = DINT(impRatio)
      ELSE
        nSplitPar = DINT(impRatio) + 1
      END IF
    END IF
!   PRINT*, 'Splitting into:', nSplitPar
    DO iSplitPar = 2, nSplitPar
!     PRINT*, 'Inserting particle number', iSplitPar
      CALL insertPar(parListTail,parWeight/nSplitPar)
!     PRINT*, 'Inserted particle number', iSplitPar
      parListTail => parListTail%next
      parWeight = parWeight/nSplitPar
    END DO
  ELSE IF (impRatio .LE. 0.5) THEN
    CALL RANDOM_NUMBER(randNum)
    IF (randNum .GT. 1.0 - impRatio) THEN
      parWeight = parWeight/impRatio
!     PRINT*, 'Particle weight increased'
    ELSE
      parStatus = 4
!     PRINT*, 'Particle killed'
    END IF
  ELSE
    WRITE(*,*) 'Wrong importance ratios'
  END IF

! PRINT*, 'Exiting geometricSplit'

END SUBROUTINE geometricSplit

SUBROUTINE insertPar(parCurrent,wt)

  USE allData

  IMPLICIT NONE

  TYPE (particle), POINTER, INTENT(IN) :: parCurrent
  REAL, INTENT (IN) :: wt

  ALLOCATE(parCurrent%next)
  parCurrent%region = currentRegion
  parCurrent%subRegion = currentSubRegion
  parCurrent%weight = wt
  parCurrent%xLoc = xLocation
  parCurrent%scatAngle = scatterAngle
  NULLIFY(parCurrent%next%next)

END SUBROUTINE insertPar

SUBROUTINE delParList

  USE allData

  IMPLICIT NONE

  TYPE (particle), POINTER :: parTemp, parCurrent

  parCurrent => parList%next
  DO WHILE (ASSOCIATED(parCurrent))
    parTemp => parCurrent%next
    DEALLOCATE(parCurrent)
    parCurrent => parTemp
  END DO

END SUBROUTINE delParList
