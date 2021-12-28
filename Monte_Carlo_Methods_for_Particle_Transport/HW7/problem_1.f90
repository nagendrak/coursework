MODULE allData

  TYPE shield
    REAL :: sigmaTotal, sigmaAbs, xMin, xMax, absRatio, thickness,  &
        importance
  END TYPE shield

  TYPE (shield), ALLOCATABLE :: shld(:,:)

  INTEGER :: nParticles, nShields, parStatus, nParAbs, nParRef,     &
      nParLeak, currentRegion, collStatus, freeFlightFlag
  REAL :: xInit, scatterAngleInit(3), xLocation, scatterAngle
  REAL :: probAbs(3), probRef(3), probLeak(3),                      &
      relativeErrorLeak(3), relativeErrorRef(3),                    &
      relativeErrorAbs(3), FOMLeak(3), FOMRef(3), FOMAbs(3),        &
      varianceLeak(3), varianceRef(3), varianceAbs(3)

END MODULE allData

PROGRAM multiRegionShield

  USE allData

  IMPLICIT NONE

  INTEGER :: iParticle, iShield, unitProbFile, iScatterAngle
  REAL :: pi, maxRelativeError, timeIn, timeOut, totalTime
  CHARACTER(80) :: nameProbFile

  ! Initialization
  xInit = 0.0
  pi = DACOS(-1.0)

  CALL RANDOM_SEED()

  nShields = 1
  nSubShields = 2
  ALLOCATE(shld(nShields,nSubShields))

  shld(1,1)%thickness = 1.0/nSubShields
  shld(1,2)%thickness = 1.0/nSubShields
  shld(1,1)%xMin = xInit
  shld(1,2)%xMin = shld(1,1)%xMax
  shld(1,1)%xMax = shld(1,1)%xMin + shld(1,1)%thickness
  shld(1,2)%xMax = shld(1,2)%xMin + shld(1,2)%thickness

  shld(1,1)%sigmaTotal = 10.0
  shld(1,1)%sigmaAbs = 8.0 ! for ratio Es/Et = 0.2
! shld(1,1)%sigmaAbs = 2.0 ! for ratio Es/Et = 0.8
  shld(1,1)%absRatio = shld(1)%sigmaAbs/shld(1)%sigmaTotal

  scatterAngleInit(1) = DCOS(0.0)
  scatterAngleInit(2) = DCOS(pi/6.0)
  scatterAngleInit(3) = DCOS(pi/3.0)
  PRINT*, scatterAngleInit(:)

  nParticles = 10000000

  ! Loop over the three scattering angles
  DO iScatterAngle = 1, 3

    nParLeak = 0
    nParAbs = 0
    nParRef = 0
    maxRelativeError = 100.0

    iParticle = 0

    CALL CPU_TIME(timeIn)

    DO WHILE ((iParticle .LT. nParticles) .AND.                     &
        (maxRelativeError .GT. 0.1))
!       (maxRelativeError .GT. 0.0))

      iParticle = iParticle + 1
      IF (MOD(iParticle,1000000) .EQ. 0)                             &
          PRINT*, 'particle number:', iParticle

      ! Initial x and mu values
      xLocation = xInit
      scatterAngle = scatterAngleInit(iScatterAngle)
      parStatus = 0
      currentRegion = 1
      currentSubRegion = 1
      collStatus = 0

      parCurrent => splitParList

      DO WHILE (ASSOCIATED(parCurrent))
      DO WHILE (parStatus .EQ. 0)

        CALL freeFlightPL

        IF ((parStatus .EQ. 0) .AND. (collStatus .EQ. 0))           &
            CALL geometricSplitting

        IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1)               &
            CALL getInteraction 

        IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1)               &
            CALL getScatterAngle

      END DO
      parCurrent => parCurrent%next
      END DO

      IF ((nParLeak .GT. 0) .AND. (nParRef .GT. 0) .AND.            &
          (nParAbs .GT. 0)) THEN

      probLeak(iScatterAngle) = 1.0*nParLeak/iParticle
      probRef(iScatterAngle) = 1.0*nParRef/iParticle
      probAbs(iScatterAngle) = 1.0*nParAbs/iParticle

      relativeErrorLeak(iScatterAngle) =                            &
          DSQRT(1.0/nParLeak - 1.0/iParticle)
      relativeErrorRef(iScatterAngle) =                             &
          DSQRT(1.0/nParRef - 1.0/iParticle)
      relativeErrorAbs(iScatterAngle) =                             &
          DSQRT(1.0/nParAbs - 1.0/iParticle)

      maxRelativeError = MAX(relativeErrorLeak(iScatterAngle),      &
          relativeErrorRef(iScatterAngle),                          &
          relativeErrorAbs(iScatterAngle))

      END IF

    END DO

    varianceLeak(iScatterAngle) = probLeak(iScatterAngle)*        &
        (1.0-probLeak(iScatterAngle))
    varianceRef(iScatterAngle) = probRef(iScatterAngle)*          &
        (1.0-probRef(iScatterAngle))
    varianceAbs(iScatterAngle) = probAbs(iScatterAngle)*          &
        (1.0-probAbs(iScatterAngle))

    CALL CPU_TIME(timeOut)
    totalTime = timeOut - timeIn
    totalTime = totalTime/60.0 ! Conversion to minutes

    FOMLeak(iScatterAngle) = 1.0                                    &
        /relativeErrorLeak(iScatterAngle)**2./totalTime
    FOMRef(iScatterAngle) = 1.0                                     &
        /relativeErrorRef(iScatterAngle)**2./totalTime
    FOMAbs(iScatterAngle) = 1.0                                     &
        /relativeErrorAbs(iScatterAngle)**2./totalTime

  END DO

  unitProbFile = 101
  nameProbFile = 'problem_1.dat'

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

  IF ((xLocation .GT. shld(currentRegion,currentSubRegion)%xMax))   &
      THEN
    xLocation = shld(currentRegion,currentSubRegion)%xMax
    currentSubRegion = currentSubRegion + 1
    IF (currentSubRegion .GT. nSubRegions) THEN
      currentRegion = currentRegion + 1
      IF (currentRegion .GT. nShields) THEN
        parStatus = 1
        nParLeak = nParLeak + 1
      END IF
    END IF
  ELSE IF (xLocation .LT.                                           &
      shld(currentRegion,currentSubRegion)%xMin) THEN
    xLocation = shld(currentRegion,currentSubRegion)%xMin
    currentSubRegion = currentSubRegion - 1
    IF (currentSubRegion .LT. 1) THEN
      currentRegion = currentRegion - 1
      IF (currentRegion .LT. 1) THEN
        parStatus = 2
        nParRef = nParRef + 1
      END IF
    END IF
  ELSE
    collStatus = 1
  END IF

END SUBROUTINE freeFlightPL

SUBROUTINE getInteraction

  USE allData

  IMPLICIT NONE

  REAL :: randNum

  CALL RANDOM_NUMBER(randNum)

  IF (randNum .LT. shld(currentRegion)%absRatio) THEN
    parStatus = 3
    nParAbs = nParAbs + 1
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


SUBROUTINE geometricSplitting

  IMPLICIT NONE

  impRatio = shld(currentRegion,currentSubRegion)%importance        &
      /shld(currentRegion,currentSubRegion)%importance

  IF (impRatio .GE. 2.0) THEN
    delta = impRatio - DINT(impRatio)
    IF (delta .EQ. 0.0) THEN
      ! impRatio is an integer
      nSplitPar = DINT(impRatio)
    ELSE
      CALL RANDOM_NUMBER(randNum)
      IF (randNum .LE. 1.0-delta)
        nSplitPar = DINT(impRatio)
      ELSE
        nSplitPar = DINT(impRatio) + 1
      END IF
    END IF
    DO iSplitPar = 1, nSplitPar
      ALLOCATE(splitParCurrent%next)
      splitParCurrent%region = currentRegion
      splitParCurrent%subRegion = currentSubRegion
      splitParCurrent%weight = parWeight/nSplitPar
      splitParCurrent%xLoc = xLocation
      splitParCurrent%scatAngle = scatterAngle
      NULLIFY(splitParCurrent%next)
    END DO
  ELSE IF (impRatio .LE. 0.5) THEN
    CALL RANDOM_NUMBER(randNum)
    IF (randNum .GT. 1.0 - impRatio) THEN
      parWeight = parWeight/impRatio
    ELSE
      parStatus = 0
    END IF
  ELSE
    WRITE(*,*) 'Wrong importance ratios'
  END IF

END SUBROUTINE geometricSplitting
