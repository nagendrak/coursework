MODULE allData

  TYPE shield
    REAL :: sigmaTotal, sigmaAbs, xMin, xMax, absRatio, thickness
  END TYPE shield

  TYPE (shield), ALLOCATABLE :: shld(:)

  INTEGER :: nParticles, nShields, parStatus, nParAbs, nParRef,     &
      nParLeak, currentRegion, collStatus, freeFlightFlag
  REAL :: xInit, scatterAngleInit(3), xLocation, scatterAngle,      &
      parRef, parLeak, parAbs, weight, weightCutoff
  REAL :: probAbs(3), probRef(3), probLeak(3),                      &
      relativeErrorLeak(3), relativeErrorRef(3),                    &
      relativeErrorAbs(3), FOMLeak(3), FOMRef(3), FOMAbs(3),        &
      varianceLeak(3), varianceRef(3), varianceAbs(3)

END MODULE allData


PROGRAM implicitCapture

  USE allData

  IMPLICIT NONE

  INTEGER :: iParticle, iShield, unitProbFile, iScatterAngle,       &
      rrd, flag
  REAL :: pi, probLeakSq, probRefSq, probAbsSq, timeIn, timeOut,    &
      totalTime, maxRelativeError
  CHARACTER(80) :: nameProbFile

  ! Initialization
  xInit = 0.0
  pi = DACOS(-1.0)

  CALL RANDOM_SEED()

  nShields = 1
  ALLOCATE(shld(nShields))

  shld(1)%thickness = 1.0
  shld(1)%xMin = xInit
  shld(1)%xMax = shld(1)%xMin + shld(1)%thickness

  shld(1)%sigmaTotal = 10.0
  shld(1)%sigmaAbs = 8.0 ! for ratio Es/Et = 0.2
! shld(1)%sigmaAbs = 2.0 ! for ratio Es/Et = 0.8
  shld(1)%absRatio = shld(1)%sigmaAbs/shld(1)%sigmaTotal

  scatterAngleInit(1) = DCOS(0.0)
  scatterAngleInit(2) = DCOS(pi/6.0)
  scatterAngleInit(3) = DCOS(pi/3.0)

  rrd = 5
  weightCutoff = 1.0E-7
  nParticles = 10000000

  ! Loop over the three scattering angles
  DO iScatterAngle = 1, 3

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
      collStatus = 0
      weight = 1.0

      DO WHILE (parStatus .EQ. 0)

        CALL freeFlightPL

        IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1)               &
            CALL getInteraction 

        IF (weight .lt. weightCutoff) THEN
          CALL russianRoulette(rrd,flag)
          IF (flag .eq. 0) THEN
            parStatus = 3
          ELSE
            weight = weight*rrd
          END IF
        END IF

        IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1)               &
            CALL getScatterAngle

      END DO

      IF (parStatus .EQ. 1) THEN
        probLeakSq = probLeakSq + weight**2.0
        relativeErrorLeak(iScatterAngle) = DSQRT(                   &
            probLeakSq/parLeak**2.0 - 1.0/iParticle)
      ELSE IF (parStatus .EQ. 2) THEN
        probRefSq = probRefSq + weight**2.0
        relativeErrorRef(iScatterAngle) = DSQRT(                    &
            probRefSq/parRef**2.0 - 1.0/iParticle)
      END IF
      probAbsSq = probAbsSq + (1.0-weight)**2.0
      parAbs = parAbs + (1.0-weight)
      relativeErrorAbs(iScatterAngle) = DSQRT(                      &
          probAbsSq/parAbs**2.0 - 1.0/iParticle)

      IF ((parLeak .GT. 0.00001) .AND. (parRef .GT. 0.00001) .AND.  &
          (parAbs .GT. 0.00001)) THEN
      maxRelativeError = MAX(relativeErrorLeak(iScatterAngle),      &
          relativeErrorRef(iScatterAngle),                          &
          relativeErrorAbs(iScatterAngle))
      END IF

    END DO

    CALL CPU_TIME(timeOut)
    totalTime = timeOut - timeIn
    totalTime = totalTime/60.0 ! Conversion to minutes

    probLeak(iScatterAngle) = 1.0*parLeak/iParticle
    probRef(iScatterAngle) = 1.0*parRef/iParticle
    probAbs(iScatterAngle) = 1.0-probLeak(iScatterAngle)            &
        -probRef(iScatterAngle)

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

END PROGRAM implicitCapture

SUBROUTINE freeFlightPL

  USE allData

  IMPLICIT NONE

  REAL :: randNum, pathLength

  CALL RANDOM_NUMBER(randNum)

  pathLength = -LOG(randNum)/shld(currentRegion)%sigmaTotal
  xLocation = xLocation + pathLength*scatterAngle

  IF ((xLocation .GT. shld(currentRegion)%xMax)) THEN
    xLocation = shld(currentRegion)%xMax
    currentRegion = currentRegion + 1
    IF (currentRegion .GT. nShields) THEN
      parStatus = 1
      nParLeak = nParLeak + 1
      parLeak = parLeak + weight
    END IF
  ELSE IF (xLocation .LT. shld(currentRegion)%xMin) THEN
    xLocation = shld(currentRegion)%xMin
    currentRegion = currentRegion - 1
    IF (currentRegion .LT. 1) THEN
      parStatus = 2
      nParRef = nParRef + 1
      parRef = parRef + weight
    END IF
  ELSE
    collStatus = 1
  END IF

END SUBROUTINE freeFlightPL


SUBROUTINE getInteraction

  USE allData

  IMPLICIT NONE

  REAL :: randNum

  weight = weight                                                 &
      *(1.0-(shld(currentRegion)%sigmaAbs                         &
      /shld(currentRegion)%sigmaTotal))

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
