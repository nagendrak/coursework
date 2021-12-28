MODULE allData

  TYPE shield
    REAL :: sigmaTotal, sigmaAbs, xMin, xMax, absRatio, thickness
  END TYPE shield

  TYPE (shield), ALLOCATABLE :: shld(:)

  INTEGER :: nParticles, nShields, parStatus, nParAbs, nParRef,     &
      nParLeak, currentRegion, collStatus, freeFlightFlag,          &
      nTallyingRegions
  REAL :: xInit, scatterAngleInit(3), xLocation, scatterAngle
  REAL :: probAbs(3), probRef(3), probLeak(3),                      &
      relativeErrorLeak(3), relativeErrorRef(3),                    &
      relativeErrorAbs(3), FOMLeak(3), FOMRef(3), FOMAbs(3),        &
      varianceLeak(3), varianceRef(3), varianceAbs(3),              &
      totalThickness
  REAL, ALLOCATABLE :: p(:), fc(:), scalarFluxColl(:),              &
      scalarFluxPL(:), regionWidth(:), regionxMin(:),               &
      regionxMax(:), scalarFluxCollSq(:), scalarFluxPLSq(:),        &
      scalarFluxCollSum(:), &
      scalarFluxPLSum(:), relErrScalFlxColl(:), relErrScalFlxPL(:)

END MODULE allData


PROGRAM multiRegionShield

  USE allData

  IMPLICIT NONE

  INTEGER :: iParticle, iShield, unitProbFile, iScatterAngle, i,    &
      n
  REAL :: pi, maxRelativeError, timeIn, timeOut, totalTime
  CHARACTER(80) :: nameProbFile

  ! Initialization
  xInit = 0.0
  pi = DACOS(-1.0)

  CALL RANDOM_SEED()

  nShields = 1
  ALLOCATE(shld(nShields))

  nTallyingRegions = 10
  ALLOCATE(p(nTallyingRegions), fc(nTallyingRegions),               &
      scalarFluxColl(nTallyingRegions),                             &
      scalarFluxPL(nTallyingRegions),regionxMin(nTallyingRegions),  &
      regionxMax(nTallyingRegions), regionWidth(nTallyingRegions),  &
      scalarFluxCollSq(nTallyingRegions),                           &
      scalarFluxPLSq(nTallyingRegions),                             &
      scalarFluxCollSum(nTallyingRegions),                          &
      scalarFluxPLSum(nTallyingRegions),                            &
      relErrScalFlxColl(nTallyingRegions),                          &
      relErrScalFlxPL(nTallyingRegions))

  shld(1)%thickness = 1.0
  shld(1)%xMin = xInit
  shld(1)%xMax = shld(1)%xMin + shld(1)%thickness
  DO i = 1, nTallyingRegions
    regionxMin(i) = shld(1)%xMin + (i-1)*1.0/nTallyingRegions
    regionxMax(i) = shld(1)%xMin + i*1.0/nTallyingRegions
    regionWidth(i) = regionxMax(i) - regionxMin(i)
  END DO
  totalThickness = 0.0
  DO n = 1, nShields
    totalThickness = totalThickness + shld(n)%thickness
  END DO

  shld(1)%sigmaTotal = 10.0
! shld(1)%sigmaAbs = 8.0 ! for ratio Es/Et = 0.2
  shld(1)%sigmaAbs = 2.0 ! for ratio Es/Et = 0.8
  shld(1)%absRatio = shld(1)%sigmaAbs/shld(1)%sigmaTotal

  scatterAngleInit(1) = DCOS(0.0)
  scatterAngleInit(2) = DCOS(pi/6.0)
  scatterAngleInit(3) = DCOS(pi/3.0)
  PRINT*, scatterAngleInit(:)

  nParticles = 10000000

  ! Loop over the three scattering angles
  DO iScatterAngle = 1, 1

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
      collStatus = 0

      DO WHILE (parStatus .EQ. 0)

        CALL freeFlightPL

        IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1)               &
            CALL getInteraction 

        IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1)               &
            CALL getScatterAngle

      END DO

      DO i = 1, nTallyingRegions
        scalarFluxColl(i) = fc(i)/iParticle/regionWidth(i)
        scalarFluxCollSq(i) = scalarFluxCollSq(i)                   &
            + scalarFluxColl(i)**2.0
        scalarFluxCollSum(i) = scalarFluxCollSum(i)                 &
            + scalarFluxColl(i)
        scalarFluxPL(i) = p(i)/iParticle/regionWidth(i)
        scalarFluxPLSq(i) = scalarFluxPLSq(i) + scalarFluxPL(i)**2.0
        scalarFluxPLSum(i) = scalarFluxPLSum(i) + scalarFluxPL(i)

        IF (MOD(iParticle,1000) .EQ. 0) THEN
        relErrScalFlxColl(i) = DSQRT((scalarFluxCollSq(i)           &
            /(scalarFluxCollSum(i)**2.0)) - 1.0/iParticle)
        relErrScalFlxPL(i) = DSQRT((scalarFluxPLSq(i)               &
            /(scalarFluxPLSum(i)**2.0)) - 1.0/iParticle)
        ! Following for collision estimator relative error
        IF (MINVAL(scalarFluxColl) .NE. 0.0) THEN
        maxRelativeError = MAXVAL(relErrScalFlxColl)
        END IF
        ! Following for path-length estimator relative error
!       IF (MINVAL(scalarFluxPL) .NE. 0.0) THEN
!       maxRelativeError = MAXVAL(relErrScalFlxPL)
!       END IF
        END IF

      END DO

    END DO

    CALL CPU_TIME(timeOut)
    totalTime = timeOut - timeIn
    totalTime = totalTime/60.0 ! Conversion to minutes

  END DO

  unitProbFile = 102
  nameProbFile = 'problem_1b.dat'

  OPEN (UNIT = unitProbFile, FILE = nameProbFile,                   &
      POSITION = 'append', FORM = 'formatted', ACTION = 'write')
  DO i = 1, nTallyingRegions
    WRITE(unitProbFile,502) i, scalarFluxColl(i),                   &
        relErrScalFlxColl(i),                                       &
        1.0/relErrScalFlxColl(i)**2./totalTime,                     &
        scalarFluxPL(i), relErrScalFlxPL(i),                        &
        1.0/relErrScalFlxPL(i)**2./totalTime
  END DO
  CLOSE(unitProbFile)

501 FORMAT (1(i1.1, 1X), 12(e12.5, 1X))
502 FORMAT (1(i2.2, 1X), 6(e12.5, 1X))

END PROGRAM multiRegionShield

SUBROUTINE freeFlightPL

  USE allData

  IMPLICIT NONE

  INTEGER :: i, iOld, iNew
  REAL :: randNum, pathLength, regionPathLength, oldxLocation

  CALL RANDOM_NUMBER(randNum)

  pathLength = -LOG(randNum)/shld(currentRegion)%sigmaTotal
  oldxLocation = xLocation
  xLocation = xLocation + pathLength*scatterAngle

  IF ((xLocation .GT. shld(currentRegion)%xMax)) THEN
    xLocation = shld(currentRegion)%xMax
    currentRegion = currentRegion + 1
    IF (currentRegion .GT. nShields) THEN
      parStatus = 1
      nParLeak = nParLeak + 1
    END IF
  ELSE IF (xLocation .LT. shld(currentRegion)%xMin) THEN
    xLocation = shld(currentRegion)%xMin
    currentRegion = currentRegion - 1
    IF (currentRegion .LT. 1) THEN
      parStatus = 2
      nParRef = nParRef + 1
    END IF
  ELSE
    collStatus = 1
! Collision estimator
    i = INT((xLocation-shld(1)%xMin)                    &
        *nTallyingRegions/totalThickness) + 1
    fc(i) = fc(i) + 1.0/shld(1)%sigmaTotal
  END IF

  iOld = INT((oldxLocation-shld(1)%xMin)                &
        *nTallyingRegions/totalThickness) + 1
  iNew = INT((xLocation-shld(1)%xMin)                   &
        *nTallyingRegions/totalThickness) + 1
  IF (xLocation .EQ. 1.0) THEN
!   print*, iNew, xLocation, iOld
    iNew = nTallyingRegions
  END IF

  DO i = iOld, iNew
    IF (xLocation .GT. regionxMax(i)) THEN
      regionPathLength = (regionxMax(i)-oldxLocation)/scatterAngle
      p(i) = p(i) + 1.0*regionPathLength
      oldxLocation = regionxMax(i)
    ELSE
      regionPathLength = (xLocation-oldxLocation)/scatterAngle
      p(i) = p(i) + 1.0*regionPathLength
    END IF

  END DO
! Path-length estimator

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
