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
      nSubRegions, currentSubRegion, oldRegion, oldSubRegion,       &
      nTallyingRegions
  REAL :: xInit, scatterAngleInit(3), xLocation, scatterAngle,      &
      parRef, parLeak, parAbs, parWeight, weightCutoff, parKilled,  &
      parAdded, impRatio
  REAL :: probAbs(3), probRef(3), probLeak(3),                      &
      relativeErrorLeak(3), relativeErrorRef(3),                    &
      relativeErrorAbs(3), FOMLeak(3), FOMRef(3), FOMAbs(3),        &
      varianceLeak(3), varianceRef(3), varianceAbs(3),              &
      totalThickness
  REAL, ALLOCATABLE :: p(:), fc(:), scalarFluxColl(:),              &
      scalarFluxPL(:), regionWidth(:), regionxMin(:),               &
      regionxMax(:), scalarFluxCollSq(:), scalarFluxPLSq(:),        &
      scalarFluxCollSum(:),                                         &
      scalarFluxPLSum(:), relErrScalFlxColl(:), relErrScalFlxPL(:)

END MODULE allData


PROGRAM multiRegionShield

  USE allData

  IMPLICIT NONE

  INTEGER :: iParticle, iShield, unitProbFile, iScatterAngle,       &
      rrd, flag, numParLeft, iSubRegion, i, n
  REAL :: pi, probLeakSq, probRefSq, probAbsSq, timeIn, timeOut,    &
      totalTime, maxRelativeError, iParLeak, iParRef, iParAbs
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
  relativeErrorLeak = 100.0
  relativeErrorAbs = 100.0
  relativeErrorRef = 100.0

  CALL RANDOM_SEED()

  nShields = 1
  nSubRegions = 5
  impRatio = 3.4
  ALLOCATE(shld(nShields,nSubRegions))

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

  ALLOCATE(parList)
  NULLIFY(parList%next)

  DO iSubRegion = 1, nSubRegions
    shld(1,iSubRegion)%thickness = 1.0/nSubRegions
    shld(1,iSubRegion)%xMin = xInit + (iSubRegion-1)*1.0/nSubRegions
    shld(1,iSubRegion)%xMax = xInit + iSubRegion*1.0/nSubRegions

    shld(1,iSubRegion)%sigmaTotal = 10.0
    shld(1,iSubRegion)%sigmaAbs = 8.0 ! for ratio Es/Et = 0.2
!   shld(1,iSubRegion)%sigmaAbs = 2.0 ! for ratio Es/Et = 0.8
    shld(1,iSubRegion)%absRatio =                                   &
        shld(1,iSubRegion)%sigmaAbs/shld(1,iSubRegion)%sigmaTotal

    shld(1,iSubRegion)%importance = impRatio**(iSubRegion-1.0)

  END DO
  DO i = 1, nTallyingRegions
    regionxMin(i) = shld(1,1)%xMin + (i-1)*1.0/nTallyingRegions
    regionxMax(i) = shld(1,1)%xMin + i*1.0/nTallyingRegions
    regionWidth(i) = regionxMax(i) - regionxMin(i)
  END DO
  totalThickness = 0.0
  DO n = 1, nShields
    DO iSubRegion = 1, nSubRegions
      totalThickness = totalThickness + shld(n,iSubRegion)%thickness
    END DO
  END DO

  scatterAngleInit(1) = DCOS(0.0)
  scatterAngleInit(2) = DCOS(pi/6.0)
  scatterAngleInit(3) = DCOS(pi/3.0)

  rrd = 5
  weightCutoff = 1.0E-7
  nParticles = 10000000

  ! Loop over the three scattering angles
  DO iScatterAngle = 1, 1

    nParLeak = 0
    nParAbs = 0
    nParRef = 0
    parLeak = 0.0
    parRef = 0.0
    parAbs = 0.0
    parKilled = 0.0
    parAdded = 0.0
    probLeakSq = 0.0; probRefSq = 0.0; probAbsSq = 0.0
    maxRelativeError = 100.0

    iParticle = 0

    CALL CPU_TIME(timeIn)

    DO WHILE ((iParticle .LT. nParticles) .AND.                     &
        (maxRelativeError .GT. 0.1))
!       (maxRelativeError .GT. 0.0))

      iParticle = iParticle + 1
      IF (MOD(iParticle,100000) .EQ. 0) THEN
        PRINT*, 'particle number:', iParticle
      END IF

      xLocation = xInit
      scatterAngle = scatterAngleInit(iScatterAngle)
      currentRegion = 1
      currentSubRegion = 1
      oldRegion = 1
      oldSubRegion = 1
      parWeight = 1.0
      iParLeak = 0.0; iParRef = 0.0; iParAbs = 0.0
      parListTail => parList
      parCurrent => parListTail
      CALL insertPar(parCurrent,parWeight)
      parListTail => parCurrent%next
      parCurrent => parCurrent%next

      DO WHILE (ASSOCIATED(parCurrent))
        parStatus = 0
        currentRegion = parCurrent%region
        currentSubRegion = parCurrent%SubRegion
        oldRegion = currentRegion
        oldSubRegion = currentSubRegion
        collStatus = 0
        parWeight = parCurrent%weight
        xLocation = parCurrent%xLoc
        scatterAngle = parCurrent%scatAngle
        DO WHILE (parStatus .EQ. 0)

          CALL freeFlightPL

          IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1) THEN
            CALL getInteraction 
          END IF

          IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1) THEN
            CALL getScatterAngle
          END IF

        END DO
        parCurrent => parCurrent%next
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

      CALL delParList

    END DO

    CALL CPU_TIME(timeOut)
    totalTime = timeOut - timeIn
    totalTime = totalTime/60.0 ! Conversion to minutes

  END DO

  unitProbFile = 102
  nameProbFile = 'problem_2b.dat'

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

  pathLength = -LOG(randNum)                                        &
      /shld(currentRegion,currentSubRegion)%sigmaTotal
  oldxLocation = xLocation
  xLocation = xLocation + pathLength*scatterAngle

  IF ((xLocation .GT. shld(currentRegion,currentSubRegion)%xMax))   &
      THEN
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
      END IF
    END IF
    IF (parStatus .EQ. 0) CALL geometricSplit
  ELSE IF (xLocation .LT.                                           &
      shld(currentRegion,currentSubRegion)%xMin) THEN
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
      END IF
    END IF
    IF (parStatus .EQ. 0) CALL geometricSplit
  ELSE
    collStatus = 1
! Collision estimator
    i = INT((xLocation-shld(1,1)%xMin)                    &
        *nTallyingRegions/totalThickness) + 1
    fc(i) = fc(i) + parWeight/shld(1,1)%sigmaTotal

!   print*, 'collision'
  END IF

  iOld = INT((oldxLocation-shld(1,1)%xMin)                          &
        *nTallyingRegions/totalThickness) + 1
  iNew = INT((xLocation-shld(1,1)%xMin)                             &
        *nTallyingRegions/totalThickness) + 1
  IF (xLocation .EQ. shld(1,nSubRegions)%xMax) THEN
    print*, iNew, xLocation, iOld
    iNew = nTallyingRegions
  END IF

  DO i = iOld, iNew
    IF (xLocation .GT. regionxMax(i)) THEN
      regionPathLength = (regionxMax(i)-oldxLocation)/scatterAngle
      p(i) = p(i) + parWeight*regionPathLength
      oldxLocation = regionxMax(i)
    ELSE
      regionPathLength = (xLocation-oldxLocation)/scatterAngle
      p(i) = p(i) + parWeight*regionPathLength
    END IF

  END DO
! Path-length estimator

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
  REAL :: randNum, delta, oldImp, newImp

  oldImp = shld(oldRegion,oldSubRegion)%importance
  newImp = shld(currentRegion,currentSubRegion)%importance

  IF (newImp .GT. oldImp) THEN
    delta = impRatio - DINT(impRatio)
    IF (delta .EQ. 0.0) THEN
      nSplitPar = DINT(impRatio)
    ELSE
      CALL RANDOM_NUMBER(randNum)
      IF (randNum .LE. 1.0-delta) THEN
        nSplitPar = DINT(impRatio)
      ELSE
        nSplitPar = DINT(impRatio) + 1
      END IF
    END IF
    DO iSplitPar = 2, nSplitPar
      CALL insertPar(parListTail,parWeight/nSplitPar)
      parListTail => parListTail%next
    END DO
    parWeight = parWeight/nSplitPar
  ELSE
    CALL RANDOM_NUMBER(randNum)
    IF (randNum .LE. (1.0/impRatio)) THEN
      parAdded = parAdded + (parWeight*impRatio)-parWeight
      parWeight = parWeight*impRatio
    ELSE
      parStatus = 4
      parKilled = parKilled + parWeight
    END IF
  END IF

END SUBROUTINE geometricSplit

SUBROUTINE insertPar(parCurrent,wt)

  USE allData

  IMPLICIT NONE

  TYPE (particle), POINTER, INTENT(IN) :: parCurrent
  REAL, INTENT (IN) :: wt

  ALLOCATE(parCurrent%next)
  parCurrent%next%region = currentRegion
  parCurrent%next%subRegion = currentSubRegion
  parCurrent%next%weight = wt
  parCurrent%next%xLoc = xLocation
  parCurrent%next%scatAngle = scatterAngle
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
