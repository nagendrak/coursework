!--------------------------------------------------------------------
! Program written by Nagendra Krishnamurthy
! Start date: 2011-09-03
! End date  : 2011-09-11
! 
! Program should be compiled with ifort (Intel fortran compiler) or
! any other compiler with which the RANDOM_NUMBER and RANDOM_SEED
! functions work properly.
!--------------------------------------------------------------------

MODULE allData

  TYPE shield
    REAL :: sigmaTotal, sigmaAbs, xMin, xMax, absRatio, thickness
  END TYPE shield

  TYPE (shield), ALLOCATABLE :: shld(:)

  INTEGER :: nParticles, nShields, parStatus, nParAbs, nParRef,     &
      nParLeak, currentRegion, collStatus
  REAL :: xInit, scatterAngleInit, xLocation, scatterAngle
  REAL :: probAbs, probRef, probLeak

END MODULE allData


PROGRAM multiRegionShield

  USE allData

  IMPLICIT NONE

  INTEGER :: iParticle, iShield, unitProbFile
  CHARACTER(80) :: nameProbFile

  ! Initialization
  xInit = 0.0
  scatterAngleInit = 1.0

  CALL RANDOM_SEED()

  PRINT*, 'Enter number of shields'
  READ*, nShields
  PRINT*, 'Enter number of particles'
  READ*, nParticles

  ALLOCATE(shld(nShields))

  DO iShield = 1, nShields
    PRINT*, 'Enter shield thickness for shield', iShield
    READ*, shld(iShield)%thickness
    PRINT*, 'Enter shield total and absorption c/s for shield',     &
        iShield
    READ*, shld(iShield)%sigmaTotal, shld(iShield)%sigmaAbs
    shld(iShield)%absRatio =                                        &
        shld(iShield)%sigmaAbs/shld(iShield)%sigmaTotal
    IF (iShield .EQ. 1) THEN
      shld(iShield)%xMin = xInit
    ELSE
      shld(iShield)%xMin = shld(iShield-1)%xMax
    END IF
    shld(iShield)%xMax = shld(iShield)%xMin + shld(iShield)%thickness
    PRINT*, 'shield', iShield, 'min, max:', shld(iShield)%xMin,     &
        shld(iShield)%xMax
  END DO

  DO iParticle = 1, nParticles

!   PRINT*, 'particle number:', iParticle

    ! Initial x and mu values
    xLocation = xInit
    scatterAngle = scatterAngleInit
    parStatus = 0
    currentRegion = 1
    collStatus = 0

    DO WHILE (parStatus .EQ. 0)

      CALL freeFlight

      IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1)                 &
          CALL getInteraction 

      IF (parStatus .EQ. 0 .AND. collStatus .EQ. 1)                 &
          CALL getScatterAngle

!     PRINT*, 'Particle region:', currentRegion
!     PRINT*, xLocation, scatterAngle

    END DO

!   PRINT*, 'particle status', parStatus

  END DO

  probLeak = 1.0*nParLeak/nParticles
  probRef = 1.0*nParRef/nParticles
  probAbs = 1.0*nParAbs/nParticles

  PRINT*, probLeak, probRef, probAbs

  unitProbFile = 101
  nameProbFile = 'probShield.dat'

  OPEN (UNIT = unitProbFile, FILE = nameProbFile,                   &
      POSITION = 'append', FORM = 'formatted', ACTION = 'write')
  WRITE(unitProbFile,501) nParticles, nParLeak, nParRef, nParAbs,   &
      probLeak, probRef, probAbs
  CLOSE(unitProbFile)

501 FORMAT (4(i8.8, 1X), 3(e12.5, 1X))

END PROGRAM multiRegionShield

SUBROUTINE freeFlight

  USE allData

  IMPLICIT NONE

  REAL :: randNum, pathLength

! PRINT*, 'in freeFlight'

  CALL RANDOM_NUMBER(randNum)

  pathLength = -LOG(randNum)/shld(currentRegion)%sigmaTotal
  xLocation = xLocation + pathLength*scatterAngle

! PRINT*, pathLength, xLocation, shld(currentRegion)%xMax

  IF ((xLocation .GT. shld(currentRegion)%xMax)) THEN
    xLocation = shld(currentRegion)%xMax
    currentRegion = currentRegion + 1
    IF (currentRegion .GT. nShields) THEN
      parStatus = 1
      nParLeak = nParLeak + 1
!     PRINT*, 'Particle leaked'
    END IF
  ELSE IF (xLocation .LT. shld(currentRegion)%xMin) THEN
    xLocation = shld(currentRegion)%xMin
    currentRegion = currentRegion - 1
    IF (currentRegion .LT. 1) THEN
      parStatus = 2
      nParRef = nParRef + 1
!     PRINT*, 'Particle reflected'
    END IF
  ELSE
    collStatus = 1
  END IF

! PRINT*, pathLength, xLocation, shld(currentRegion)%xMax

END SUBROUTINE freeFlight

SUBROUTINE getInteraction

  USE allData

  IMPLICIT NONE

  REAL :: randNum

! PRINT*, 'in getInteraction'

  CALL RANDOM_NUMBER(randNum)

  IF (randNum .LT. shld(currentRegion)%absRatio) THEN
    parStatus = 3
    nParAbs = nParAbs + 1
!   PRINT*, 'Particle absorbed'
  END IF

! PRINT*, 'parStatus', parStatus

END SUBROUTINE getInteraction

SUBROUTINE getScatterAngle

  USE allData

  IMPLICIT NONE

  REAL :: randNum, pi, mu_0, phi_0, mu_prime

! PRINT*, 'in getScatterAngle'

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

! PRINT*, 'scatterAngle', scatterAngle

END SUBROUTINE getScatterAngle
