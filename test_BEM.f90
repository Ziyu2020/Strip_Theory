!/////////////////////////////////////////////////////////

!#define _TEST_

!/////////////////////////////////////////////////////////
!
!  TEST_BEM
!
!> @brief test BEM: compute force output for a range of rotation speeds
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

PROGRAM TEST

!/////////////////////////////////////////////////////////

!USE MOD_KINDS
!USE MOD_SIMU
!USE MOD_STDOUT
!USE MOD_CPU
USE MOD_BET

!USE MPI

IMPLICIT NONE

!INTERFACE
!#include<bem_inc.fpp>
!#include<init_inc.fpp>
!END INTERFACE

!/////////////////////////////////////////////////////////

REAL*8 , ALLOCATABLE :: OMEGA(:)
!REAL*8  :: OMEGA

REAL*8  :: VIT, TIME, RPM

INTEGER :: I, IERR

REAL*8 :: RE, MACH

REAL*8, DIMENSION(1:20) :: TRADIUS, TCHORD, TGAMMA

REAL*8, DIMENSION(1:500, 1:20) :: TALPHA, TCL, TCD

INTEGER :: NEL, NVALUES

REAL*8  :: FX, TORQUE, POWER

!/////////////////////////////////////////////////////////
! READ CONTROL PARAMETERS
CALL BET_READ_PARAM

!PARAMETERS
    ALLOCATE(OMEGA(BET_RPM_NUMBER))
    
    DO I = 1, BET_RPM_NUMBER
        RPM = BET_RPM_START + (I-1)*BET_RPM_STEP
        OMEGA(I) = RPM/60.0d0 * PI * 2
    END DO

    !BET_VIT = 0.00

    TIME = 0.0 

DO I = 1, BET_RPM_NUMBER

    !INIT
    !CALL INIT_MPI
    !CALL INIT_IO

    !INPUT
    PRINT *, "INPUT PROCESS START..."
    PRINT *, " "

    PRINT *, "BLADE GEOMETRY INSERTING..."
    CALL BET_READ_BLADE_GEOMETRY(NEL,TRADIUS,TCHORD,TGAMMA)
    PRINT *, "BLADE GEOMETRY DONE"

    ! CALCULATE DIMENSIONLESS NUMBER
    CALL BET_RE_MACH(OMEGA(I), VIT, TRADIUS, RE, MACH, NEL)

    ! GENERATE POLAR FOR THIS CONFIGURATION
    CALL BET_POLAR(NEL, RE, MACH)

    ! READ POLAR
    PRINT *, "AIRFOIL POALAR INSERTING"
    CALL BEM_READ_TABULATED_VALUES(NEL, NVALUES,TALPHA,TCL,TCD)
    PRINT *, "AIRFOIL PLOAR DONE"
    PRINT *, " "


    !CALCULATION AND OUTPUT

    CALL BEM_FORCE(OMEGA(I),BET_VIT,NEL,TRADIUS,TCHORD,TGAMMA,NVALUES,TALPHA,TCL,TCD,FX,TORQUE, POWER)

    CALL BEM_WRITE_FORCE(FX, TORQUE, POWER, TIME, OMEGA(I))

    !pause

    !FINALIZE
    CALL BEM_FINALIZE(NEL)
    !/////////////////////////////////////////////////////////


ENDDO

END PROGRAM TEST

!/////////////////////////////////////////////////////////
