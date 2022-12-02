!/////////////////////////////////////////////////////////

!#define _BEM_READ_TABULATED_VALUES_

!/////////////////////////////////////////////////////////
!
!  BEM_READ_TABULATED_VALUES
!
!> @brief Read the tabulated values of Cl and Cd
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BEM_READ_TABULATED_VALUES(NEL, NVALUES,TABCISSE,TCL,TCD)

!/////////////////////////////////////////////////////////

USE MOD_BET

IMPLICIT NONE

!INTERFACE
!#include<io_inc.fpp>
!END INTERFACE

!/////////////////////////////////////////////////////////

INTEGER, INTENT(in) :: NEL

REAL*8, INTENT(out) :: TABCISSE(1:500,1:NEL),TCL(1:500,1:NEL),TCD(1:500,1:NEL)

INTEGER, INTENT(out):: NVALUES

CHARACTER(LEN=1) :: NEL_CHAR

!/////////////////////////////////////////////////////////

REAL*8 :: P0(1:4, 1:2), P1(1:2)

INTEGER :: J, K, I, ERROR, START_INDEX

REAL*8 xtemp

!/////////////////////////////////////////////////////////
TABCISSE = -1000
TCL = 0
TCD = 0

PRINT *, "**********************************"
PRINT *, "READ AND EXTRAPOLATE AIRFOIL POLAR"

DO J = 1,NEL

    WRITE(NEL_CHAR, "(I1)") J

    !OPEN(UNIT=J,FILE=TRIM("polar_auto"//NEL_CHAR//".dat"))
    OPEN(UNIT=J,FILE=TRIM("polar_auto1.dat"))

    I = 181
    K = 0  ! NUMBER OF CL/CD FROM XFOIL, WHICH WILL BE KEPT IN FOLLOWING EXTRAPOLATION
    DO
    !READ(1,*) NVALUES
    !ALLOCATE(TABCISSE(1:NVALUES),TCL(1:NVALUES),TCD(1:NVALUES))

        READ(J,*, IOSTAT=ERROR) TABCISSE(I,J), TCL(I,J), TCD(I,J)
        IF(ERROR/=0) EXIT
        I = I + 1
        K = K + 1
    ENDDO

    CLOSE(J)

    TABCISSE(1:500, J) = TABCISSE(1:500, J)*PI/180.0D0

    !//////////////////////////
    ! EXTRAPOLATION PROCESS
    !//////////////////////////

    NVALUES = 361  ! WHERE EXTRAPOLATE ENDS
    xtemp = 0.0d0  ! FIND LOCATION OF 0 AOA
    
    CALL BET_FINDLOC(TABCISSE(1:500, J), xtemp, START_INDEX)   !  WHERE EXTRAPOLATE STARTS
    
    !CALL BET_EXTRAPOLATE(TABCISSE(1:500, J), TCL(1:500, J), TCD(1:500, J), START_INDEX, NVALUES, NEL)
    CALL BET_EXTRAPOLATE_SIN(TABCISSE(1:500, J), TCL(1:500, J), TCD(1:500, J), START_INDEX, NVALUES, NEL, K)
    
    OPEN(UNIT=11, FILE="CL_360.dat")
    DO I = 1,361
        WRITE(11, '(3(F10.5))') TABCISSE(I, J)*180.0D0/PI, TCL(I, J), TCD(I, J)
    END DO
    CLOSE(11)

END DO

PRINT *, "MULTI-SECTION POLARS HAVE BEEN READ AND EXTRAPOLTAED!!!"
PRINT *, "*******************************************************"

!/////////////////////////////////////////////////////////

END SUBROUTINE BEM_READ_TABULATED_VALUES

!/////////////////////////////////////////////////////////
