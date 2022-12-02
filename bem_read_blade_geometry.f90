!/////////////////////////////////////////////////////////

!#define _BEM_READ_BLADE_GEOMETRY_

!/////////////////////////////////////////////////////////
!
!  BEM_READ_BLADE_GEOMETRY
!
!> @brief Read the blade geometry file
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BET_READ_BLADE_GEOMETRY(NEL,TRADIUS,TCHORD,TGAMMA)

!/////////////////////////////////////////////////////////

!USE MPI

!USE MOD_KINDS
!USE MOD_STDOUT
USE MOD_BET
!USE MOD_MATH

IMPLICIT NONE

!INTERFACE
!#include<io_inc.fpp>
!END INTERFACE

!/////////////////////////////////////////////////////////

INTEGER, INTENT(out) :: NEL

REAL*8, INTENT(out) :: TRADIUS(1:20), TCHORD(1:20), TGAMMA(1:20)

!/////////////////////////////////////////////////////////

INTEGER :: I

REAL*8 :: TEMP

!/////////////////////////////////////////////////////////
PRINT *, "***********************"
PRINT *, "READ BLADE GEOMETRY"

OPEN(UNIT=1,FILE=TRIM(BET_FBLADE))

READ(1,"(I2)") NEL

!ALLOCATE(TRADIUS(1:NEL), TCHORD(1:NEL), TGAMMA(1:NEL))

PRINT *, "BLADE GEOMETRY IS:"
PRINT *, ""

DO I=1,NEL
    READ(1,*) TRADIUS(I),TCHORD(I),TGAMMA(I)
    PRINT *, TRADIUS(I),TCHORD(I),TGAMMA(I)
ENDDO

CLOSE(1)

!TRADIUS = BET_R*TRADIUS
!TCHORD = BET_R*TCHORD
!TGAMMA = 90-TGAMMA

TGAMMA = TGAMMA*PI/180.0

!/////////////////////////////////////////////////////////

END SUBROUTINE BET_READ_BLADE_GEOMETRY

!/////////////////////////////////////////////////////////
