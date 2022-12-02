!/////////////////////////////////////////////////////////

!#define _BEM_WRITE_FORCE_

!/////////////////////////////////////////////////////////
!
!  BEM_WRITE_FORCE
!
!> @brief Write force and torque output for a given rotation speed
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BEM_WRITE_FORCE(FX, TORQUE, POWER, TIME, ROT_SPEED)

!/////////////////////////////////////////////////////////

!USE MOD_KINDS
!USE MOD_FILE
!USE MOD_SIMU
!USE MOD_STDOUT
USE MOD_BET
!USE MOD_PARA

IMPLICIT NONE

!INTERFACE
!#include<bem_inc.fpp>
!END INTERFACE

!/////////////////////////////////////////////////////////

REAL*8 , INTENT(in) :: FX, TORQUE, POWER

REAL*8, INTENT(in) :: TIME, ROT_SPEED

!/////////////////////////////////////////////////////////

CHARACTER(LEN=255) :: PATH

LOGICAL :: EXISTS

REAL*8 :: n, CT, CQ, J, ETA, ETA_0


!/////////////////////////////////////////////////////////

INTEGER :: PARA_RANK = 0

IF(PARA_RANK .EQ. 0) THEN
!IF(STDOUT) THEN

    PATH = TRIM(ADJUSTL(BET_FOUT))
    
    INQUIRE(FILE=PATH, EXIST=EXISTS)

    IF(EXISTS) THEN

        OPEN(UNIT=1,FILE=PATH,FORM="formatted",POSITION="APPEND")
        WRITE(1,'("#", 7(A8 ,3X), "#")') "RPM", "FX","TORQUE", "CT", "CQ", "POWER" , "EFF_0"

    ELSE

        OPEN(UNIT=1,FILE=PATH,FORM="formatted",STATUS="REPLACE")
        WRITE(1,'("#", 7(A8 ,3X), "#")') "RPM", "FX","TORQUE", "CT", "CQ", "POWER" , "EFF_0"

    ENDIF

    !WRITE(1,'(4('//RP_FORMAT//',1X))') TIME , FX, FTHETA, TORQUE
    n = ROT_SPEED /(2.0d0*PI)
!    CT = FX/( BET_RHO * (n**2) * (BET_R*2)**4 )
!    CQ = TORQUE / ( BET_RHO * (n**2) * (BET_R*2)**5)
    CT = FX/( BET_RHO * (ROT_SPEED*BET_R)**2 * (PI*BET_R**2) )
    CQ = TORQUE/( BET_RHO * (ROT_SPEED*BET_R)**2 * (PI*BET_R**3) )
    J = 0.001d0/ (n * 2.0d0 * BET_R)
    !ETA = CT * J / (2.0d0 * PI * CQ)
    ETA = 0.01 * FX / (ROT_SPEED * TORQUE)
    ! TODO HERE
    !ETA_0 = FX**1.5 / (POWER*SQRT(2*BET_RHO*PI*BET_R*BET_R))
    ETA_0 = CT**1.5 / (SQRT(2.0D0)*CQ)

    WRITE(1,'(F6.1,1X, 6(F10.4,1X))') n*60.0d0, FX, TORQUE, CT, CQ, POWER, ETA_0

    CLOSE(1)

ENDIF

IF (ISNAN(ETA_0)) THEN 
    CALL SYSTEM("del "//PATH)
END IF

!/////////////////////////////////////////////////////////

END SUBROUTINE BEM_WRITE_FORCE

!/////////////////////////////////////////////////////////
