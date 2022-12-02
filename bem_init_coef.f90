!/////////////////////////////////////////////////////////

!#define _BEM_INIT_COEF_

!/////////////////////////////////////////////////////////
!
!  BEM_INIT_COEF
!
!> @brief Gives an initial guess for the coefficients a and a_p
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BEM_INIT_COEF(ROT_SPEED,VELOCITY,NEL,R,C,G,NVALUES,TABCISSE,TCL,TCD,A_INIT,A_P_INIT)

!/////////////////////////////////////////////////////////

!USE MOD_KINDS
USE MOD_BET
!USE MOD_MATH

IMPLICIT NONE

!INTERFACE
!#include<bem_inc.fpp>
!END INTERFACE

!/////////////////////////////////////////////////////////

REAL*8 , INTENT(in) :: ROT_SPEED, VELOCITY

REAL*8 , INTENT(in) :: TABCISSE(1:500),TCL(1:500),TCD(1:500)

REAL*8 , INTENT(in) :: R, C, G

INTEGER, INTENT(in) :: NVALUES, NEL

REAL*8 , INTENT(out) :: A_INIT, A_P_INIT

!/////////////////////////////////////////////////////////

REAL*8  :: SIGMAP, BETA, INV_TSR, ALPHA, CL, CD, Q, K, W, B1, CIRCULATION

!/////////////////////////////////////////////////////////

!A_P_INIT = 0.00D0

!A_INIT = 0.5 * (-VELOCITY + SQRT(VELOCITY**2+4*(ROT_SPEED*R*A_P_INIT - A_P_INIT**2)))

!BETA = ATAN((VELOCITY+A_INIT)/(ROT_SPEED*R-A_P_INIT))

BETA = G - 0.01
ALPHA = G - BETA

CALL BEM_INTPOL(ALPHA,NVALUES,TABCISSE,TCL, CL)
CALL BEM_INTPOL(ALPHA,NVALUES,TABCISSE,TCD, CD)

W = SQRT((ROT_SPEED*R-A_P_INIT)**2 + (VELOCITY+A_INIT)**2)

CIRCULATION = 0.5*C*CL*W

Q = (2.0d0 /PI)*ACOS(EXP(-BET_NBLADES*(1-2*R/(2*BET_R))/(2.0*SIN(BETA))))

A_P_INIT = BET_NBLADES*CIRCULATION / (4*PI*R*Q)

A_INIT = 0.5 * (-VELOCITY + SQRT(VELOCITY**2+4*(ROT_SPEED*R*A_P_INIT - A_P_INIT**2)))

print*, "a, ap init = ", a_init, a_p_init
print *, " "

!pause

!/////////////////////////////////////////////////////////

END SUBROUTINE BEM_INIT_COEF

!/////////////////////////////////////////////////////////
