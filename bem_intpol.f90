!/////////////////////////////////////////////////////////

!#define _BEM_INTPOL_

!/////////////////////////////////////////////////////////
!
!  BEM_INTPOL
!
!> @brief Basic interpolation: from arrays of values and associated images, find the closest image for a given value x
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BEM_INTPOL(X,NVAL,TVAL,TIM, Y) 

!/////////////////////////////////////////////////////////

!USE MOD_KINDS

IMPLICIT NONE

!INTERFACE
!#include <finalize_inc.fpp>
!END INTERFACE

!/////////////////////////////////////////////////////////

REAL*8, INTENT(in) :: X

REAL*8, INTENT(out) :: Y

INTEGER, INTENT(in) :: NVAL

REAL*8, INTENT(in) :: TVAL(361), TIM(361)

!/////////////////////////////////////////////////////////

INTEGER :: INDX

REAL*8  :: COEF

!/////////////////////////////////////////////////////////

IF( (X .LT. TVAL(1)) ) THEN
    WRITE(*,*) "LOWER INTERPOLATION ERROR OCCURRED"
    !CALL ERROR("INTERPOLATION ERROR")
ENDIF

IF( (X .GT. TVAL(NVAL)) ) THEN
    WRITE(*,*) "UPPER INTERPOLATION ERROR OCCURRED"
    !CALL ERROR("INTERPOLATION ERROR")
ENDIF

INDX = 1

DO WHILE( (X .GT. TVAL(INDX+1)) .AND. (INDX .LT. NVAL-1) )
    INDX = INDX + 1
ENDDO

COEF = (X - TVAL(INDX))/(TVAL(INDX+1)-TVAL(INDX))

Y = (TIM(INDX+1) - TIM(INDX)) * COEF + TIM(INDX)

!/////////////////////////////////////////////////////////

END SUBROUTINE BEM_INTPOL

!/////////////////////////////////////////////////////////
