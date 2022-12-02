!/////////////////////////////////////////////////////////

!#define _BEM_ITERATE_

!/////////////////////////////////////////////////////////
!
!  BEM_ITERATE
!
!> @brief Iterations on the coefficients a and a_p, for calculating a and angle beta
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BEM_ITERATE(ROT_SPEED,VELOCITY,A_INIT,A_P_INIT,R,C,G,LAMBDAR,NVALUES,TABCISSE,TCL,TCD,A,A_P,ALPHA,BETA,nsec)

!/////////////////////////////////////////////////////////

!USE MOD_KINDS
!USE MOD_STDOUT
USE MOD_BET
!USE MOD_MATH

IMPLICIT NONE

!INTERFACE
!#include<bem_inc.fpp>
!#include<io_inc.fpp>
!END INTERFACE

!/////////////////////////////////////////////////////////

REAL*8 , INTENT(in) :: ROT_SPEED, A_INIT, A_P_INIT, R, C, G, LAMBDAR

REAL*8 , INTENT(in) :: TABCISSE(1:500), TCL(1:500), TCD(1:500)

INTEGER, INTENT(in) :: NVALUES, nsec

REAL*8 , INTENT(out) :: A, A_P, BETA, ALPHA

!/////////////////////////////////////////////////////////

INTEGER :: NIT, NIT_RE

REAL*8  :: A_OLD, A_P_OLD, ERR_A, ERR_A_P
REAL*8  :: CL, CD, SIGMAP, Q, K, OMEGA, W, VELOCITY, CIRCULATION

CHARACTER(LEN=8) :: ISTRING, IDT_FORMAT
CHARACTER(LEN=255) :: PATH
LOGICAL :: EXISTS

!/////////////////////////////////////////////////////////

!CALCULATE LOCAL SOLIDITY SIGMAP
SIGMAP = BET_NBLADES*C/(2.0d0 *PI*R)

!INITIALIZE COEFFICIENTS 
A = A_INIT
A_P = A_P_INIT
    
!INITIALIZE ERR_A AND ERR_A_P GREATER THEN CRITERION
ERR_A = BEM_COEF_MAXVAR + 1.0 
ERR_A_P = BEM_COEF_MAXVAR + 1.0  
    
!INITIALIZE ITERATION COUNTER
NIT = 0

!ITERATIONS
DO WHILE( ABS(MAX(ERR_A,ERR_A_P) .GT. BEM_COEF_MAXVAR) .AND. (NIT .LE. BEM_COEF_MAXIT) )
!DO WHILE( (NIT .LE. BET_COEF_MAXIT) )

    !STORE PREVIOUS VALUES OF A AND AP
    A_OLD = A
    A_P_OLD = A_P
    PRINT *, "A = ", A
    PRINT *, "A_P = ", A_P

    !ANGLE BETA
    !BETA = ATAN( (1.0d0 + A)/(1.0d0 - A_P)/ LAMBDAR)
    BETA = ATAN((VELOCITY+A)/(ROT_SPEED*R-A_P)) 

    !CORRECTION FACTOR
    Q = (2.0d0 /PI)*ACOS(EXP(-BET_NBLADES*(1-2*R/(2*BET_R))/(2.0*SIN(BETA))))
    PRINT *, "Q = ", Q
    
    !INCIDENCE ANGLE
    ALPHA = G - BETA

    PRINT*, "G = ", G*180/3.1416
    PRINT*, "B = ", BETA*180/3.1416
    PRINT*, "A = ", ALPHA*180/3.1416
    !pause
    
    !LIFT AND DRAG COEFFICIENTS
    CALL BEM_INTPOL(ALPHA,NVALUES,TABCISSE,TCL, CL)
    CALL BEM_INTPOL(ALPHA,NVALUES,TABCISSE,TCD, CD)


    W = SQRT((ROT_SPEED*R-A_P)**2 + (VELOCITY+A)**2)
    PRINT *,  "W = ", W

    CIRCULATION = 0.5*C*CL*W
    
    A_P = BET_NBLADES*CIRCULATION / (4*PI*R*Q)
    A = 0.5 * (-VELOCITY + SQRT(VELOCITY**2+4*(ROT_SPEED*R*A_P - A_P**2)))

    PRINT *, "a, a_p it. = ", A, A_P

    !pause

    !COMPUTE ERROR BETWEEN OLD AND NEW VALUES
    ERR_A = ABS(A_OLD-A)
    ERR_A_P = ABS(A_P_OLD-A_P)

    !INCREMENT ITERATION COUNTER
    NIT = NIT+1

    !COMPUTE ALPHA AND BETA WITH THE LAST VALUES OF A AND A_P
    BETA = ATAN((VELOCITY+A)/(ROT_SPEED*R-A_P))
    ALPHA = G - BETA
    
    !OUTPUT       
    !CALL IO_FORMAT_INTEGER(BEM_NSEC, IDT_FORMAT)
    !WRITE(ISTRING,'('//TRIM(IDT_FORMAT)//')') NSEC
    !    
    !PATH = TRIM("OUTPUT/BEM_convergence")//TRIM(ADJUSTL(ISTRING))//".dat"
    ! 
    !INQUIRE(FILE=PATH, EXIST=EXISTS)
    !IF(EXISTS) THEN
    !    OPEN(UNIT=1,FILE=PATH,FORM="formatted",STATUS="OLD",POSITION="APPEND")
    !ELSE
    !    OPEN(UNIT=1,FILE=PATH,FORM="formatted",STATUS="REPLACE")
    !ENDIF
    !WRITE(1,'(I,2('//RP_FORMAT//',1X))') NIT, ERR_A, ERR_A_P
    !CLOSE(1)
ENDDO

!/////////////////////////////////////////////////////////

END SUBROUTINE BEM_ITERATE

!/////////////////////////////////////////////////////////
