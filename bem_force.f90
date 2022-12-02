!/////////////////////////////////////////////////////////

!#define _BEM_FORCE_

!/////////////////////////////////////////////////////////
!
!  BEM_FORCE
!
!> @brief Computes the torque and force components by summation on each blade section
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BEM_FORCE(ROT_SPEED,VELOCITY,NEL,TRADIUS,TCHORD,TGAMMA,NVALUES,TABCISSE,TCL,TCD,FX,TORQUE, POWER)

!/////////////////////////////////////////////////////////

USE MOD_BET

IMPLICIT NONE

!INTERFACE
!#include<bem_inc.fpp>
!END INTERFACE

!/////////////////////////////////////////////////////////

REAL*8, INTENT(in) :: ROT_SPEED, VELOCITY

INTEGER, INTENT(in) :: NVALUES, NEL

REAL*8, INTENT(in) :: TABCISSE(1:500,1:NEL),TCL(1:500,1:NEL),TCD(1:500,1:NEL)

REAL*8, INTENT(in) :: TRADIUS(1:NEL), TCHORD(1:NEL), TGAMMA(1:NEL)

REAL*8, INTENT(out) :: FX, TORQUE, POWER

!/////////////////////////////////////////////////////////

INTEGER :: I,J

REAL*8 :: ALPHA

REAL*8 :: R, DR, C, G, LAMBDAR, A_INIT, A_P_INIT, A, A_P, BETA, DL, DD, W, CL, CD, DFX, DFTHETA, DT, Q

!/////////////////////////////////////////////////////////

PRINT *, "SECTION RADIUS : ", TRADIUS(1:NEL)
PRINT *, "SECTION CHORD : ", TCHORD(1:NEL)
PRINT *, "SECTION PICH ANGLE(rad) : ", TGAMMA(1:NEL)
PRINT *, " "

DR = (TRADIUS(NEL)-TRADIUS(1))/BET_NSEC

FX = 0.0 
TORQUE = 0.0 

DO I=1,BET_NSEC

    !PLACE R AT THE MIDDLE OF THE SECTION I
    R = (I-0.5)*DR + TRADIUS(1)

    DO J = 2,NEL

    IF (R < TRADIUS(J)) THEN

        PRINT *, "R = ", R
    
        !LOCAL TSR
        LAMBDAR = ROT_SPEED*R/VELOCITY
    
        !INTERPOLATION OF LOCAL CHORD
        CALL BEM_INTPOL(R,NEL,TRADIUS,TCHORD, C)
        PRINT  *, "C = ", C
    
        !INTERPOLATION OF LOCAL GAMMA
        CALL BEM_INTPOL(R,NEL,TRADIUS,TGAMMA, G)
        PRINT  *, "G = ", G*180/3.1416
    
        !!INITIALIZE COEFFICIENTS WITH INITIAL GUESS
        !! BET METHOD DOES NOT REQUIRE THIS STEP
        CALL BEM_INIT_COEF(ROT_SPEED,VELOCITY,NEL,R,C,G,NVALUES,TABCISSE(1:361, J),TCL(1:361, J),TCD(1:361, J),A_INIT,A_P_INIT)
    
        !!ITERATIONS FOR CALCULATING VALUES OF A ALPHA AND BETA
        CALL BEM_ITERATE(ROT_SPEED,VELOCITY,A_INIT,A_P_INIT,R,C,G,LAMBDAR,NVALUES,TABCISSE,TCL(1:361, J),TCD(1:361, J),A,A_P,ALPHA,BETA, I)
        
        print*, "B = ", BETA*180/3.1416
        print*, "A = ", ALPHA*180/3.1416
        !!!!! OPTION 1: DIRECT CALCULATION
        
        !pause

        !RELATIVE VELOCITY
        W = SQRT((VELOCITY+A)**2 + (ROT_SPEED*R-A_P)**2) ! BEM:: A VERIFIER: POSSIBLE DIVISION PAR 0?
        !W = SQRT(VELOCITY**2 + (ROT_SPEED * R)**2)
        
        !BETA = ATAN(VELOCITY/(ROT_SPEED * R))
        
        PRINT *, "W = ", W
        
        !PRINT *, "TABULATED ANGLE: ",TABCISSE(1:180, J)
        !PRINT *, "CL: ", TCL(1:180, J)
        !PRINT *, "CD: ", TCD(1:180, J)
    
        !LIFT AND DRAG COEFFICIENTS
        CALL BEM_INTPOL(ALPHA,NVALUES,TABCISSE(1:361, J),TCL(1:361, J), CL)
        CALL BEM_INTPOL(ALPHA,NVALUES,TABCISSE(1:361, J),TCD(1:361, J), CD)

        PRINT *, "CL = ", CL
        PRINT *, "CD = ", CD
    
        !LOCAL LIFT AND DRAG FORCES ON EACH BLADE
        DL = CL*0.5d0 *BET_RHO*W*W*C*DR
        DD = CD*0.5d0 *BET_RHO*W*W*C*DR
        PRINT *, "DL = ", DL
        PRINT *, "DD = ", DD

        !IF (R == NINT(0.2*BET_R) .OR. R == NINT(0.75*BET_R) .OR. R == NINT(0.9*BET_R)) THEN
            CALL WRITE_AOACLCD(R, ALPHA, CL, CD)
        !END IF
    
        !LOCAL TANGENTIAL AND AXIAL FORCE ON EACH BLADE
!        IF (A > 0) THEN
!        DFX = -DL*SIN(-PI/2+BETA) - DD*SIN(BETA)
!        DFTHETA = DL*COS(-PI/2+BETA) + DD*COS(BETA)
        DFX = DL*COS(BETA) - DD*SIN(BETA)
        DFTHETA = DL*SIN(BETA) + DD*COS(BETA)
            !DFTHETA = -(DL*SIN(PI/2-BETA) + DD*COS(BETA))
!        ELSE
!            DFX = DL*SIN(PI/2-BETA) + DD*SIN(BETA)
!            DFTHETA = DL*COS(-PI/2-BETA) + DD*COS(BETA)
!            !DFTHETA = -(DL*COS(PI/2-BETA) - DD*COS(BETA))
!        END IF
    
        !MULTIPLY BY NUMBER OF BLADES
        DFTHETA = BET_NBLADES*DFTHETA
        DFX = BET_NBLADES*DFX
    
        !LOCAL TORQUE
        DT = DFTHETA*R

        ! FOR ELEMENT IN WINDMILL STATE, NEGLECT ITS FORCES
        IF (ISNAN(DFX)) THEN
            DFX = 0.0D0
        END IF

        IF (ISNAN(DT)) THEN 
            DT = 0.0D0
        END IF

        !!!!! TIP LOSS CORRECTION 
    
        !CORRECTION FACTOR
        !Q = (2.0d0/PI)*ACOS(EXP((-BET_NBLADES/2.0d0)*(1.0d0-R/BET_R)/((R/BET_R)*SIN(BETA))))
        Q = (2.0d0 /PI)*ACOS(EXP(-BET_NBLADES*(BET_R-R)/(2.0 *R*SIN(BETA))))
        IF (ISNAN(Q)) Q = 1
    
        !LOCAL TORQUE AND AXIAL FORCE
        DFX = Q*DFX
        DT = Q*DT
        
        PRINT *, "DFX = ", DFX
    
        !PRINT *, "Q = ",Q  
        !ADD INTO GLOBAL AXIAL FORCE AND TORQUE VALUES
        FX = FX + DFX
        TORQUE = TORQUE + DT


        PRINT *, "FX = ", FX

        !pause
        EXIT
    
    END IF

    END DO


END DO

POWER = ROT_SPEED * TORQUE

!/////////////////////////////////////////////////////////

END SUBROUTINE BEM_FORCE
