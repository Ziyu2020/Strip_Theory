!/////////////////////////////////////////////////////////
!
!  BEM_EXTRAPOLATE_SINUSOID
!
!> @brief Extrapolate the tabulated values of Cl and Cd
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BET_EXTRAPOLATE_SIN(TABCISSE, TCL ,TCD, START_INDEX, NVALUES, NEL, K)

!/////////////////////////////////////////////////////////

USE MOD_BET

IMPLICIT NONE

INTEGER, INTENT(IN) :: START_INDEX, NVALUES, NEL, K

REAL*8, INTENT(INOUT) :: TABCISSE(1:500), TCL(1:500), TCD(1:500)

REAL*8:: A1, B1, A2, B2, DELTA, RANGE_X, NUMBER_X, STEP_Y

INTEGER :: I, J

!/////////////////////////////////////////////////////////

!PRINT *, "A, B = ", A, B
!J = 1
B1 = 1.7D0
A1 = B1/2

!B2 = TCD(START_INDEX-1) * (1-SIN(TABCISSE(START_INDEX))**2/COS(TABCISSE(START_INDEX)))
!A2 = TCL(START_INDEX) - TCD(START_INDEX)*SIN(TABCISSE(START_INDEX))*COS(TABCISSE(START_INDEX))

B2 = 0.023 - 0.023*SIN(9.0D0* PI / 180.0D0)**2/COS(9.0D0* PI / 180.0D0)
A2 = 1.262 - 1.7D0*SIN(9.0D0* PI / 180.0D0)*COS(9.0D0* PI / 180.0D0)

TABCISSE(1) =  -180.0D0 * PI / 180.0D0
DELTA = 1.0D0 * PI / 180.0D0

DO I = 2, 180

    TABCISSE(I) = TABCISSE(I-1) + DELTA
    TCL(I) = A1*SIN(2*TABCISSE(I)) + A2*COS(TABCISSE(I))**2*SIN(TABCISSE(I))
    TCD(I) = B1*SIN(TABCISSE(I))**2 + B2*COS(TABCISSE(I))

END DO

DO I = 181 + K, NVALUES
    
    TABCISSE(I) = TABCISSE(I-1) + DELTA
    TCL(I) = A1*SIN(2*TABCISSE(I)) + A2*COS(TABCISSE(I))**2*SIN(TABCISSE(I))
    TCD(I) = B1*SIN(TABCISSE(I))**2 + B2*COS(TABCISSE(I))

END DO 

END SUBROUTINE