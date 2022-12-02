!//////////////////////////////////////////////////////////
!
!  BET_POLAR
!
!> @brief Reads a parameter file to BET parameters
!> @author Ziyu GE
!
!//////////////////////////////////////////////////////////

SUBROUTINE BET_POLAR(NEL, RE, MACH)

!//////////////////////////////////////////////////////////

USE MOD_BET

IMPLICIT NONE

INTEGER, INTENT(IN):: NEL

INTEGER :: SECTION, I

REAL*8, INTENT(IN) :: RE, MACH

REAL*8 :: ALPHA

CHARACTER(LEN=1) :: SECTION_CHAR  

!//////////////////////////////////////////////////////////

    DO SECTION = 1, 1

        WRITE(SECTION_CHAR, '(I1)') SECTION
        
        ! COPY SECTION AS INPUT AIRFOIL OF MSES
        CALL SYSTEM("copy section"//SECTION_CHAR//".dat blade.dat" )

        ALPHA = 0.0D0

        ! GENERATE MESH AT SMALL AOA
        CALL BET_RENEW(RE, MACH, 1.0D0)

        CALL SYSTEM("Mesm.exe")
        CALL SYSTEM("Tgrid1.exe")
        CALL SYSTEM("Meeul.exe")

        DO I = 1,20

            ! RENEW INPUT OF PARAMETERS
            CALL BET_RENEW(RE, MACH, ALPHA)
            
            ! CALL MSES SOFTWARE TO CALCULATE
            CALL SYSTEM("Meeul.exe")

            ! EXTRACT RESULTS
            CALL BET_WRITE_POLARS(SECTION_CHAR, ALPHA)
            ALPHA = ALPHA + 1.0D0

        END DO
    END DO

!/////////////////////////////////////////////////////////

END SUBROUTINE BET_POLAR

!/////////////////////////////////////////////////////////