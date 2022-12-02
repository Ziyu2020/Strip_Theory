!//////////////////////////////////////////////////////////
!
!  BET_RENEW
!
!> @brief Rewrite Mesm input parameter file by calculated RE and MACH
!> @author Ziyu GE
!
!//////////////////////////////////////////////////////////

SUBROUTINE BET_RENEW(RE, MACH, ALPHA)

!//////////////////////////////////////////////////////////
IMPLICIT NONE

REAL*8, INTENT(in) :: RE, MACH, ALPHA

INTEGER :: LINE

CHARACTER(LEN=20) :: MESM_INPUT = "MEEUL.DAT"

CHARACTER(LEN=60), DIMENSION(1:20) :: C

REAL*8, DIMENSION(1:3, 1:5) :: NUMBER

INTEGER :: I

!//////////////////////////////////////////////////////////

    OPEN(UNIT=1, FILE = TRIM(ADJUSTL(MESM_INPUT)) )

    DO LINE = 1,2
        READ(1, '(A)') C(LINE)
    END DO
    
    ! READ ORIGINAL DATA
    READ(1, *) (NUMBER(1, I), I=1,5)
    READ(1, '(A)') C(3)
    READ(1, *) (NUMBER(2, I), I=1,3)
    READ(1, *) (NUMBER(3, I), I=1,3)
    READ(1, '(A)') C(4)

    CLOSE(1)

    ! REPLACE PARAMETERS
    NUMBER(1, 1) = MACH
    NUMBER(1, 5) = ALPHA
    NUMBER(2, 1) = RE

    OPEN(UNIT=2, FILE = TRIM(ADJUSTL(MESM_INPUT)), STATUS = "REPLACE")

    DO LINE = 1,2
        WRITE(2, "(A)") ADJUSTL(C(LINE))
    END DO

    ! REWRITE INPUT FILE
    WRITE(2, '(5(F5.2,1X))') (NUMBER(1, I), I=1,5)
    WRITE(2, '(A)') C(3)
    WRITE(2, '(F9.0, 2(F5.2,1X))') (NUMBER(2, I), I=1,3)
    WRITE(2, '(3(F5.2,1X))') (NUMBER(3, I), I=1,3)
    WRITE(2, '(A)') C(4)

    CLOSE(2)

END SUBROUTINE