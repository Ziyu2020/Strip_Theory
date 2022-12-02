!/////////////////////////////////////////////////////////
!
!  BEM_FINDLOC
!
!> @brief Read the tabulated values of Cl and Cd
!> @author Ziyu GE
!
!/////////////////////////////////////////////////////////

SUBROUTINE BET_FINDLOC(ARRAY, NUMBER, INDEX)

!/////////////////////////////////////////////////////////

IMPLICIT NONE

REAL*8, DIMENSION(1:500), INTENT(IN) :: ARRAY

REAL*8, INTENT(IN):: NUMBER

INTEGER, INTENT(OUT) :: INDEX

!/////////////////////////////////////////////////////////

DO INDEX = 1, SIZE(ARRAY,1)

    IF (ARRAY(INDEX) == NUMBER) THEN

        EXIT

    END IF

END DO

END SUBROUTINE