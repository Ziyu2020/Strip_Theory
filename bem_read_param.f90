!//////////////////////////////////////////////////////////

!#define _BET_READ_PARAM_

!//////////////////////////////////////////////////////////
!
!  BET_READ_PARAM
!
!> @brief Reads a parameter file to BET parameters
!> @author Ziyu GE
!
!//////////////////////////////////////////////////////////

SUBROUTINE BET_READ_PARAM

!//////////////////////////////////////////////////////////

USE MOD_BET
!USE MOD_KINDS

IMPLICIT NONE

!//////////////////////////////////////////////////////////

    OPEN(UNIT=1, FILE=BET_PARAM_FILE)

    READ(1,NML=PARAM_BET)

!    BET_RHO = 1.225
!
!    BET_NBLADES = 2
!
!    BET_R = 0.37
!
!    BET_NSEC = 100
!
!    BET_Lref = 0.05
!
!    BET_mu = 18E-6
!
!    BET_FBLADE = "blade_geometry.dat"
!
!    BET_FTAB = "polar.dat"
!
!    BET_FOUT = "force.dat" 

    PRINT *, "RHO = ", BET_RHO
    PRINT *, "GEOMETRY FILE IS: ", BET_FBLADE
    PRINT *, "POLAR FILE IS: ", BET_FTAB
    PRINT *, "OUTPUT FILE IS: ", BET_FOUT

    CLOSE(1)

!/////////////////////////////////////////////////////////

END SUBROUTINE BET_READ_PARAM

!/////////////////////////////////////////////////////////
