!//////////////////////////////////////////////////////////
!
!  BET_POLAR
!
!> @brief Reads a parameter file to BET parameters
!> @author Ziyu GE
!
!//////////////////////////////////////////////////////////

SUBROUTINE BET_RE_MACH(ROT_SPEED, VELOCITY, TRADIUS, RE, MACH, NEL)
!//////////////////////////////////////////////////////////

USE MOD_BET

IMPLICIT NONE

INTEGER, INTENT(IN) :: NEL

REAL*8, INTENT(IN) :: VELOCITY

REAL*8, INTENT(IN) :: TRADIUS(1:500)

REAL*8, INTENT(IN) :: ROT_SPEED

REAL*8, INTENT(OUT) :: RE, MACH

REAL*8, ALLOCATABLE:: RE_POSITION(:), MACH_POSITION(:)

REAL*8, ALLOCATABLE :: V(:)

INTEGER :: POSITION

!//////////////////////////////////////////////////////////

    ALLOCATE(V(1:NEL), RE_POSITION(1:NEL), MACH_POSITION(1:NEL))

    DO POSITION = 1,NEL
        
        V(POSITION) = SQRT(VELOCITY**2 + (TRADIUS(POSITION)*ROT_SPEED)**2)
        !print *, BET_RHO,V(POSITION),BET_Lref,BET_mu
        RE_POSITION(POSITION) = BET_RHO * V(POSITION) * BET_Lref / BET_mu

        MACH_POSITION(POSITION) = V(POSITION) / 340
    
    END DO

    RE = 0
    MACH = 0

    DO POSITION = 1,NEL
        RE = RE + RE_POSITION(POSITION)
        MACH = MACH + MACH_POSITION(POSITION)
    END DO

    RE = RE/NEL
    MACH = MACH /NEL

!    POSITION = NINT(NEL*0.75)
!
!    V(1) = SQRT(VELOCITY**2 + (TRADIUS(POSITION)*ROT_SPEED)**2)
!
!    RE_POSITION(1) = BET_RHO * V(1) * BET_Lref / BET_mu
!    MACH_POSITION(1) = V(1) / 340
!
!    RE = RE_POSITION(1)
!    MACH = MACH_POSITION(1)

    print *, "  " 
    print *, "RE OVERALL = ", RE
    print *, "MA OVERALL = ", MACH

!/////////////////////////////////////////////////////////

END SUBROUTINE BET_RE_MACH

!/////////////////////////////////////////////////////////