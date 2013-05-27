        interface
          subroutine nmmess(code,dp0,dp1,dp,fonc,nit,nitmax,iret)
            character(len=1) :: code
            real(kind=8) :: dp0
            real(kind=8) :: dp1
            real(kind=8) :: dp
            real(kind=8) :: fonc
            external fonc
            integer :: nit
            integer :: nitmax
            integer :: iret
          end subroutine nmmess
        end interface
