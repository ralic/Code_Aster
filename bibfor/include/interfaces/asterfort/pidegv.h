        interface
          subroutine pidegv(neps,tau,epsm,epsp,epsd,copilo)
            integer :: neps
            real(kind=8) :: tau
            real(kind=8) :: epsm(neps)
            real(kind=8) :: epsp(neps)
            real(kind=8) :: epsd(neps)
            real(kind=8) :: copilo(2,2)
          end subroutine pidegv
        end interface
