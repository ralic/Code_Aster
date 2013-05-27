        interface
          subroutine piesgv(neps,tau,mat,vim,epsm,epsp,epsd,typmod,&
     &etamin,etamax,copilo)
            integer :: neps
            real(kind=8) :: tau
            integer :: mat
            real(kind=8) :: vim(3)
            real(kind=8) :: epsm(neps)
            real(kind=8) :: epsp(neps)
            real(kind=8) :: epsd(neps)
            character(len=8) :: typmod
            real(kind=8) :: etamin
            real(kind=8) :: etamax
            real(kind=8) :: copilo(2,2)
          end subroutine piesgv
        end interface
