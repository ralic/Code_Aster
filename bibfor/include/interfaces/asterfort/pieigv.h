        interface
          subroutine pieigv(neps,tau,imate,vim,epsm,epspc,epsdc,typmod&
     &,etamin,etamax,copilo)
            integer :: neps
            real(kind=8) :: tau
            integer :: imate
            real(kind=8) :: vim(2)
            real(kind=8) :: epsm(neps)
            real(kind=8) :: epspc(neps)
            real(kind=8) :: epsdc(neps)
            character(len=8) :: typmod(2)
            real(kind=8) :: etamin
            real(kind=8) :: etamax
            real(kind=8) :: copilo(2,2)
          end subroutine pieigv
        end interface
