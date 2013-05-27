        interface
          subroutine pipeds(ndim,typmod,tau,mate,vim,epsm,epspc,epsdc,&
     &etamin,etamax,a0,a1,a2,a3,etas)
            integer :: ndim
            character(len=8) :: typmod(*)
            real(kind=8) :: tau
            integer :: mate
            real(kind=8) :: vim(2)
            real(kind=8) :: epsm(6)
            real(kind=8) :: epspc(6)
            real(kind=8) :: epsdc(6)
            real(kind=8) :: etamin
            real(kind=8) :: etamax
            real(kind=8) :: a0
            real(kind=8) :: a1
            real(kind=8) :: a2
            real(kind=8) :: a3
            real(kind=8) :: etas
          end subroutine pipeds
        end interface
