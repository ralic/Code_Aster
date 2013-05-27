        interface
          subroutine pil000(typilo,compor,neps,tau,mat,vim,sigm,epsm,&
     &epsp,epsd,typmod,etamin,etamax,copilo)
            integer :: neps
            character(len=16) :: typilo
            character(len=16) :: compor(*)
            real(kind=8) :: tau
            integer :: mat
            real(kind=8) :: vim(*)
            real(kind=8) :: sigm(neps)
            real(kind=8) :: epsm(neps)
            real(kind=8) :: epsp(neps)
            real(kind=8) :: epsd(neps)
            character(len=8) :: typmod(*)
            real(kind=8) :: etamin
            real(kind=8) :: etamax
            real(kind=8) :: copilo(2,2)
          end subroutine pil000
        end interface
