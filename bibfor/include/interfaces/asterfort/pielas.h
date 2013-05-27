        interface
          subroutine pielas(ndim,npg,kpg,compor,typmod,mate,elgeom,&
     &lgpg,vim,epsm,epsp,epsd,sigma,etamin,etamax,tau,copilo)
            integer :: lgpg
            integer :: npg
            integer :: ndim
            integer :: kpg
            character(len=16) :: compor(*)
            character(len=8) :: typmod(*)
            integer :: mate
            real(kind=8) :: elgeom(10,*)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: epsm(6)
            real(kind=8) :: epsp(6)
            real(kind=8) :: epsd(6)
            real(kind=8) :: sigma(6)
            real(kind=8) :: etamin
            real(kind=8) :: etamax
            real(kind=8) :: tau
            real(kind=8) :: copilo(5,npg)
          end subroutine pielas
        end interface
