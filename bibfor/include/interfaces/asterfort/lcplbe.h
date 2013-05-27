        interface
          subroutine lcplbe(toler,itmax,nmat,materf,nvi,vind,sigf,vinf&
     &,elgeom,nseuil,irteti)
            integer :: nmat
            real(kind=8) :: toler
            integer :: itmax
            real(kind=8) :: materf(nmat,2)
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            real(kind=8) :: elgeom(*)
            integer :: nseuil
            integer :: irteti
          end subroutine lcplbe
        end interface
