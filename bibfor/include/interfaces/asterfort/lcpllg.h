        interface
          subroutine lcpllg(toler,itmax,mod,nbmat,mater,nr,nvi,deps,&
     &sigd,vind,seuil,icomp,sigf,vinf,devg,devgii,irtet)
            integer :: nbmat
            real(kind=8) :: toler
            integer :: itmax
            character(len=8) :: mod
            real(kind=8) :: mater(nbmat,2)
            integer :: nr
            integer :: nvi
            real(kind=8) :: deps(6)
            real(kind=8) :: sigd(6)
            real(kind=8) :: vind(*)
            real(kind=8) :: seuil
            integer :: icomp
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(*)
            real(kind=8) :: devg(6)
            real(kind=8) :: devgii
            integer :: irtet
          end subroutine lcpllg
        end interface
