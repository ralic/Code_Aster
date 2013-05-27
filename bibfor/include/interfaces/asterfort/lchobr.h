        interface
          subroutine lchobr(toler,itmax,mod,nbmat,materf,nr,nvi,depsm,&
     &sigm,vim,seuil,vp,vecp,icomp,sigp,vip,irtet)
            integer :: nbmat
            real(kind=8) :: toler
            integer :: itmax
            character(len=8) :: mod
            real(kind=8) :: materf(nbmat,2)
            integer :: nr
            integer :: nvi
            real(kind=8) :: depsm(6)
            real(kind=8) :: sigm(6)
            real(kind=8) :: vim(*)
            real(kind=8) :: seuil
            real(kind=8) :: vp(3)
            real(kind=8) :: vecp(3,3)
            integer :: icomp
            real(kind=8) :: sigp(6)
            real(kind=8) :: vip(*)
            integer :: irtet
          end subroutine lchobr
        end interface
