        interface
          subroutine burcvx(mod,nmat,materd,materf,timed,timef,nvi,&
     &vind,nr,sigd,deps,yd,yf,toler,seuil)
            integer :: nvi
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: vind(nvi)
            integer :: nr
            real(kind=8) :: sigd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: yd(13)
            real(kind=8) :: yf(13)
            real(kind=8) :: toler
            real(kind=8) :: seuil
          end subroutine burcvx
        end interface
