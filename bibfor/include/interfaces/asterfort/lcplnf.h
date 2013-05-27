        interface
          subroutine lcplnf(loi,vind,nbcomm,nmat,cpmono,materd,materf,&
     &iter,nvi,itmax,toler,pgl,nfs,nsg,toutms,hsr,dt,dy,yd,yf,vinf,&
     &tampon,comp,sigd,sigf,deps,nr,mod,timed,timef,indi,vins,codret)
            integer :: nsg
            integer :: nfs
            integer :: nvi
            integer :: nmat
            character(len=16) :: loi
            real(kind=8) :: vind(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: materd(nmat,2)
            real(kind=8) :: materf(nmat,2)
            integer :: iter
            integer :: itmax
            real(kind=8) :: toler
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: dt
            real(kind=8) :: dy(*)
            real(kind=8) :: yd(*)
            real(kind=8) :: yf(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: tampon(*)
            character(len=16) :: comp(*)
            real(kind=8) :: sigd(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: deps(*)
            integer :: nr
            character(len=8) :: mod
            real(kind=8) :: timed
            real(kind=8) :: timef
            integer :: indi(7)
            real(kind=8) :: vins(nvi)
            integer :: codret
          end subroutine lcplnf
        end interface
