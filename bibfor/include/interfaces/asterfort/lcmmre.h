        interface
          subroutine lcmmre(typmod,nmat,materd,materf,comp,nbcomm,&
     &cpmono,pgl,nfs,nsg,toutms,hsr,nr,nvi,vind,itmax,toler,timed,timef,&
     &yd,yf,deps,dy,r,iret)
            integer :: nr
            integer :: nsg
            integer :: nfs
            integer :: nmat
            character(len=8) :: typmod
            real(kind=8) :: materd(nmat*2)
            real(kind=8) :: materf(nmat*2)
            character(len=16) :: comp(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            integer :: nvi
            real(kind=8) :: vind(*)
            integer :: itmax
            real(kind=8) :: toler
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: yd(nr)
            real(kind=8) :: yf(nr)
            real(kind=8) :: deps(*)
            real(kind=8) :: dy(nr)
            real(kind=8) :: r(nr)
            integer :: iret
          end subroutine lcmmre
        end interface
