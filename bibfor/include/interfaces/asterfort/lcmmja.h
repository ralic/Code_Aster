        interface
          subroutine lcmmja(comp,typmod,nmat,materf,timed,timef,itmax,&
     &toler,nbcomm,cpmono,pgl,nfs,nsg,toutms,hsr,nr,nvi,vind,df,yf,yd,dy&
     &,drdy,iret)
            integer :: nr
            integer :: nsg
            integer :: nfs
            integer :: nmat
            character(len=16) :: comp(*)
            character(len=8) :: typmod
            real(kind=8) :: materf(nmat*2)
            real(kind=8) :: timed
            real(kind=8) :: timef
            integer :: itmax
            real(kind=8) :: toler
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: df(3,3)
            real(kind=8) :: yf(*)
            real(kind=8) :: yd(*)
            real(kind=8) :: dy(*)
            real(kind=8) :: drdy(nr,nr)
            integer :: iret
          end subroutine lcmmja
        end interface
