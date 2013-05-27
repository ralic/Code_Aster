        interface
          subroutine lcmmjg(comp,nmat,nbcomm,cpmono,hsr,dt,nvi,vind,yd&
     &,dy,itmax,toler,materf,sigf,fkooh,nfs,nsg,toutms,pgl,msnst,gamsns,&
     &dfpdga,iret)
            integer :: nsg
            integer :: nfs
            integer :: nmat
            character(len=16) :: comp(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: dt
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: yd(*)
            real(kind=8) :: dy(*)
            integer :: itmax
            real(kind=8) :: toler
            real(kind=8) :: materf(nmat*2)
            real(kind=8) :: sigf(6)
            real(kind=8) :: fkooh(6,6)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: msnst(3,3,nsg)
            real(kind=8) :: gamsns(3,3)
            real(kind=8) :: dfpdga(3,3,nsg)
            integer :: iret
          end subroutine lcmmjg
        end interface
