        interface
          subroutine lcdpec(vind,nbcomm,nmat,ndt,cpmono,materf,iter,&
     &nvi,itmax,toler,pgl,nfs,nsg,toutms,hsr,dt,dy,yd,vinf,tampon,comp,&
     &sigf,df,nr,mod,codret)
            integer :: nsg
            integer :: nfs
            integer :: nmat
            real(kind=8) :: vind(*)
            integer :: nbcomm(nmat,3)
            integer :: ndt
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: materf(nmat*2)
            integer :: iter
            integer :: nvi
            integer :: itmax
            real(kind=8) :: toler
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: dt
            real(kind=8) :: dy(*)
            real(kind=8) :: yd(*)
            real(kind=8) :: vinf(*)
            real(kind=8) :: tampon(*)
            character(len=16) :: comp(*)
            real(kind=8) :: sigf(6)
            real(kind=8) :: df(3,3)
            integer :: nr
            character(len=8) :: mod
            integer :: codret
          end subroutine lcdpec
        end interface
