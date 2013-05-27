        interface
          subroutine lcmmj2(taur,materf,cpmono,ifa,nmat,nbcomm,dt,nsfv&
     &,nsfa,ir,is,nbsys,nfs,nsg,hsr,vind,dy,dgsdts,dksdts,dgrdbs,dksdbr,&
     &iret)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: taur
            real(kind=8) :: materf(nmat*2)
            character(len=24) :: cpmono(5*nmat+1)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            real(kind=8) :: dt
            integer :: nsfv
            integer :: nsfa
            integer :: ir
            integer :: is
            integer :: nbsys
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: vind(*)
            real(kind=8) :: dy(*)
            real(kind=8) :: dgsdts
            real(kind=8) :: dksdts
            real(kind=8) :: dgrdbs
            real(kind=8) :: dksdbr
            integer :: iret
          end subroutine lcmmj2
        end interface
