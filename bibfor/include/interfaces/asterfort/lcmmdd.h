        interface
          subroutine lcmmdd(taus,coeft,ifa,nmat,nbcomm,is,nbsys,nfs,&
     &nsg,hsr,vind,dy,dt,rp,nuecou,dalpha,dgamma,dp,iret)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: taus
            real(kind=8) :: coeft(nmat)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            integer :: is
            integer :: nbsys
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: vind(*)
            real(kind=8) :: dy(12)
            real(kind=8) :: dt
            real(kind=8) :: rp
            integer :: nuecou
            real(kind=8) :: dalpha
            real(kind=8) :: dgamma
            real(kind=8) :: dp
            integer :: iret
          end subroutine lcmmdd
        end interface
