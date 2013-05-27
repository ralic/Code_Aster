        interface
          subroutine lcmmkr(taus,coeft,cisa2,ifa,nmat,nbcomm,is,nbsys,&
     &nfs,nsg,hsr,vind,dy,dt,dalpha,dgamma,dp,crit,sgns,iret)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: taus
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: cisa2
            integer :: ifa
            integer :: nbcomm(nmat,3)
            integer :: is
            integer :: nbsys
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: vind(*)
            real(kind=8) :: dy(*)
            real(kind=8) :: dt
            real(kind=8) :: dalpha
            real(kind=8) :: dgamma
            real(kind=8) :: dp
            real(kind=8) :: crit
            real(kind=8) :: sgns
            integer :: iret
          end subroutine lcmmkr
        end interface
