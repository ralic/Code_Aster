        interface
          subroutine lcmmfe(taus,coeft,materf,ifa,nmat,nbcomm,necoul,&
     &is,nbsys,vind,dy,rp,alphap,gammap,dt,dalpha,dgamma,dp,crit,sgns,&
     &nfs,nsg,hsr,iret)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: taus
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: materf(nmat)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            character(len=16) :: necoul
            integer :: is
            integer :: nbsys
            real(kind=8) :: vind(*)
            real(kind=8) :: dy(*)
            real(kind=8) :: rp
            real(kind=8) :: alphap
            real(kind=8) :: gammap
            real(kind=8) :: dt
            real(kind=8) :: dalpha
            real(kind=8) :: dgamma
            real(kind=8) :: dp
            real(kind=8) :: crit
            real(kind=8) :: sgns
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            integer :: iret
          end subroutine lcmmfe
        end interface
