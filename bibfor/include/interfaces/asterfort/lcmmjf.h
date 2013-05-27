        interface
          subroutine lcmmjf(taus,coeft,materf,ifa,nmat,nbcomm,dt,&
     &necoul,is,ir,nbsys,vind,dy,nfs,nsg,hsr,rp,alphap,dalpha,gammap,&
     &dgamma,sgns,dgdtau,dgdal,dfdr,petith,iret)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: taus
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: materf(nmat)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            real(kind=8) :: dt
            character(len=16) :: necoul
            integer :: is
            integer :: ir
            integer :: nbsys
            real(kind=8) :: vind(*)
            real(kind=8) :: dy(*)
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: rp
            real(kind=8) :: alphap
            real(kind=8) :: dalpha
            real(kind=8) :: gammap
            real(kind=8) :: dgamma
            real(kind=8) :: sgns
            real(kind=8) :: dgdtau
            real(kind=8) :: dgdal
            real(kind=8) :: dfdr
            real(kind=8) :: petith
            integer :: iret
          end subroutine lcmmjf
        end interface
