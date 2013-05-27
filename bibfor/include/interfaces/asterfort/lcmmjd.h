        interface
          subroutine lcmmjd(taur,materf,ifa,nmat,nbcomm,dt,ir,is,nbsys&
     &,nfs,nsg,hsr,vind,dy,dpdtau,dprdas,dhrdas,hr,dpr,sgnr,iret)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: taur
            real(kind=8) :: materf(nmat*2)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            real(kind=8) :: dt
            integer :: ir
            integer :: is
            integer :: nbsys
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: vind(36)
            real(kind=8) :: dy(12)
            real(kind=8) :: dpdtau
            real(kind=8) :: dprdas
            real(kind=8) :: dhrdas
            real(kind=8) :: hr
            real(kind=8) :: dpr
            real(kind=8) :: sgnr
            integer :: iret
          end subroutine lcmmjd
        end interface
