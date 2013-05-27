        interface
          subroutine lcmmvx(sigf,vin,nmat,materf,nbcomm,cpmono,pgl,nvi&
     &,hsr,nfs,nsg,toutms,timed,timef,deps,seuil)
            integer :: nsg
            integer :: nfs
            integer :: nvi
            integer :: nmat
            real(kind=8) :: sigf(6)
            real(kind=8) :: vin(nvi)
            real(kind=8) :: materf(nmat*2)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: hsr(nsg,nsg)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: timed
            real(kind=8) :: timef
            real(kind=8) :: deps(6)
            real(kind=8) :: seuil
          end subroutine lcmmvx
        end interface
