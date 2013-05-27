        interface
          subroutine lcmmdh(coeft,ifa,nmat,nbcomm,alphap,nfs,nsg,hsr,&
     &nbsys,is,nuecou,hs,soms1,soms2,soms3)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: coeft(*)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            real(kind=8) :: alphap(12)
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            integer :: nbsys
            integer :: is
            integer :: nuecou
            real(kind=8) :: hs
            real(kind=8) :: soms1
            real(kind=8) :: soms2
            real(kind=8) :: soms3
          end subroutine lcmmdh
        end interface
