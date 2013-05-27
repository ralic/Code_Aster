        interface
          subroutine lcmmfi(coeft,ifa,nmat,nbcomm,necris,is,nbsys,vind&
     &,nsfv,dy,nfs,nsg,hsr,iexp,expbp,rp)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: coeft(nmat)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            character(len=16) :: necris
            integer :: is
            integer :: nbsys
            real(kind=8) :: vind(*)
            integer :: nsfv
            real(kind=8) :: dy(*)
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            integer :: iexp
            real(kind=8) :: expbp(*)
            real(kind=8) :: rp
          end subroutine lcmmfi
        end interface
