        interface
          subroutine lcmmji(coeft,ifa,nmat,nbcomm,necris,nfs,nsg,hsr,&
     &is,ir,pr,drdps)
            integer :: nsg
            integer :: nmat
            real(kind=8) :: coeft(nmat)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            character(len=16) :: necris
            integer :: nfs
            real(kind=8) :: hsr(nsg,nsg)
            integer :: is
            integer :: ir
            real(kind=8) :: pr
            real(kind=8) :: drdps
          end subroutine lcmmji
        end interface
