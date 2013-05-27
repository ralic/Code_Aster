        interface
          subroutine lcmmdc(coeft,ifa,nmat,nbcomm,alphap,is,ceff,&
     &dcdals)
            integer :: nmat
            real(kind=8) :: coeft(*)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            real(kind=8) :: alphap(12)
            integer :: is
            real(kind=8) :: ceff
            real(kind=8) :: dcdals
          end subroutine lcmmdc
        end interface
