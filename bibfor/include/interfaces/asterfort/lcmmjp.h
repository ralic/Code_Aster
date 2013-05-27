        interface
          subroutine lcmmjp(mod,nmat,mater,timed,timef,comp,nbcomm,&
     &cpmono,pgl,nfs,nsg,toutms,hsr,nr,nvi,itmax,toler,vinf,vind,dsde,&
     &drdy,option,iret)
            common/tdim/ ndt,ndi
              integer :: ndt
              integer :: ndi
            integer :: nr
            integer :: nsg
            integer :: nfs
            integer :: nmat
            character(len=8) :: mod
            real(kind=8) :: mater(*)
            real(kind=8) :: timed
            real(kind=8) :: timef
            character(len=16) :: comp(*)
            integer :: nbcomm(nmat,3)
            character(len=24) :: cpmono(5*nmat+1)
            real(kind=8) :: pgl(3,3)
            real(kind=8) :: toutms(nfs,nsg,6)
            real(kind=8) :: hsr(nsg,nsg)
            integer :: nvi
            integer :: itmax
            real(kind=8) :: toler
            real(kind=8) :: vinf(*)
            real(kind=8) :: vind(*)
            real(kind=8) :: dsde(6,*)
            real(kind=8) :: drdy(nr,nr)
            character(len=16) :: option
            integer :: iret
          end subroutine lcmmjp
        end interface
