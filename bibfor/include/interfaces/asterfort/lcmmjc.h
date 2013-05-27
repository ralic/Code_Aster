        interface
          subroutine lcmmjc(coeft,ifa,nmat,nbcomm,ir,is,necrci,dgamms,&
     &alphmr,dalpha,sgnr,daldgr)
            integer :: nmat
            real(kind=8) :: coeft(nmat)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            integer :: ir
            integer :: is
            character(len=16) :: necrci
            real(kind=8) :: dgamms
            real(kind=8) :: alphmr
            real(kind=8) :: dalpha
            real(kind=8) :: sgnr
            real(kind=8) :: daldgr
          end subroutine lcmmjc
        end interface
