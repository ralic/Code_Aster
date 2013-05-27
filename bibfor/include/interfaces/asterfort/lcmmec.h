        interface
          subroutine lcmmec(coeft,ifa,nmat,nbcomm,necrci,itmax,toler,&
     &alpham,dgamma,dalpha,iret)
            integer :: nmat
            real(kind=8) :: coeft(nmat)
            integer :: ifa
            integer :: nbcomm(nmat,3)
            character(len=16) :: necrci
            integer :: itmax
            real(kind=8) :: toler
            real(kind=8) :: alpham
            real(kind=8) :: dgamma
            real(kind=8) :: dalpha
            integer :: iret
          end subroutine lcmmec
        end interface
