        interface
          subroutine mpglcp(typecp,nbnolo,coordo,alpha,beta,gamma,pgl,&
     &iret)
            character(len=1) :: typecp
            integer :: nbnolo
            real(kind=8) :: coordo(*)
            real(kind=8) :: alpha
            real(kind=8) :: beta
            real(kind=8) :: gamma
            real(kind=8) :: pgl(3,3)
            integer :: iret
          end subroutine mpglcp
        end interface
