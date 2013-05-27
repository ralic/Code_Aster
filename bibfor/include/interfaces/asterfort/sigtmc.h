        interface
          subroutine sigtmc(fami,nno,ndim,nbsig,npg,ni,xyz,instan,&
     &mater,repere,option,sigma)
            character(*) :: fami
            integer :: nno
            integer :: ndim
            integer :: nbsig
            integer :: npg
            real(kind=8) :: ni(1)
            real(kind=8) :: xyz(1)
            real(kind=8) :: instan
            integer :: mater
            real(kind=8) :: repere(7)
            character(len=16) :: option
            real(kind=8) :: sigma(1)
          end subroutine sigtmc
        end interface
