        interface
          subroutine sigimc(fami,nno,ndim,nbsig,npg,ni,xyz,instan,&
     &mater,repere,epsini,sigma)
            character(len=4) :: fami
            integer :: nno
            integer :: ndim
            integer :: nbsig
            integer :: npg
            real(kind=8) :: ni(1)
            real(kind=8) :: xyz(1)
            real(kind=8) :: instan
            integer :: mater
            real(kind=8) :: repere(7)
            real(kind=8) :: epsini(1)
            real(kind=8) :: sigma(1)
          end subroutine sigimc
        end interface
