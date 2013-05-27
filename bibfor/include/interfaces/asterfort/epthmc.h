        interface
          subroutine epthmc(fami,nno,ndim,nbsig,npg,ni,xyz,repere,&
     &instan,mater,option,epsith)
            character(*) :: fami
            integer :: nno
            integer :: ndim
            integer :: nbsig
            integer :: npg
            real(kind=8) :: ni(1)
            real(kind=8) :: xyz(*)
            real(kind=8) :: repere(7)
            real(kind=8) :: instan
            integer :: mater
            character(len=16) :: option
            real(kind=8) :: epsith(1)
          end subroutine epthmc
        end interface
