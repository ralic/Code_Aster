        interface
          subroutine epsimc(option,xyz,nno,npg,ndim,nbsig,ni,eps)
            character(len=16) :: option
            real(kind=8) :: xyz(1)
            integer :: nno
            integer :: npg
            integer :: ndim
            integer :: nbsig
            real(kind=8) :: ni(1)
            real(kind=8) :: eps(1)
          end subroutine epsimc
        end interface
