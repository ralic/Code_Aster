        interface
          subroutine sigela(typmod,ndim,e,nu,epse,sigel)
            character(len=8) :: typmod(1)
            integer :: ndim
            real(kind=8) :: e
            real(kind=8) :: nu
            real(kind=8) :: epse(6)
            real(kind=8) :: sigel(6)
          end subroutine sigela
        end interface
