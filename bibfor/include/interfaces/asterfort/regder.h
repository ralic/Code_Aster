        interface
          subroutine regder(dimdef,dimcon,ndim,regula,dsde2g,drde)
            integer :: ndim
            integer :: dimcon
            integer :: dimdef
            integer :: regula(6)
            real(kind=8) :: dsde2g(ndim*ndim*ndim,ndim*ndim*ndim)
            real(kind=8) :: drde(dimcon,dimdef)
          end subroutine regder
        end interface
