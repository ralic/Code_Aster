        interface
          subroutine dilder(interp,dimdef,dimcon,ndim,regula,rpena,&
     &dsde2g,drde)
            integer :: ndim
            integer :: dimcon
            integer :: dimdef
            character(len=2) :: interp
            integer :: regula(6)
            real(kind=8) :: rpena
            real(kind=8) :: dsde2g(ndim,ndim)
            real(kind=8) :: drde(dimcon,dimdef)
          end subroutine dilder
        end interface
