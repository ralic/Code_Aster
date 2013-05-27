        interface
          subroutine dilcge(interp,dimdef,dimcon,regula,ndim,defgep,&
     &sigp,rpena,r)
            integer :: ndim
            integer :: dimcon
            integer :: dimdef
            character(len=2) :: interp
            integer :: regula(6)
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: sigp(ndim)
            real(kind=8) :: rpena
            real(kind=8) :: r(dimcon)
          end subroutine dilcge
        end interface
