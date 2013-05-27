        interface
          subroutine equdil(imate,option,compor,regula,dimdef,dimcon,&
     &defgep,interp,ndim,contp,rpena,r,drde)
            integer :: ndim
            integer :: dimcon
            integer :: dimdef
            integer :: imate
            character(len=16) :: option
            character(len=16) :: compor(*)
            integer :: regula(6)
            real(kind=8) :: defgep(dimdef)
            character(len=2) :: interp
            real(kind=8) :: contp(dimcon)
            real(kind=8) :: rpena
            real(kind=8) :: r(dimcon)
            real(kind=8) :: drde(dimcon,dimdef)
          end subroutine equdil
        end interface
