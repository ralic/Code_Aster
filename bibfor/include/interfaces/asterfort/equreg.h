        interface
          subroutine equreg(imate,option,compor,regula,dimdef,dimcon,&
     &defgep,ndim,contp,r,drde)
            integer :: ndim
            integer :: dimcon
            integer :: dimdef
            integer :: imate
            character(len=16) :: option
            character(len=16) :: compor(*)
            integer :: regula(6)
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: contp(dimcon)
            real(kind=8) :: r(dimcon)
            real(kind=8) :: drde(dimcon,dimdef)
          end subroutine equreg
        end interface
