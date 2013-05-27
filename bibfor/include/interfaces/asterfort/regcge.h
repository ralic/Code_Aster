        interface
          subroutine regcge(dimdef,dimcon,regula,ndim,defgep,sigp,r)
            integer :: ndim
            integer :: dimcon
            integer :: dimdef
            integer :: regula(6)
            real(kind=8) :: defgep(dimdef)
            real(kind=8) :: sigp(ndim*ndim*ndim)
            real(kind=8) :: r(dimcon)
          end subroutine regcge
        end interface
