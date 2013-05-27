        interface
          subroutine dfplgl(nmnbn,nmplas,nmdpla,bend,dfpl)
            real(kind=8) :: nmnbn(6)
            real(kind=8) :: nmplas(2,3)
            real(kind=8) :: nmdpla(2,2)
            integer :: bend
            real(kind=8) :: dfpl(*)
          end subroutine dfplgl
        end interface
