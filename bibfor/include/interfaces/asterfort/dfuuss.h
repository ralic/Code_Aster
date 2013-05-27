        interface
          subroutine dfuuss(nmnbn,nmplas,nmdpla,nmprox,bend,dfuu)
            real(kind=8) :: nmnbn(6)
            real(kind=8) :: nmplas(2,3)
            real(kind=8) :: nmdpla(2,2)
            integer :: nmprox(2)
            integer :: bend
            real(kind=8) :: dfuu(*)
          end subroutine dfuuss
        end interface
