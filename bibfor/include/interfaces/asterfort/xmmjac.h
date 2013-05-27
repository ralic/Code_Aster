        interface
          subroutine xmmjac(alias,geom,dff,jac)
            character(len=8) :: alias
            real(kind=8) :: geom(9)
            real(kind=8) :: dff(3,9)
            real(kind=8) :: jac
          end subroutine xmmjac
        end interface
