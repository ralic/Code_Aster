        interface
          subroutine dfdm2j(nno,ipg,idfde,coor,jac)
            integer :: nno
            integer :: ipg
            integer :: idfde
            real(kind=8) :: coor(1)
            real(kind=8) :: jac
          end subroutine dfdm2j
        end interface
