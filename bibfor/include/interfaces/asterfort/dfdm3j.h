        interface
          subroutine dfdm3j(nno,ipg,idfde,coor,jac)
            integer :: nno
            integer :: ipg
            integer :: idfde
            real(kind=8) :: coor(1)
            real(kind=8) :: jac
          end subroutine dfdm3j
        end interface
