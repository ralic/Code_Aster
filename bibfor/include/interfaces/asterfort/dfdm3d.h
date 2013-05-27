        interface
          subroutine dfdm3d(nno,ipg,ipoids,idfde,coor,dfdx,dfdy,dfdz,&
     &jac)
            integer :: nno
            integer :: ipg
            integer :: ipoids
            integer :: idfde
            real(kind=8) :: coor(1)
            real(kind=8) :: dfdx(1)
            real(kind=8) :: dfdy(1)
            real(kind=8) :: dfdz(1)
            real(kind=8) :: jac
          end subroutine dfdm3d
        end interface
