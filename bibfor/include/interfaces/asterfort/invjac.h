        interface
          subroutine invjac(nno,ipg,ipoids,idfde,coor,invja,jac)
            integer :: nno
            integer :: ipg
            integer :: ipoids
            integer :: idfde
            real(kind=8) :: coor(1)
            real(kind=8) :: invja(3,3)
            real(kind=8) :: jac
          end subroutine invjac
        end interface
