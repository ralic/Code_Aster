        interface
          subroutine vff2dn(ndim,nno,ipg,ipoids,idfde,coor,nx,ny,jac)
            integer :: ndim
            integer :: nno
            integer :: ipg
            integer :: ipoids
            integer :: idfde
            real(kind=8) :: coor(1)
            real(kind=8) :: nx
            real(kind=8) :: ny
            real(kind=8) :: jac
          end subroutine vff2dn
        end interface
