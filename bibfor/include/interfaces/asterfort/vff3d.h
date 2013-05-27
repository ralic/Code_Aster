        interface
          subroutine vff3d(nno,poids,dfde,coor,jac)
            integer :: nno
            real(kind=8) :: poids
            real(kind=8) :: dfde(nno)
            real(kind=8) :: coor(3*nno)
            real(kind=8) :: jac
          end subroutine vff3d
        end interface
