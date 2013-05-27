        interface
          subroutine cq3d2d(nno,coor3d,coteta,siteta,coor2d)
            integer :: nno
            real(kind=8) :: coor3d(*)
            real(kind=8) :: coteta
            real(kind=8) :: siteta
            real(kind=8) :: coor2d(*)
          end subroutine cq3d2d
        end interface
