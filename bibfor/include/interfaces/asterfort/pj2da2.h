        interface
          subroutine pj2da2(ino2,geom2,i,geom1,tria3,cobary,d2,surf)
            integer :: ino2
            real(kind=8) :: geom2(*)
            integer :: i
            real(kind=8) :: geom1(*)
            integer :: tria3(*)
            real(kind=8) :: cobary(3)
            real(kind=8) :: d2
            real(kind=8) :: surf
          end subroutine pj2da2
        end interface
