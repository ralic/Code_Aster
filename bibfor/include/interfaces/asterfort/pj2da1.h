        interface
          subroutine pj2da1(ino2,geom2,i,geom1,tria3,cobar2,ok)
            integer :: ino2
            real(kind=8) :: geom2(*)
            integer :: i
            real(kind=8) :: geom1(*)
            integer :: tria3(*)
            real(kind=8) :: cobar2(3)
            logical :: ok
          end subroutine pj2da1
        end interface
