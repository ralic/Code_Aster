        interface
          subroutine pj3da1(ino2,geom2,i,geom1,tetr4,cobar2,ok)
            integer :: ino2
            real(kind=8) :: geom2(*)
            integer :: i
            real(kind=8) :: geom1(*)
            integer :: tetr4(*)
            real(kind=8) :: cobar2(4)
            logical :: ok
          end subroutine pj3da1
        end interface
