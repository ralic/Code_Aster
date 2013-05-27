        interface
          subroutine pj3da2(ino2,geom2,i,geom1,tetr4,cobary,d2,volu)
            integer :: ino2
            real(kind=8) :: geom2(*)
            integer :: i
            real(kind=8) :: geom1(*)
            integer :: tetr4(*)
            real(kind=8) :: cobary(4)
            real(kind=8) :: d2
            real(kind=8) :: volu
          end subroutine pj3da2
        end interface
