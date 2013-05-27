        interface
          subroutine rot3d(x,sina,cosa,sinb,cosb,sing,cosg,y)
            real(kind=8) :: x(*)
            real(kind=8) :: sina
            real(kind=8) :: cosa
            real(kind=8) :: sinb
            real(kind=8) :: cosb
            real(kind=8) :: sing
            real(kind=8) :: cosg
            real(kind=8) :: y(*)
          end subroutine rot3d
        end interface
