        interface
          subroutine gloloc(xgloba,xorig,sina,cosa,sinb,cosb,sing,cosg&
     &,xlocal)
            real(kind=8) :: xgloba(3)
            real(kind=8) :: xorig(3)
            real(kind=8) :: sina
            real(kind=8) :: cosa
            real(kind=8) :: sinb
            real(kind=8) :: cosb
            real(kind=8) :: sing
            real(kind=8) :: cosg
            real(kind=8) :: xlocal(3)
          end subroutine gloloc
        end interface
