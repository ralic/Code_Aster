        interface
          subroutine projsg(x3dca,x3d1,x3d2,normal,x3dp,xbar,iproj,&
     &excent)
            real(kind=8) :: x3dca(*)
            real(kind=8) :: x3d1(*)
            real(kind=8) :: x3d2(*)
            real(kind=8) :: normal(*)
            real(kind=8) :: x3dp(*)
            real(kind=8) :: xbar(*)
            integer :: iproj
            real(kind=8) :: excent
          end subroutine projsg
        end interface
