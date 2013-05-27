        interface
          subroutine projtq(nbcnx,xyzma,icnx,x3dp,itria,xbar,iproj)
            integer :: nbcnx
            real(kind=8) :: xyzma(3,*)
            integer :: icnx
            real(kind=8) :: x3dp(*)
            integer :: itria
            real(kind=8) :: xbar(*)
            integer :: iproj
          end subroutine projtq
        end interface
