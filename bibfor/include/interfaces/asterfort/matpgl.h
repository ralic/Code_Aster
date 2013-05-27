        interface
          subroutine matpgl(nb1,xr,plg)
            integer :: nb1
            real(kind=8) :: xr(*)
            real(kind=8) :: plg(9,3,3)
          end subroutine matpgl
        end interface
