        interface
          subroutine tranlg(nb1,nddlx,nddlet,plg,matloc,xr)
            integer :: nddlx
            integer :: nb1
            integer :: nddlet
            real(kind=8) :: plg(9,3,3)
            real(kind=8) :: matloc(nddlx,nddlx)
            real(kind=8) :: xr(*)
          end subroutine tranlg
        end interface
