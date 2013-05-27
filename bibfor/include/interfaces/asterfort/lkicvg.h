        interface
          subroutine lkicvg(nr,itmax,toler,iter,r,nvi,vinf,dy,irtet)
            integer :: nvi
            integer :: nr
            integer :: itmax
            real(kind=8) :: toler
            integer :: iter
            real(kind=8) :: r(nr)
            real(kind=8) :: vinf(nvi)
            real(kind=8) :: dy(nr)
            integer :: irtet
          end subroutine lkicvg
        end interface
