        interface
          subroutine burcvg(nr,itmax,toler,iter,dy,r,rini,irtet)
            integer :: nr
            integer :: itmax
            real(kind=8) :: toler
            integer :: iter
            real(kind=8) :: dy(nr)
            real(kind=8) :: r(nr)
            real(kind=8) :: rini(nr)
            integer :: irtet
          end subroutine burcvg
        end interface
