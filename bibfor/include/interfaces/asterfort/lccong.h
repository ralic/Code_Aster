        interface
          subroutine lccong(nr,itmax,toler,iter,r,rini,yd,dy,irtet)
            integer :: nr
            integer :: itmax
            real(kind=8) :: toler
            integer :: iter
            real(kind=8) :: r(nr)
            real(kind=8) :: rini(*)
            real(kind=8) :: yd(*)
            real(kind=8) :: dy(*)
            integer :: irtet
          end subroutine lccong
        end interface
