        interface
          subroutine lcmmcv(yd,dy,ddy,nr,itmax,toler,iter,r,rini,epstr&
     &,irteti)
            integer :: nr
            real(kind=8) :: yd(nr)
            real(kind=8) :: dy(nr)
            real(kind=8) :: ddy(nr)
            integer :: itmax
            real(kind=8) :: toler
            integer :: iter
            real(kind=8) :: r(nr)
            real(kind=8) :: rini(nr)
            real(kind=8) :: epstr(6)
            integer :: irteti
          end subroutine lcmmcv
        end interface
