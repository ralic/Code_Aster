        interface
          subroutine irrcvg(dy,ddy,nr,nmat,mater,itmax,toler,iter,r,&
     &rini,irteti)
            integer :: nmat
            integer :: nr
            real(kind=8) :: dy(nr)
            real(kind=8) :: ddy(nr)
            real(kind=8) :: mater(nmat,2)
            integer :: itmax
            real(kind=8) :: toler
            integer :: iter
            real(kind=8) :: r(nr)
            real(kind=8) :: rini(nr)
            integer :: irteti
          end subroutine irrcvg
        end interface
