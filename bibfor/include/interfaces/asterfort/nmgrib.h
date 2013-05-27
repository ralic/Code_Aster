        interface
          subroutine nmgrib(nno,geom,dff,dir11,lexc,vecn,b,jac,p)
            integer :: nno
            real(kind=8) :: geom(3,nno)
            real(kind=8) :: dff(2,nno)
            real(kind=8) :: dir11(3)
            logical :: lexc
            real(kind=8) :: vecn(3)
            real(kind=8) :: b(6,nno)
            real(kind=8) :: jac
            real(kind=8) :: p(3,6)
          end subroutine nmgrib
        end interface
