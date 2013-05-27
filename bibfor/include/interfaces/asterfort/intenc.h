        interface
          subroutine intenc(nbna,jac,vectx,vecty,mat11,mat22,mat12,nx,&
     &ny,inte)
            integer :: nbna
            real(kind=8) :: jac(3)
            real(kind=8) :: vectx(3)
            real(kind=8) :: vecty(3)
            real(kind=8) :: mat11(3)
            real(kind=8) :: mat22(3)
            real(kind=8) :: mat12(3)
            real(kind=8) :: nx(3)
            real(kind=8) :: ny(3)
            real(kind=8) :: inte
          end subroutine intenc
        end interface
