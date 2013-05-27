        interface
          subroutine intega(npgf,jac,poidsf,vectx,vecty,vectz,mat11,&
     &mat22,mat33,mat12,mat13,mat23,nx,ny,nz,inte)
            integer :: npgf
            real(kind=8) :: jac(9)
            real(kind=8) :: poidsf(9)
            real(kind=8) :: vectx(9)
            real(kind=8) :: vecty(9)
            real(kind=8) :: vectz(9)
            real(kind=8) :: mat11(9)
            real(kind=8) :: mat22(9)
            real(kind=8) :: mat33(9)
            real(kind=8) :: mat12(9)
            real(kind=8) :: mat13(9)
            real(kind=8) :: mat23(9)
            real(kind=8) :: nx(9)
            real(kind=8) :: ny(9)
            real(kind=8) :: nz(9)
            real(kind=8) :: inte
          end subroutine intega
        end interface
