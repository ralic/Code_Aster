        interface
          subroutine coefft(cothe,coeff,dcothe,dcoeff,x,dtime,coeft,&
     &nmat,coel)
            integer :: nmat
            real(kind=8) :: cothe(nmat)
            real(kind=8) :: coeff(nmat)
            real(kind=8) :: dcothe(nmat)
            real(kind=8) :: dcoeff(nmat)
            real(kind=8) :: x
            real(kind=8) :: dtime
            real(kind=8) :: coeft(nmat)
            real(kind=8) :: coel(nmat)
          end subroutine coefft
        end interface
