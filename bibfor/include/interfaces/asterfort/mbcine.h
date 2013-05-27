        interface
          subroutine mbcine(nno,geom,dff,alpha,beta,b,jac)
            integer :: nno
            real(kind=8) :: geom(3,nno)
            real(kind=8) :: dff(2,nno)
            real(kind=8) :: alpha
            real(kind=8) :: beta
            real(kind=8) :: b(3,3,nno)
            real(kind=8) :: jac
          end subroutine mbcine
        end interface
