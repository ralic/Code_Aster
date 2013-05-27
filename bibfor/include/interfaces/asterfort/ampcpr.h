        interface
          subroutine ampcpr(cmat,nb1,nb2,bmat,n1,n2,i,j,fac,npar,nsym)
            integer :: n2
            integer :: n1
            complex(kind=8) :: cmat(*)
            integer :: nb1
            integer :: nb2
            real(kind=8) :: bmat(n1,n2)
            integer :: i
            integer :: j
            real(kind=8) :: fac
            integer :: npar
            integer :: nsym
          end subroutine ampcpr
        end interface
