        interface
          subroutine amppr(amat,nb1,nb2,bmat,n1,n2,i,j)
            integer :: n2
            integer :: n1
            integer :: nb2
            integer :: nb1
            real(kind=8) :: amat(nb1,nb2)
            real(kind=8) :: bmat(n1,n2)
            integer :: i
            integer :: j
          end subroutine amppr
        end interface
