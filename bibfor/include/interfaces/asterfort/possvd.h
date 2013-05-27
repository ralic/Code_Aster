        interface
          subroutine possvd(nm,m,n,w,matu,u,matv,v,eps,rg,rv1)
            integer :: n
            integer :: nm
            integer :: m
            real(kind=8) :: w(n)
            logical :: matu
            real(kind=8) :: u(nm,n)
            logical :: matv
            real(kind=8) :: v(nm,n)
            real(kind=8) :: eps
            integer :: rg
            real(kind=8) :: rv1(n)
          end subroutine possvd
        end interface
