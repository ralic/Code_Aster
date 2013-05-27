        interface
          subroutine calsvd(nm,m,n,a,w,matu,u,matv,v,ierr)
            integer :: n
            integer :: m
            integer :: nm
            real(kind=8) :: a(nm,n)
            real(kind=8) :: w(n)
            logical :: matu
            real(kind=8) :: u(nm,m)
            logical :: matv
            real(kind=8) :: v(nm,n)
            integer :: ierr
          end subroutine calsvd
        end interface
