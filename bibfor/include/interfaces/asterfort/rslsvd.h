        interface
          subroutine rslsvd(nm,m,n,a,w,u,v,nb,b,eps,ierr,rvnm)
            integer :: nb
            integer :: n
            integer :: m
            integer :: nm
            real(kind=8) :: a(nm,n)
            real(kind=8) :: w(n)
            real(kind=8) :: u(nm,m)
            real(kind=8) :: v(nm,n)
            real(kind=8) :: b(nm,nb)
            real(kind=8) :: eps
            integer :: ierr
            real(kind=8) :: rvnm(nm)
          end subroutine rslsvd
        end interface
