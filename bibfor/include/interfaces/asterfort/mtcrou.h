        interface
          subroutine mtcrou(a,b,nmax,n,nbscmb,l,d)
            integer :: nbscmb
            integer :: n
            integer :: nmax
            real(kind=8) :: a(nmax,nmax)
            real(kind=8) :: b(nmax,nbscmb)
            real(kind=8) :: l(n,n)
            real(kind=8) :: d(n)
          end subroutine mtcrou
        end interface
