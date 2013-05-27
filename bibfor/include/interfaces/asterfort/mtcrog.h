        interface
          subroutine mtcrog(a,b,nmax,n,nbsc,c,wks,ier)
            integer :: nbsc
            integer :: n
            integer :: nmax
            real(kind=8) :: a(nmax,n)
            real(kind=8) :: b(nmax,nbsc)
            real(kind=8) :: c(nmax,nbsc)
            real(kind=8) :: wks(nmax)
            integer :: ier
          end subroutine mtcrog
        end interface
