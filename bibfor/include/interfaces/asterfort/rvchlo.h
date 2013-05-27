        interface
          subroutine rvchlo(epsi,ssch19,nbcp,nbco,nbsp,nbcm,nbsm,m,f,n&
     &,r,valcp)
            real(kind=8) :: epsi
            character(len=19) :: ssch19
            integer :: nbcp
            integer :: nbco
            integer :: nbsp
            integer :: nbcm
            integer :: nbsm
            integer :: m
            integer :: f(*)
            integer :: n
            real(kind=8) :: r(*)
            real(kind=8) :: valcp(*)
          end subroutine rvchlo
        end interface
