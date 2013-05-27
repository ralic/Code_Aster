        interface
          subroutine vpzqrs(n,m,hh,ih,loc,valpi,valpr,zvps,iz,b,ib,u,v&
     &,acc,ifail)
            integer :: ib
            integer :: iz
            integer :: ih
            integer :: m
            integer :: n
            real(kind=8) :: hh(ih,n)
            logical :: loc(n)
            real(kind=8) :: valpi(n)
            real(kind=8) :: valpr(n)
            real(kind=8) :: zvps(iz,m)
            real(kind=8) :: b(ib,n)
            real(kind=8) :: u(n)
            real(kind=8) :: v(n)
            real(kind=8) :: acc
            integer :: ifail
          end subroutine vpzqrs
        end interface
