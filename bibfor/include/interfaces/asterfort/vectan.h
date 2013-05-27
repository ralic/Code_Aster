        interface
          subroutine vectan(nb1,nb2,xi,xr,vecta,vectn,vectpt)
            integer :: nb1
            integer :: nb2
            real(kind=8) :: xi(3,*)
            real(kind=8) :: xr(*)
            real(kind=8) :: vecta(9,2,3)
            real(kind=8) :: vectn(9,3)
            real(kind=8) :: vectpt(9,2,3)
          end subroutine vectan
        end interface
