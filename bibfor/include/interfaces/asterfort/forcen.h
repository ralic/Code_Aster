        interface
          subroutine forcen(rnormc,intsn,nb1,xi,xr,rho,epais,vomega,&
     &vecl1,xa)
            real(kind=8) :: rnormc
            integer :: intsn
            integer :: nb1
            real(kind=8) :: xi(3,*)
            real(kind=8) :: xr(*)
            real(kind=8) :: rho
            real(kind=8) :: epais
            real(kind=8) :: vomega(3)
            real(kind=8) :: vecl1(42)
            real(kind=8) :: xa(3)
          end subroutine forcen
        end interface
