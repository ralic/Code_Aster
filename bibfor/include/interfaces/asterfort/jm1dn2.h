        interface
          subroutine jm1dn2(indn,indc,nb1,nb2,xr,epais,ksi3s2,intsx,&
     &vecnph,jm1,j1dn2)
            integer :: indn
            integer :: indc
            integer :: nb1
            integer :: nb2
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: ksi3s2
            integer :: intsx
            real(kind=8) :: vecnph(9,3)
            real(kind=8) :: jm1(3)
            real(kind=8) :: j1dn2(9,51)
          end subroutine jm1dn2
        end interface
