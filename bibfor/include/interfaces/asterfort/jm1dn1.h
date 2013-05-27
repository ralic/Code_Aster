        interface
          subroutine jm1dn1(indn,indc,nb1,nb2,xr,epais,ksi3s2,intsx,&
     &jm1,j1dn1)
            integer :: indn
            integer :: indc
            integer :: nb1
            integer :: nb2
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: ksi3s2
            integer :: intsx
            real(kind=8) :: jm1(3)
            real(kind=8) :: j1dn1(9,51)
          end subroutine jm1dn1
        end interface
