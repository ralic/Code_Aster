        interface
          subroutine jm1dn3(nb2,xr,epais,ksi3s2,intsn,jm1,j1dn3)
            integer :: nb2
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: ksi3s2
            integer :: intsn
            real(kind=8) :: jm1(3)
            real(kind=8) :: j1dn3(9,27)
          end subroutine jm1dn3
        end interface
