        interface
          subroutine resrot(rota,coor,ff,rho,nno,npg,frx,fry)
            real(kind=8) :: rota(3)
            real(kind=8) :: coor(18)
            real(kind=8) :: ff(81)
            real(kind=8) :: rho
            integer :: nno
            integer :: npg
            real(kind=8) :: frx(9)
            real(kind=8) :: fry(9)
          end subroutine resrot
        end interface
