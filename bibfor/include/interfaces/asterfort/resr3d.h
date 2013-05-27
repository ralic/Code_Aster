        interface
          subroutine resr3d(rota,coor,ff,rho,nno,npg,frx,fry,frz)
            real(kind=8) :: rota(*)
            real(kind=8) :: coor(1)
            real(kind=8) :: ff(1)
            real(kind=8) :: rho
            integer :: nno
            integer :: npg
            real(kind=8) :: frx(27)
            real(kind=8) :: fry(27)
            real(kind=8) :: frz(27)
          end subroutine resr3d
        end interface
