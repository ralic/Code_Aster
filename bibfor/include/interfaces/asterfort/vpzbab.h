        interface
          subroutine vpzbab(n,ibas,lhi,m,d,zvps,iz)
            integer :: iz
            integer :: m
            integer :: n
            integer :: ibas
            integer :: lhi
            real(kind=8) :: d(n)
            real(kind=8) :: zvps(iz,m)
          end subroutine vpzbab
        end interface
