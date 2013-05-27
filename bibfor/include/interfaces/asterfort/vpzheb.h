        interface
          subroutine vpzheb(kl,l,m,a,ia,intger,zvps,iz,n)
            integer :: n
            integer :: iz
            integer :: ia
            integer :: m
            integer :: kl
            integer :: l
            real(kind=8) :: a(ia,n)
            integer :: intger(n)
            real(kind=8) :: zvps(iz,m)
          end subroutine vpzheb
        end interface
