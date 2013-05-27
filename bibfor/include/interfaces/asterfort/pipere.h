        interface
          subroutine pipere(npg,a,tau,nsol,eta)
            integer :: npg
            real(kind=8) :: a(0:1,npg)
            real(kind=8) :: tau
            integer :: nsol
            real(kind=8) :: eta(2)
          end subroutine pipere
        end interface
