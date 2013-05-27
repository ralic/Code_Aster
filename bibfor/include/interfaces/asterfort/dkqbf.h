        interface
          subroutine dkqbf(qsi,eta,jacob,caraq4,bf)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: jacob(*)
            real(kind=8) :: caraq4(*)
            real(kind=8) :: bf(3,12)
          end subroutine dkqbf
        end interface
