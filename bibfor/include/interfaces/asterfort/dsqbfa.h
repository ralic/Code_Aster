        interface
          subroutine dsqbfa(qsi,eta,jacob,caraq4,bfa)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: jacob(*)
            real(kind=8) :: caraq4(*)
            real(kind=8) :: bfa(3,4)
          end subroutine dsqbfa
        end interface
