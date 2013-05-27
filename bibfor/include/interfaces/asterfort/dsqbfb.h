        interface
          subroutine dsqbfb(qsi,eta,jacob,bfb)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: jacob(*)
            real(kind=8) :: bfb(3,12)
          end subroutine dsqbfb
        end interface
