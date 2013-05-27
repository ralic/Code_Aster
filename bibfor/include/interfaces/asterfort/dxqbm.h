        interface
          subroutine dxqbm(qsi,eta,jacob,bm)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: jacob(*)
            real(kind=8) :: bm(3,8)
          end subroutine dxqbm
        end interface
