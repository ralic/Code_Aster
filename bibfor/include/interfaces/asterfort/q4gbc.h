        interface
          subroutine q4gbc(qsi,eta,jacob,caraq4,bc)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: jacob(*)
            real(kind=8) :: caraq4(*)
            real(kind=8) :: bc(2,12)
          end subroutine q4gbc
        end interface
