        interface
          subroutine dktbf(qsi,eta,carat3,bf)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: carat3(*)
            real(kind=8) :: bf(3,9)
          end subroutine dktbf
        end interface
