        interface
          subroutine jquad4(xyzl,qsi,eta,jacob)
            real(kind=8) :: xyzl(3,*)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: jacob(*)
          end subroutine jquad4
        end interface
