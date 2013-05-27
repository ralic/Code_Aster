        interface
          subroutine t3gbc(xyzl,qsi,eta,bc)
            real(kind=8) :: xyzl(3,*)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: bc(2,9)
          end subroutine t3gbc
        end interface
