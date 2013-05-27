        interface
          subroutine dxhmft(dmf,jacob,hmft2)
            real(kind=8) :: dmf(3,3)
            real(kind=8) :: jacob(*)
            real(kind=8) :: hmft2(2,6)
          end subroutine dxhmft
        end interface
