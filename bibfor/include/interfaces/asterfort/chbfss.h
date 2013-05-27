        interface
          subroutine chbfss(sig,x1,x2,id,ddfdds)
            real(kind=8) :: sig(6)
            real(kind=8) :: x1(6)
            real(kind=8) :: x2(6)
            real(kind=8) :: id(6,6)
            real(kind=8) :: ddfdds(6,6)
          end subroutine chbfss
        end interface
