        interface
          subroutine chbfsx(sig,x1,x2,i4,ddfdsx)
            real(kind=8) :: sig(6)
            real(kind=8) :: x1(6)
            real(kind=8) :: x2(6)
            real(kind=8) :: i4(6,6)
            real(kind=8) :: ddfdsx(6,6)
          end subroutine chbfsx
        end interface
