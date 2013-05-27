        interface
          subroutine dkqlxy(qsi,eta,hlt2,depf,codi,lcot,lambda)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: hlt2(4,6)
            real(kind=8) :: depf(12)
            real(kind=8) :: codi(*)
            real(kind=8) :: lcot(*)
            real(kind=8) :: lambda(4)
          end subroutine dkqlxy
        end interface
