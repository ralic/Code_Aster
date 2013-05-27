        interface
          subroutine dkqtxy(qsi,eta,hft2,depf,codi,lcot,vt)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: hft2(2,6)
            real(kind=8) :: depf(12)
            real(kind=8) :: codi(*)
            real(kind=8) :: lcot(*)
            real(kind=8) :: vt(2)
          end subroutine dkqtxy
        end interface
