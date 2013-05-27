        interface
          subroutine dsqlxy(qsi,eta,hlt2,an,depf,codi,lambda)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: hlt2(4,6)
            real(kind=8) :: an(4,12)
            real(kind=8) :: depf(12)
            real(kind=8) :: codi(*)
            real(kind=8) :: lambda(4)
          end subroutine dsqlxy
        end interface
