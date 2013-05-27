        interface
          subroutine dsqcis(qsi,eta,caraq4,hmft2,hft2,bcm,bcb,bca)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: caraq4(*)
            real(kind=8) :: hmft2(2,6)
            real(kind=8) :: hft2(2,6)
            real(kind=8) :: bcm(2,8)
            real(kind=8) :: bcb(2,12)
            real(kind=8) :: bca(2,4)
          end subroutine dsqcis
        end interface
