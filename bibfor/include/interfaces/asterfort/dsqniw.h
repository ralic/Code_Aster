        interface
          subroutine dsqniw(qsi,eta,caraq4,dci,bcm,bcb,bca,an,am,wsq,&
     &wmesq)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: caraq4(*)
            real(kind=8) :: dci(2,2)
            real(kind=8) :: bcm(2,8)
            real(kind=8) :: bcb(2,12)
            real(kind=8) :: bca(2,4)
            real(kind=8) :: an(4,12)
            real(kind=8) :: am(4,8)
            real(kind=8) :: wsq(12)
            real(kind=8) :: wmesq(8)
          end subroutine dsqniw
        end interface
