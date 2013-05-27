        interface
          subroutine dstniw(qsi,eta,carat3,dci,bca,an,am,wst,wmest)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: carat3(*)
            real(kind=8) :: dci(2,2)
            real(kind=8) :: bca(2,3)
            real(kind=8) :: an(3,9)
            real(kind=8) :: am(3,6)
            real(kind=8) :: wst(9)
            real(kind=8) :: wmest(6)
          end subroutine dstniw
        end interface
