        interface
          subroutine dsqdi2(xyzl,df,dci,dmf,dfc,dmc,an,am)
            real(kind=8) :: xyzl(3,*)
            real(kind=8) :: df(3,3)
            real(kind=8) :: dci(2,2)
            real(kind=8) :: dmf(3,3)
            real(kind=8) :: dfc(3,2)
            real(kind=8) :: dmc(3,2)
            real(kind=8) :: an(4,12)
            real(kind=8) :: am(4,8)
          end subroutine dsqdi2
        end interface
