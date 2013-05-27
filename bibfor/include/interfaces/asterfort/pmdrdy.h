        interface
          subroutine pmdrdy(dsidep,coef,cimpo,valimp,y,sigp,r,drdy)
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: coef
            real(kind=8) :: cimpo(6,12)
            real(kind=8) :: valimp(6)
            real(kind=8) :: y(12)
            real(kind=8) :: sigp(6)
            real(kind=8) :: r(12)
            real(kind=8) :: drdy(12,12)
          end subroutine pmdrdy
        end interface
