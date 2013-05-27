        interface
          subroutine xtcaln(ndim,tau1,tau2,norm,mprojt)
            integer :: ndim
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: norm(3)
            real(kind=8) :: mprojt(3,3)
          end subroutine xtcaln
        end interface
