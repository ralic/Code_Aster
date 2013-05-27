        interface
          subroutine mmcaln(ndim,tau1,tau2,norm,mprojn,mprojt)
            integer :: ndim
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: norm(3)
            real(kind=8) :: mprojn(3,3)
            real(kind=8) :: mprojt(3,3)
          end subroutine mmcaln
        end interface
