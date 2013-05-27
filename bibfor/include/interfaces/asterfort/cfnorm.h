        interface
          subroutine cfnorm(ndim,tau1,tau2,norm,noor)
            integer :: ndim
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: norm(3)
            real(kind=8) :: noor
          end subroutine cfnorm
        end interface
