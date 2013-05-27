        interface
          subroutine xmmab6(ndim,nnol,pla,ffc,jac,tau1,tau2,lact,mmat)
            integer :: ndim
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: jac
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: lact(8)
            real(kind=8) :: mmat(216,216)
          end subroutine xmmab6
        end interface
