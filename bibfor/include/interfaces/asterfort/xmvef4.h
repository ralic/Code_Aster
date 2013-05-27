        interface
          subroutine xmvef4(ndim,nnol,pla,ffc,reac12,jac,tau1,tau2,&
     &lact,vtmp)
            integer :: ndim
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: reac12(3)
            real(kind=8) :: jac
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: lact(8)
            real(kind=8) :: vtmp(400)
          end subroutine xmvef4
        end interface
