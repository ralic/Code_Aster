        interface
          subroutine xmmbp5(ndim,nnol,pla,ffc,jac,coeffp,seuil,tau1,&
     &tau2,mu,lact,mmat)
            integer :: ndim
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: jac
            real(kind=8) :: coeffp
            real(kind=8) :: seuil
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mu
            integer :: lact(8)
            real(kind=8) :: mmat(216,216)
          end subroutine xmmbp5
        end interface
