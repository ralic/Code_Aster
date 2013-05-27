        interface
          subroutine xmmab5(ndim,nnol,pla,ffc,jac,coeffr,seuil,tau1,&
     &tau2,mu,ik,lact,mmat)
            integer :: ndim
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: jac
            real(kind=8) :: coeffr
            real(kind=8) :: seuil
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mu
            real(kind=8) :: ik(3,3)
            integer :: lact(8)
            real(kind=8) :: mmat(216,216)
          end subroutine xmmab5
        end interface
