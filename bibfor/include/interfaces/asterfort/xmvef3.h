        interface
          subroutine xmvef3(ndim,nnol,pla,ffc,reac12,pb,jac,seuil,tau1&
     &,tau2,lact,cstafr,mu,vtmp)
            integer :: ndim
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: reac12(3)
            real(kind=8) :: pb(3)
            real(kind=8) :: jac
            real(kind=8) :: seuil
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            integer :: lact(8)
            real(kind=8) :: cstafr
            real(kind=8) :: mu
            real(kind=8) :: vtmp(400)
          end subroutine xmvef3
        end interface
