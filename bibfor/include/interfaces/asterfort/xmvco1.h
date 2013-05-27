        interface
          subroutine xmvco1(ndim,nno,nnol,sigma,pla,lact,dtang,nfh,&
     &ddls,jac,ffc,ffp,singu,rr,cstaco,nd,tau1,tau2,vtmp)
            integer :: ndim
            integer :: nno
            integer :: nnol
            real(kind=8) :: sigma(6)
            integer :: pla(27)
            integer :: lact(8)
            real(kind=8) :: dtang(3)
            integer :: nfh
            integer :: ddls
            real(kind=8) :: jac
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            integer :: singu
            real(kind=8) :: rr
            real(kind=8) :: cstaco
            real(kind=8) :: nd(3)
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: vtmp(400)
          end subroutine xmvco1
        end interface
