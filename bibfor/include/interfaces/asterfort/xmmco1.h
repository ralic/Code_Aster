        interface
          subroutine xmmco1(ndim,nno,dsidep,pp,p,nd,nfh,ddls,jac,ffp,&
     &singu,rr,tau1,tau2,mmat)
            integer :: ndim
            integer :: nno
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: pp(3,3)
            real(kind=8) :: p(3,3)
            real(kind=8) :: nd(3)
            integer :: nfh
            integer :: ddls
            real(kind=8) :: jac
            real(kind=8) :: ffp(27)
            integer :: singu
            real(kind=8) :: rr
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mmat(216,216)
          end subroutine xmmco1
        end interface
