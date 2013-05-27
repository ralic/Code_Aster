        interface
          subroutine xmmbp3(ndim,nno,nnos,nnol,pla,ffc,ffp,jac,knp,nfh&
     &,seuil,tau1,tau2,mu,singu,rr,lact,ddls,ddlm,mmat)
            integer :: ndim
            integer :: nno
            integer :: nnos
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            real(kind=8) :: jac
            real(kind=8) :: knp(3,3)
            integer :: nfh
            real(kind=8) :: seuil
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: mu
            integer :: singu
            real(kind=8) :: rr
            integer :: lact(8)
            integer :: ddls
            integer :: ddlm
            real(kind=8) :: mmat(216,216)
          end subroutine xmmbp3
        end interface
