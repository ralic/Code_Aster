        interface
          subroutine xmfrot(algofr,coeffr,coeffp,ddlm,ddls,ffc,ffp,&
     &idepd,idepm,indco,jac,lact,mmat,mu,nd,ndim,nfh,nfiss,nno,nnol,nnos&
     &,nvit,pla,rr,seuil,singu,tau1,tau2)
            integer :: algofr
            real(kind=8) :: coeffr
            real(kind=8) :: coeffp
            integer :: ddlm
            integer :: ddls
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            integer :: idepd
            integer :: idepm
            integer :: indco
            real(kind=8) :: jac
            integer :: lact(8)
            real(kind=8) :: mmat(216,216)
            real(kind=8) :: mu
            real(kind=8) :: nd(3)
            integer :: ndim
            integer :: nfh
            integer :: nfiss
            integer :: nno
            integer :: nnol
            integer :: nnos
            integer :: nvit
            integer :: pla(27)
            real(kind=8) :: rr
            real(kind=8) :: seuil
            integer :: singu
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine xmfrot
        end interface
