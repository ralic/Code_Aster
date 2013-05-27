        interface
          subroutine xmvef2(ndim,nno,nnos,ffp,jac,seuil,reac12,singu,&
     &nfh,rr,coeffp,coeffr,mu,algofr,nd,ddls,ddlm,idepl,pb,vtmp)
            integer :: ndim
            integer :: nno
            integer :: nnos
            real(kind=8) :: ffp(27)
            real(kind=8) :: jac
            real(kind=8) :: seuil
            real(kind=8) :: reac12(3)
            integer :: singu
            integer :: nfh
            real(kind=8) :: rr
            real(kind=8) :: coeffp
            real(kind=8) :: coeffr
            real(kind=8) :: mu
            integer :: algofr
            real(kind=8) :: nd(3)
            integer :: ddls
            integer :: ddlm
            integer :: idepl
            real(kind=8) :: pb(3)
            real(kind=8) :: vtmp(400)
          end subroutine xmvef2
        end interface
