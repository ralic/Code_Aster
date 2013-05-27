        interface
          subroutine xvfrot(algofr,coeffp,coeffr,ddlm,ddls,ffc,ffp,&
     &idepl,idepm,ifa,ifiss,indco,jac,jfisno,jheafa,lact,mu,ncomph,nd,&
     &nddl,ndim,nfh,nfiss,nno,nnol,nnos,nvit,pla,reac12,rr,seuil,singu,&
     &tau1,tau2,vtmp)
            integer :: algofr
            real(kind=8) :: coeffp
            real(kind=8) :: coeffr
            integer :: ddlm
            integer :: ddls
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            integer :: idepl
            integer :: idepm
            integer :: ifa
            integer :: ifiss
            integer :: indco
            real(kind=8) :: jac
            integer :: jfisno
            integer :: jheafa
            integer :: lact(8)
            real(kind=8) :: mu
            integer :: ncomph
            real(kind=8) :: nd(3)
            integer :: nddl
            integer :: ndim
            integer :: nfh
            integer :: nfiss
            integer :: nno
            integer :: nnol
            integer :: nnos
            integer :: nvit
            integer :: pla(27)
            real(kind=8) :: reac12(3)
            real(kind=8) :: rr
            real(kind=8) :: seuil
            integer :: singu
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: vtmp(400)
          end subroutine xvfrot
        end interface
