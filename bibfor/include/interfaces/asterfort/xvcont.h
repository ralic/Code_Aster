        interface
          subroutine xvcont(algocr,cohes,coefcp,coefcr,ddlm,ddls,ffc,&
     &ffp,idepl,idepm,ifa,ifiss,imate,indco,ipgf,jac,jfisno,jheafa,lact,&
     &ncomph,nd,nddl,ndim,nfh,nfiss,nno,nnol,nnos,nvit,pla,rela,reac,rr,&
     &singu,tau1,tau2,vtmp)
            integer :: algocr
            real(kind=8) :: cohes(3)
            real(kind=8) :: coefcp
            real(kind=8) :: coefcr
            integer :: ddlm
            integer :: ddls
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            integer :: idepl
            integer :: idepm
            integer :: ifa
            integer :: ifiss
            integer :: imate
            integer :: indco
            integer :: ipgf
            real(kind=8) :: jac
            integer :: jfisno
            integer :: jheafa
            integer :: lact(8)
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
            real(kind=8) :: rela
            real(kind=8) :: reac
            real(kind=8) :: rr
            integer :: singu
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: vtmp(400)
          end subroutine xvcont
        end interface
