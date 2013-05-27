        interface
          subroutine xmcont(algocr,coefcr,coefcp,cohes,coheo,ddlm,ddls&
     &,ffc,ffp,idepd,idepm,ifa,ifiss,jmate,indco,ipgf,jac,jfisno,jheafa,&
     &mmat,lact,ncomph,nd,nddl,ndim,nfh,nfiss,nno,nnol,nnos,nvit,pla,&
     &rela,rr,singu,tau1,tau2)
            integer :: algocr
            real(kind=8) :: coefcr
            real(kind=8) :: coefcp
            real(kind=8) :: cohes(3)
            real(kind=8) :: coheo(3)
            integer :: ddlm
            integer :: ddls
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            integer :: idepd
            integer :: idepm
            integer :: ifa
            integer :: ifiss
            integer :: jmate
            integer :: indco
            integer :: ipgf
            real(kind=8) :: jac
            integer :: jfisno
            integer :: jheafa
            real(kind=8) :: mmat(216,216)
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
            real(kind=8) :: rr
            integer :: singu
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau2(3)
          end subroutine xmcont
        end interface
