        interface
          subroutine xmvep2(ndim,nno,nnos,nnol,pla,ffc,ffp,reac,jac,&
     &nfh,saut,singu,nd,rr,cpenco,ddls,ddlm,jfisno,nfiss,ifiss,jheafa,&
     &ncomph,ifa,vtmp)
            integer :: ndim
            integer :: nno
            integer :: nnos
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            real(kind=8) :: reac
            real(kind=8) :: jac
            integer :: nfh
            real(kind=8) :: saut(3)
            integer :: singu
            real(kind=8) :: nd(3)
            real(kind=8) :: rr
            real(kind=8) :: cpenco
            integer :: ddls
            integer :: ddlm
            integer :: jfisno
            integer :: nfiss
            integer :: ifiss
            integer :: jheafa
            integer :: ncomph
            integer :: ifa
            real(kind=8) :: vtmp(400)
          end subroutine xmvep2
        end interface
