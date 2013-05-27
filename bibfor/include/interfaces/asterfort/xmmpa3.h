        interface
          subroutine xmmpa3(ndim,nno,nnos,nnol,pla,ffc,ffp,jac,nfh,nd,&
     &cpenco,singu,rr,ddls,ddlm,jfisno,nfiss,ifiss,jheafa,ncomph,ifa,&
     &mmat)
            integer :: ndim
            integer :: nno
            integer :: nnos
            integer :: nnol
            integer :: pla(27)
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            real(kind=8) :: jac
            integer :: nfh
            real(kind=8) :: nd(3)
            real(kind=8) :: cpenco
            integer :: singu
            real(kind=8) :: rr
            integer :: ddls
            integer :: ddlm
            integer :: jfisno
            integer :: nfiss
            integer :: ifiss
            integer :: jheafa
            integer :: ncomph
            integer :: ifa
            real(kind=8) :: mmat(216,216)
          end subroutine xmmpa3
        end interface
