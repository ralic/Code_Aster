        interface
          subroutine xmmco2(ndim,nno,nnos,nnol,ddls,ddlm,dsidep,p,r,&
     &nfh,jac,ffp,ffc,pla,singu,nfiss,jheafa,jfisno,ifa,ncomph,ifiss,rr,&
     &mmat)
            integer :: ndim
            integer :: nno
            integer :: nnos
            integer :: nnol
            integer :: ddls
            integer :: ddlm
            real(kind=8) :: dsidep(6,6)
            real(kind=8) :: p(3,3)
            real(kind=8) :: r
            integer :: nfh
            real(kind=8) :: jac
            real(kind=8) :: ffp(27)
            real(kind=8) :: ffc(8)
            integer :: pla(27)
            integer :: singu
            integer :: nfiss
            integer :: jheafa
            integer :: jfisno
            integer :: ifa
            integer :: ncomph
            integer :: ifiss
            real(kind=8) :: rr
            real(kind=8) :: mmat(216,216)
          end subroutine xmmco2
        end interface
