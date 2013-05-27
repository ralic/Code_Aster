        interface
          subroutine xmvco2(ndim,nno,nnol,nnos,lamb,am,delta,pla,lact,&
     &nfh,ddls,ddlm,nfiss,ifiss,jheafa,ifa,ncomph,jfisno,jac,ffc,ffp,&
     &singu,r,rr,vtmp,p)
            integer :: ndim
            integer :: nno
            integer :: nnol
            integer :: nnos
            real(kind=8) :: lamb(3)
            real(kind=8) :: am(3)
            real(kind=8) :: delta(6)
            integer :: pla(27)
            integer :: lact(8)
            integer :: nfh
            integer :: ddls
            integer :: ddlm
            integer :: nfiss
            integer :: ifiss
            integer :: jheafa
            integer :: ifa
            integer :: ncomph
            integer :: jfisno
            real(kind=8) :: jac
            real(kind=8) :: ffc(8)
            real(kind=8) :: ffp(27)
            integer :: singu
            real(kind=8) :: r
            real(kind=8) :: rr
            real(kind=8) :: vtmp(400)
            real(kind=8) :: p(3,3)
          end subroutine xmvco2
        end interface
