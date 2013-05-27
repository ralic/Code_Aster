        interface
          subroutine xmvco3(sigref,depref,ndim,nno,nnol,nnos,pla,lact,&
     &nfh,ddls,ddlm,nfiss,ifiss,jheafa,ifa,ncomph,jfisno,jac,ffc,ffp,&
     &singu,rr,vtmp)
            real(kind=8) :: sigref
            real(kind=8) :: depref
            integer :: ndim
            integer :: nno
            integer :: nnol
            integer :: nnos
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
            real(kind=8) :: rr
            real(kind=8) :: vtmp(400)
          end subroutine xmvco3
        end interface
