        interface
          subroutine xbsir2(elref,contac,ddlc,ddlm,ddls,igeom,jfisno,&
     &jlst,ivectu,singu,nddl,ndim,nfe,nfh,nfiss,nno,nnom,nnos,depref,&
     &sigref,nomte)
            character(len=8) :: elref
            integer :: contac
            integer :: ddlc
            integer :: ddlm
            integer :: ddls
            integer :: igeom
            integer :: jfisno
            integer :: jlst
            integer :: ivectu
            integer :: singu
            integer :: nddl
            integer :: ndim
            integer :: nfe
            integer :: nfh
            integer :: nfiss
            integer :: nno
            integer :: nnom
            integer :: nnos
            real(kind=8) :: depref
            real(kind=8) :: sigref
            character(len=16) :: nomte
          end subroutine xbsir2
        end interface
