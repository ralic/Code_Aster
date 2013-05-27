        interface
          subroutine xpesro(elrefp,ndim,coorse,igeom,jheavt,jfisno,nfh&
     &,ddlc,nfe,nfiss,ise,nnop,jlsn,jlst,ivectu,fno)
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            real(kind=8) :: coorse(*)
            integer :: igeom
            integer :: jheavt
            integer :: jfisno
            integer :: nfh
            integer :: ddlc
            integer :: nfe
            integer :: nfiss
            integer :: ise
            integer :: jlsn
            integer :: jlst
            integer :: ivectu
            real(kind=8) :: fno(ndim*nnop)
          end subroutine xpesro
        end interface
