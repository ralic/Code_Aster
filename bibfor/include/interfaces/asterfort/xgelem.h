        interface
          subroutine xgelem(elrefp,ndim,coorse,igeom,jheavt,ise,nfh,&
     &ddlc,ddlm,nfe,basloc,nnop,idepl,lsn,lst,igthet,fno,nfiss,jfisno)
            integer :: nfiss
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            real(kind=8) :: coorse(*)
            integer :: igeom
            integer :: jheavt
            integer :: ise
            integer :: nfh
            integer :: ddlc
            integer :: ddlm
            integer :: nfe
            real(kind=8) :: basloc(3*ndim*nnop)
            integer :: idepl
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            integer :: igthet
            real(kind=8) :: fno(ndim*nnop)
            integer :: jfisno
          end subroutine xgelem
        end interface
