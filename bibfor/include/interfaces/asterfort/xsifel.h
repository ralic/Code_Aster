        interface
          subroutine xsifel(elrefp,ndim,coorse,igeom,jheavt,ise,nfh,&
     &ddlc,ddlm,nfe,rho,puls,lmoda,basloc,nnop,idepl,lsn,lst,idecpg,&
     &igthet,fno,nfiss,jfisno)
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
            real(kind=8) :: rho
            real(kind=8) :: puls
            logical :: lmoda
            real(kind=8) :: basloc(3*ndim*nnop)
            integer :: idepl
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            integer :: idecpg
            integer :: igthet
            real(kind=8) :: fno(ndim*nnop)
            integer :: jfisno
          end subroutine xsifel
        end interface
