        interface
          subroutine xfovol(elrefp,ndim,coorse,igeom,he,ddlh,ddlc,nfe,&
     &nnop,jlsn,jlst,iforc,itemps,ivectu,fonc,fono)
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            real(kind=8) :: coorse(*)
            integer :: igeom
            real(kind=8) :: he
            integer :: ddlh
            integer :: ddlc
            integer :: nfe
            integer :: jlsn
            integer :: jlst
            integer :: iforc
            integer :: itemps
            integer :: ivectu
            logical :: fonc
            logical :: fono
          end subroutine xfovol
        end interface
