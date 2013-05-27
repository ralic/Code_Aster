        interface
          subroutine xmase2(elrefp,ndim,coorse,igeom,he,ddlh,ddlc,nfe,&
     &basloc,nnop,npg,imate,lsn,lst,matuu)
            integer :: nnop
            integer :: nfe
            integer :: ddlc
            integer :: ddlh
            integer :: ndim
            character(len=8) :: elrefp
            real(kind=8) :: coorse(*)
            integer :: igeom
            real(kind=8) :: he
            real(kind=8) :: basloc(6*nnop)
            integer :: npg
            integer :: imate
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: matuu(*)
          end subroutine xmase2
        end interface
