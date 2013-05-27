        interface
          subroutine xrige2(elrefp,elrese,ndim,coorse,igeom,he,ddlh,&
     &ddlc,ddlm,nfe,basloc,nnop,npg,lsn,lst,sig,matuu)
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            character(len=8) :: elrese
            real(kind=8) :: coorse(*)
            integer :: igeom
            real(kind=8) :: he
            integer :: ddlh
            integer :: ddlc
            integer :: ddlm
            integer :: nfe
            real(kind=8) :: basloc(6*nnop)
            integer :: npg
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: sig(48)
            real(kind=8) :: matuu(*)
          end subroutine xrige2
        end interface
