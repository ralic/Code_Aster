        interface
          subroutine xrigel(nnop,ddlh,nfe,ddlc,ddlm,igeom,jpintt,cnset&
     &,heavt,lonch,basloc,lsn,lst,sig,matuu,jpmilt)
            integer :: nnop
            integer :: ddlh
            integer :: nfe
            integer :: ddlc
            integer :: ddlm
            integer :: igeom
            integer :: jpintt
            integer :: cnset(128)
            integer :: heavt(36)
            integer :: lonch(10)
            real(kind=8) :: basloc(*)
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: sig(*)
            real(kind=8) :: matuu(*)
            integer :: jpmilt
          end subroutine xrigel
        end interface
