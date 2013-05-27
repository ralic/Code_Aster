        interface
          subroutine xmasel(nnop,ddlh,nfe,ddlc,igeom,imate,pintt,cnset&
     &,heavt,lonch,basloc,lsn,lst,matuu)
            integer :: nnop
            integer :: ddlh
            integer :: nfe
            integer :: ddlc
            integer :: igeom
            integer :: imate
            real(kind=8) :: pintt(33)
            integer :: cnset(128)
            integer :: heavt(36)
            integer :: lonch(10)
            real(kind=8) :: basloc(*)
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: matuu(*)
          end subroutine xmasel
        end interface
