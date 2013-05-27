        interface
          subroutine xsifle(ndim,ifa,jptint,jaint,cface,igeom,nfh,&
     &singu,nfe,ddlc,ddlm,jlst,ipres,ipref,itemps,idepl,nnop,valres,&
     &basloc,ithet,nompar,presn,option,igthet,jbasec)
            integer :: nnop
            integer :: ndim
            integer :: ifa
            integer :: jptint
            integer :: jaint
            integer :: cface(5,3)
            integer :: igeom
            integer :: nfh
            integer :: singu
            integer :: nfe
            integer :: ddlc
            integer :: ddlm
            integer :: jlst
            integer :: ipres
            integer :: ipref
            integer :: itemps
            integer :: idepl
            real(kind=8) :: valres(3)
            real(kind=8) :: basloc(9*nnop)
            integer :: ithet
            character(len=8) :: nompar(4)
            real(kind=8) :: presn(27)
            character(len=16) :: option
            integer :: igthet
            integer :: jbasec
          end subroutine xsifle
        end interface
