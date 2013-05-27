        interface
          subroutine xrechp(ndim,elrefp,nnop,igeom,itps,ihechp,jptint,&
     &jaint,jcface,jlonch,jlst,jbasec,nfh,nfe,fonree,imattt)
            integer :: nfe
            integer :: nfh
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            integer :: igeom
            integer :: itps
            integer :: ihechp
            integer :: jptint
            integer :: jaint
            integer :: jcface
            integer :: jlonch
            integer :: jlst
            integer :: jbasec
            character(len=4) :: fonree
            integer :: imattt
          end subroutine xrechp
        end interface
