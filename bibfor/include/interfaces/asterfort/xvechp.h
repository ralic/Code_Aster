        interface
          subroutine xvechp(ndim,elrefp,nnop,igeom,itemp,itps,ihechp,&
     &jptint,jaint,jcface,jlonch,jlst,jbasec,nfh,nfe,fonree,ivectt)
            integer :: nfe
            integer :: nfh
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            integer :: igeom
            integer :: itemp
            integer :: itps
            integer :: ihechp
            integer :: jptint
            integer :: jaint
            integer :: jcface
            integer :: jlonch
            integer :: jlst
            integer :: jbasec
            character(len=4) :: fonree
            integer :: ivectt
          end subroutine xvechp
        end interface
