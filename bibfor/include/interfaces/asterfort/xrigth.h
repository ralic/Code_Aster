        interface
          subroutine xrigth(ndim,elrefp,nnop,imate,itemps,igeom,lonch,&
     &cnset,jpintt,lsn,lst,basloc,heavt,nfh,nfe,mattt)
            integer :: nfe
            integer :: nfh
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            integer :: imate
            integer :: itemps
            integer :: igeom
            integer :: lonch(10)
            integer :: cnset(128)
            integer :: jpintt
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: basloc(*)
            integer :: heavt(36)
            real(kind=8) :: mattt(*)
          end subroutine xrigth
        end interface
