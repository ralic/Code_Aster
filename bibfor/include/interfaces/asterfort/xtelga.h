        interface
          subroutine xtelga(ndim,elrefp,nnop,igeom,tempno,lonch,cnset,&
     &jpintt,lsn,lst,basloc,heavt,nfh,nfe,temppg)
            integer :: nfe
            integer :: nfh
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            integer :: igeom
            real(kind=8) :: tempno(nnop*(1+nfh+nfe))
            integer :: lonch(10)
            integer :: cnset(128)
            integer :: jpintt
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: basloc(*)
            integer :: heavt(36)
            real(kind=8) :: temppg(*)
          end subroutine xtelga
        end interface
