        interface
          subroutine xvetth(ndim,elrefp,nnop,imate,itps,igeom,temper,&
     &lonch,cnset,jpintt,lsn,lst,basloc,heavt,nfh,nfe,vectt)
            integer :: nfe
            integer :: nfh
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            integer :: imate
            integer :: itps
            integer :: igeom
            real(kind=8) :: temper(nnop*(1+nfh+nfe))
            integer :: lonch(10)
            integer :: cnset(128)
            integer :: jpintt
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: basloc(*)
            integer :: heavt(36)
            real(kind=8) :: vectt(*)
          end subroutine xvetth
        end interface
