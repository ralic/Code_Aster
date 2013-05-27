        interface
          subroutine xbsir(ndim,nnop,nfh,nfe,ddlc,ddlm,igeom,compor,&
     &jpintt,cnset,heavt,lonch,basloc,sigref,nbsig,idepl,lsn,lst,ivectu,&
     &jpmilt,nfiss,jfisno)
            integer :: nfiss
            integer :: nnop
            integer :: ndim
            integer :: nfh
            integer :: nfe
            integer :: ddlc
            integer :: ddlm
            integer :: igeom
            character(len=16) :: compor(4)
            integer :: jpintt
            integer :: cnset(128)
            integer :: heavt(*)
            integer :: lonch(10)
            real(kind=8) :: basloc(*)
            real(kind=8) :: sigref
            integer :: nbsig
            integer :: idepl
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            integer :: ivectu
            integer :: jpmilt
            integer :: jfisno
          end subroutine xbsir
        end interface
