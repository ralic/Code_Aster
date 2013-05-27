        interface
          subroutine xsidep(nnop,nfh,nfe,ddlc,ddlm,igeom,typmod,imate,&
     &compor,jpintt,cnset,heavt,lonch,basloc,idepl,lsn,lst,sig,jpmilt,&
     &nfiss,jfisno)
            integer :: nfiss
            integer :: nnop
            integer :: nfh
            integer :: nfe
            integer :: ddlc
            integer :: ddlm
            integer :: igeom
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(4)
            integer :: jpintt
            integer :: cnset(128)
            integer :: heavt(*)
            integer :: lonch(10)
            real(kind=8) :: basloc(*)
            integer :: idepl
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: sig(*)
            integer :: jpmilt
            integer :: jfisno
          end subroutine xsidep
        end interface
