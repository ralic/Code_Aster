        interface
          subroutine xnmel(poum,nnop,nfh,nfe,ddlc,ddlm,igeom,typmod,&
     &option,imate,compor,lgpg,crit,jpintt,cnset,heavt,lonch,basloc,&
     &idepl,lsn,lst,sig,vi,matuu,ivectu,codret,jpmilt,nfiss,jfisno)
            integer :: nfiss
            integer :: nnop
            character(*) :: poum
            integer :: nfh
            integer :: nfe
            integer :: ddlc
            integer :: ddlm
            integer :: igeom
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(4)
            integer :: lgpg
            real(kind=8) :: crit(3)
            integer :: jpintt
            integer :: cnset(128)
            integer :: heavt(*)
            integer :: lonch(10)
            real(kind=8) :: basloc(*)
            integer :: idepl
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            real(kind=8) :: sig(*)
            real(kind=8) :: vi(*)
            real(kind=8) :: matuu(*)
            integer :: ivectu
            integer :: codret
            integer :: jpmilt
            integer :: jfisno
          end subroutine xnmel
        end interface
