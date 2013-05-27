        interface
          subroutine xnmgr(nnop,nfh,nfe,ddlc,ddlm,igeom,instam,instap,&
     &ideplp,sigm,vip,typmod,option,imate,compor,lgpg,crit,jpintt,cnset,&
     &heavt,lonch,basloc,idepl,lsn,lst,nfiss,jfisno,sig,vi,matuu,ivectu,&
     &codret,jpmilt)
            integer :: nfiss
            integer :: nnop
            integer :: nfh
            integer :: nfe
            integer :: ddlc
            integer :: ddlm
            integer :: igeom
            real(kind=8) :: instam
            real(kind=8) :: instap
            integer :: ideplp
            real(kind=8) :: sigm(*)
            real(kind=8) :: vip(*)
            character(len=8) :: typmod(*)
            character(len=16) :: option
            integer :: imate
            character(len=16) :: compor(4)
            integer :: lgpg
            real(kind=8) :: crit(3)
            integer :: jpintt
            integer :: cnset(128)
            integer :: heavt(36*nfiss)
            integer :: lonch(10)
            real(kind=8) :: basloc(*)
            integer :: idepl
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            integer :: jfisno
            real(kind=8) :: sig(*)
            real(kind=8) :: vi(*)
            real(kind=8) :: matuu(*)
            integer :: ivectu
            integer :: codret
            integer :: jpmilt
          end subroutine xnmgr
        end interface
