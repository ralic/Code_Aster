        interface
          subroutine xside2(elrefp,ndim,coorse,elrese,igeom,he,nfh,&
     &ddlc,ddlm,nfe,basloc,nnop,npg,idecpg,typmod,imate,compor,idepl,lsn&
     &,lst,nfiss,fisno,sig)
            integer :: nfiss
            integer :: npg
            integer :: nnop
            integer :: ndim
            character(len=8) :: elrefp
            real(kind=8) :: coorse(*)
            character(len=8) :: elrese
            integer :: igeom
            real(kind=8) :: he(nfiss)
            integer :: nfh
            integer :: ddlc
            integer :: ddlm
            integer :: nfe
            real(kind=8) :: basloc(6*nnop)
            integer :: idecpg
            character(len=8) :: typmod(*)
            integer :: imate
            character(len=16) :: compor(4)
            integer :: idepl
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            integer :: fisno(nnop,nfiss)
            real(kind=8) :: sig(4,npg)
          end subroutine xside2
        end interface
