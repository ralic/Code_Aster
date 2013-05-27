        interface
          subroutine xpoajd(elrefp,ino,nnop,lsn,lst,ninter,iainc,typma&
     &,co,igeom,jdirno,nfiss,jfisno,he,ndime,ndim,cmp,nbcmp,nfh,nfe,ddlc&
     &,ima,jconx1,jconx2,jcnsv1,jcnsv2,jcnsl2,nbnoc,inntot,inn,nnn,&
     &contac,lmeca)
            integer :: nbcmp
            integer :: nfiss
            integer :: nnop
            character(len=8) :: elrefp
            integer :: ino
            real(kind=8) :: lsn(nfiss)
            real(kind=8) :: lst(nfiss)
            integer :: ninter
            integer :: iainc
            character(len=8) :: typma
            real(kind=8) :: co(3)
            integer :: igeom
            integer :: jdirno
            integer :: jfisno
            integer :: he(nfiss)
            integer :: ndime
            integer :: ndim
            integer :: cmp(nbcmp)
            integer :: nfh
            integer :: nfe
            integer :: ddlc
            integer :: ima
            integer :: jconx1
            integer :: jconx2
            integer :: jcnsv1
            integer :: jcnsv2
            integer :: jcnsl2
            integer :: nbnoc
            integer :: inntot
            integer :: inn
            integer :: nnn
            integer :: contac
            logical :: lmeca
          end subroutine xpoajd
        end interface
