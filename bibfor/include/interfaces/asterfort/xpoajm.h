        interface
          subroutine xpoajm(maxfem,jtypm2,itypse,jcnse,im,n,nnose,&
     &prefno,jdirno,nnm,inm,inmtot,nbmac,he,jnivgr,iagma,ngrm,jdirgr,&
     &opmail,nfiss,ndim,ndime,jconx1,jconx2,jconq1,jconq2,ima,iad1,nnn,&
     &inn,inntot,nbnoc,nbnofi,inofi,iacoo1,iacoo2,iad9,ninter,iainc,&
     &elrefp,jlsn,jlst,typma,igeom,jfisno,contac,cmp,nbcmp,nfh,nfe,ddlc,&
     &jcnsv1,jcnsv2,jcnsl2,lmeca)
            integer :: nfiss
            character(len=8) :: maxfem
            integer :: jtypm2
            integer :: itypse
            integer :: jcnse
            integer :: im
            integer :: n
            integer :: nnose
            character(len=2) :: prefno(4)
            integer :: jdirno
            integer :: nnm
            integer :: inm
            integer :: inmtot
            integer :: nbmac
            integer :: he(nfiss)
            integer :: jnivgr
            integer :: iagma
            integer :: ngrm
            integer :: jdirgr
            logical :: opmail
            integer :: ndim
            integer :: ndime
            integer :: jconx1
            integer :: jconx2
            integer :: jconq1
            integer :: jconq2
            integer :: ima
            integer :: iad1
            integer :: nnn
            integer :: inn
            integer :: inntot
            integer :: nbnoc
            integer :: nbnofi
            integer :: inofi
            integer :: iacoo1
            integer :: iacoo2
            integer :: iad9
            integer :: ninter
            integer :: iainc
            character(len=8) :: elrefp
            integer :: jlsn
            integer :: jlst
            character(len=8) :: typma
            integer :: igeom
            integer :: jfisno
            integer :: contac
            integer :: cmp(*)
            integer :: nbcmp
            integer :: nfh
            integer :: nfe
            integer :: ddlc
            integer :: jcnsv1
            integer :: jcnsv2
            integer :: jcnsl2
            logical :: lmeca
          end subroutine xpoajm
        end interface
