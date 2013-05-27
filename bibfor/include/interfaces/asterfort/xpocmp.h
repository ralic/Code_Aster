        interface
          subroutine xpocmp(elrefp,cns1,ima,n,jconx1,jconx2,ndim,nfh,&
     &nfe,ddlc,nbcmp,cmp,lmeca)
            integer :: nbcmp
            integer :: n
            character(len=8) :: elrefp
            character(len=19) :: cns1
            integer :: ima
            integer :: jconx1
            integer :: jconx2
            integer :: ndim
            integer :: nfh
            integer :: nfe
            integer :: ddlc
            integer :: cmp(nbcmp)
            logical :: lmeca
          end subroutine xpocmp
        end interface
