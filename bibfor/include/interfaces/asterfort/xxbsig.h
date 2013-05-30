        interface
          subroutine xxbsig(option,elrefp,elrese,ndim,coorse,igeom,he,&
     &nfh,ddlc,ddlm,nfe,basloc,nnop,npg,sigma,compor,idepl,lsn,lst,nfiss&
     &,fisno,codopt,ivectu)
            integer :: codopt
            integer :: nfiss
            integer :: npg
            integer :: nnop
            integer :: nfe
            integer :: nfh
            integer :: ndim
            character(len=16) :: option
            character(len=8) :: elrefp
            character(len=8) :: elrese
            real(kind=8) :: coorse(*)
            integer :: igeom
            real(kind=8) :: he(nfiss)
            integer :: ddlc
            integer :: ddlm
            real(kind=8) :: basloc(3*ndim*nnop)
            real(kind=8) :: sigma(codopt*(2*ndim-1)+1,codopt*(npg-1)+1)
            character(len=16) :: compor(4)
            integer :: idepl
            real(kind=8) :: lsn(nnop)
            real(kind=8) :: lst(nnop)
            integer :: fisno(nnop,nfiss)
            integer :: ivectu
          end subroutine xxbsig
        end interface
