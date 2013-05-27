        interface
          subroutine xteddl(ndim,nfh,nfe,ddls,nddl,nno,nnos,stano,&
     &lcontx,matsym,option,nomte,mat,vect,ddlm,nfiss,jfisno)
            integer :: nfiss
            integer :: nno
            integer :: ndim
            integer :: nfh
            integer :: nfe
            integer :: ddls
            integer :: nddl
            integer :: nnos
            integer :: stano(*)
            logical :: lcontx
            logical :: matsym
            character(len=16) :: option
            character(len=16) :: nomte
            real(kind=8) :: mat(*)
            real(kind=8) :: vect(*)
            integer :: ddlm
            integer :: jfisno
          end subroutine xteddl
        end interface
