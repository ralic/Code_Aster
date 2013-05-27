        interface
          subroutine caladj(col,diag,xadj,adjncy,n,nnz,deb,tab,suiv,&
     &lmat,ladjn,nrl)
            integer :: lmat
            integer :: n
            integer :: col(lmat)
            integer :: diag(0:n)
            integer :: xadj(n+1)
            integer :: adjncy(*)
            integer :: nnz(n)
            integer :: deb(n)
            integer :: tab(*)
            integer :: suiv(*)
            integer :: ladjn
            integer :: nrl
          end subroutine caladj
        end interface
