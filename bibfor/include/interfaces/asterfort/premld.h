        interface
          subroutine premld(n1,diag,col,xadj1,adjnc1,nnz,deb,voisin,&
     &suiv,ladjn,nrl)
            integer :: n1
            integer :: diag(0:*)
            integer :: col(*)
            integer :: xadj1(n1+1)
            integer :: adjnc1(*)
            integer :: nnz(1:n1)
            integer :: deb(1:n1)
            integer :: voisin(*)
            integer :: suiv(*)
            integer :: ladjn
            integer :: nrl
          end subroutine premld
        end interface
