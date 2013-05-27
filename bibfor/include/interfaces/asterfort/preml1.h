        interface
          subroutine preml1(neq,n2,diag,delg,col,xadj,adjncy,parent,&
     &adress,supnd,nnz,qsize,llist,suiv,p,q,invp,perm,lgind,ddlmoy,nbsn,&
     &optnum,lgadjn,nrl,deb,vois,suit,ier,nec,prno,deeq,noeud,ddl,invpnd&
     &,permnd,spndnd,xadjd,matgen)
            integer :: lgadjn
            integer :: n2
            integer :: neq
            integer :: diag(0:neq)
            integer :: delg(neq)
            integer :: col(*)
            integer :: xadj(neq+1)
            integer :: adjncy(lgadjn)
            integer :: parent(neq)
            integer :: adress(neq)
            integer :: supnd(neq)
            integer :: nnz(1:neq)
            integer :: qsize(neq)
            integer :: llist(neq)
            integer :: suiv(neq)
            integer :: p(neq)
            integer :: q(n2)
            integer :: invp(neq)
            integer :: perm(neq)
            integer :: lgind
            integer :: ddlmoy
            integer :: nbsn
            integer :: optnum
            integer :: nrl
            integer :: deb(*)
            integer :: vois(*)
            integer :: suit(*)
            integer :: ier
            integer :: nec
            integer :: prno(*)
            integer :: deeq(*)
            integer :: noeud(*)
            integer :: ddl(*)
            integer :: invpnd(*)
            integer :: permnd(*)
            integer :: spndnd(*)
            integer :: xadjd(*)
            logical :: matgen
          end subroutine preml1
        end interface
