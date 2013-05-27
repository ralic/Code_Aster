        interface
          subroutine premlc(n1,diag,col,parent,parend,anc,nouv,supnd,&
     &supnd2,nouvsn,ancsn,p,q,lbd1,lbd2,rl,rl1,rl2,nrl,invp,perm,lgind,&
     &ddlmoy,nbsnd)
            integer :: n1
            integer :: diag(0:*)
            integer :: col(*)
            integer :: parent(*)
            integer :: parend(*)
            integer :: anc(n1)
            integer :: nouv(n1)
            integer :: supnd(n1)
            integer :: supnd2(n1)
            integer :: nouvsn(0:n1)
            integer :: ancsn(*)
            integer :: p(*)
            integer :: q(*)
            integer :: lbd1(n1)
            integer :: lbd2(n1)
            integer :: rl(4,*)
            integer :: rl1(*)
            integer :: rl2(*)
            integer :: nrl
            integer :: invp(n1)
            integer :: perm(n1)
            integer :: lgind
            integer :: ddlmoy
            integer :: nbsnd
          end subroutine premlc
        end interface
