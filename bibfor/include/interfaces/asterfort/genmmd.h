        interface
          subroutine genmmd(neqns,neqp1,nadj,xadj,adjncy,maxint,delta,&
     &invp,perm,nbsn,supnd,adress,parent,gssubs,fctnzs,fctops,dhead,&
     &qsize,llist,marker)
            integer :: nadj
            integer :: neqp1
            integer :: neqns
            integer :: xadj(neqp1)
            integer :: adjncy(nadj)
            integer :: maxint
            integer :: delta
            integer :: invp(neqns)
            integer :: perm(neqns)
            integer :: nbsn
            integer :: supnd(neqp1)
            integer :: adress(neqp1)
            integer :: parent(neqns)
            integer :: gssubs
            integer :: fctnzs
            real(kind=8) :: fctops
            integer :: dhead(neqns)
            integer :: qsize(neqns)
            integer :: llist(neqns)
            integer :: marker(neqns)
          end subroutine genmmd
        end interface
