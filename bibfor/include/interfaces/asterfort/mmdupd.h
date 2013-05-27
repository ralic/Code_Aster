        interface
          subroutine mmdupd(ehead,neqns,xadj,adjncy,delta,mdeg,dhead,&
     &dforw,dbakw,qsize,llist,marker,maxint,tag)
            integer :: ehead
            integer :: neqns
            integer :: xadj(*)
            integer :: adjncy(*)
            integer :: delta
            integer :: mdeg
            integer :: dhead(*)
            integer :: dforw(*)
            integer :: dbakw(*)
            integer :: qsize(*)
            integer :: llist(*)
            integer :: marker(*)
            integer :: maxint
            integer :: tag
          end subroutine mmdupd
        end interface
