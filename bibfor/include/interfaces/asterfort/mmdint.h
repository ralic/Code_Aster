        interface
          subroutine mmdint(neqns,xadj,dhead,dforw,dbakw,qsize,llist,&
     &marker)
            integer :: neqns
            integer :: xadj(*)
            integer :: dhead(*)
            integer :: dforw(*)
            integer :: dbakw(*)
            integer :: qsize(*)
            integer :: llist(*)
            integer :: marker(*)
          end subroutine mmdint
        end interface
