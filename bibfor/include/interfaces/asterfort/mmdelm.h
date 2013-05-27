        interface
          subroutine mmdelm(mdnode,xadj,adjncy,dhead,dforw,dbakw,qsize&
     &,llist,marker,maxint,tag,parent)
            integer :: mdnode
            integer :: xadj(*)
            integer :: adjncy(*)
            integer :: dhead(*)
            integer :: dforw(*)
            integer :: dbakw(*)
            integer :: qsize(*)
            integer :: llist(*)
            integer :: marker(*)
            integer :: maxint
            integer :: tag
            integer :: parent(*)
          end subroutine mmdelm
        end interface
