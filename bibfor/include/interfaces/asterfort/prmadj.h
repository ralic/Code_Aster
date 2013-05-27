        interface
          subroutine prmadj(nbnd,neq,n2,adjncy,xadj,xadjd,liste,q,&
     &noeud)
            integer :: n2
            integer :: neq
            integer :: nbnd
            integer :: adjncy(*)
            integer :: xadj(neq+1)
            integer :: xadjd(*)
            integer :: liste(neq)
            integer :: q(n2)
            integer :: noeud(*)
          end subroutine prmadj
        end interface
