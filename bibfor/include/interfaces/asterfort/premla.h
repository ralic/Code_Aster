        interface
          subroutine premla(neq,diag,col,lt,nrl,rl,deb,vois,suit,ier)
            integer :: nrl
            integer :: neq
            integer :: diag(0:neq)
            integer :: col(*)
            integer :: lt
            integer :: rl(4,nrl)
            integer :: deb(neq)
            integer :: vois(*)
            integer :: suit(*)
            integer :: ier
          end subroutine premla
        end interface
