        interface
          subroutine mltpos(nbsn,parent,fils,frere,pile,lfront,seq,&
     &flag,estim,u,w,tab,liste)
            integer :: nbsn
            integer :: parent(*)
            integer :: fils(*)
            integer :: frere(*)
            integer :: pile(*)
            integer :: lfront(*)
            integer :: seq(*)
            integer :: flag(*)
            integer :: estim
            integer :: u(nbsn)
            integer :: w(nbsn)
            integer :: tab(nbsn)
            integer :: liste(nbsn)
          end subroutine mltpos
        end interface
