        interface
          subroutine mmdpar(nd,nbsn,nbsn1,supnd,nouv,parent,prov,&
     &invsup)
            integer :: nbsn1
            integer :: nd
            integer :: nbsn
            integer :: supnd(nbsn1)
            integer :: nouv(nd)
            integer :: parent(nd)
            integer :: prov(nd)
            integer :: invsup(nd)
          end subroutine mmdpar
        end interface
