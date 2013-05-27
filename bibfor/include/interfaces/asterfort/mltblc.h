        interface
          subroutine mltblc(nbsn,debfsn,mxbloc,seq,nbloc,decal,lgbloc,&
     &ncbloc)
            integer :: nbsn
            integer :: debfsn(nbsn+1)
            integer :: mxbloc
            integer :: seq(nbsn)
            integer :: nbloc
            integer :: decal(nbsn)
            integer :: lgbloc(*)
            integer :: ncbloc(*)
          end subroutine mltblc
        end interface
