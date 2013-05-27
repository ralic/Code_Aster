        interface
          subroutine prnchk(nbsn,adress,global,fils,frere,lgsn,lfront,&
     &invsup,seq)
            integer :: nbsn
            integer :: adress(*)
            integer(kind=4) :: global(*)
            integer :: fils(*)
            integer :: frere(*)
            integer :: lgsn(*)
            integer :: lfront(*)
            integer :: invsup(*)
            integer :: seq(*)
          end subroutine prnchk
        end interface
