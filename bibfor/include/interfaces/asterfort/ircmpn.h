        interface
          subroutine ircmpn(nofimd,ncmprf,ncmpve,numcmp,exicmp,nbvato,&
     &nbnoec,linoec,adsl,caimpi,caimpk,profas,innoce)
            integer :: nbvato
            integer :: ncmprf
            character(*) :: nofimd
            integer :: ncmpve
            integer :: numcmp(ncmprf)
            logical :: exicmp(nbvato)
            integer :: nbnoec
            integer :: linoec(*)
            integer :: adsl
            integer :: caimpi(10,1)
            character(len=80) :: caimpk(3,1)
            integer :: profas(nbvato)
            integer :: innoce(nbvato)
          end subroutine ircmpn
        end interface
