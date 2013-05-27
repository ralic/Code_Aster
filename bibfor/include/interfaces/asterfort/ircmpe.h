        interface
          subroutine ircmpe(nofimd,ncmpve,numcmp,exicmp,nbvato,nbmaec,&
     &limaec,adsd,adsl,nbimpr,ncaimi,ncaimk,tyefma,typmai,typgeo,nomtyp,&
     &typech,profas,promed,prorec,nroimp,chanom,sdcarm)
            integer :: nbvato
            integer :: ncmpve
            character(*) :: nofimd
            integer :: numcmp(ncmpve)
            logical :: exicmp(nbvato)
            integer :: nbmaec
            integer :: limaec(*)
            integer :: adsd
            integer :: adsl
            integer :: nbimpr
            character(len=24) :: ncaimi
            character(len=24) :: ncaimk
            integer :: tyefma(*)
            integer :: typmai(*)
            integer :: typgeo(*)
            character(len=8) :: nomtyp(*)
            character(len=8) :: typech
            integer :: profas(nbvato)
            integer :: promed(nbvato)
            integer :: prorec(nbvato)
            integer :: nroimp(nbvato)
            character(len=19) :: chanom
            character(len=8) :: sdcarm
          end subroutine ircmpe
        end interface
