        interface
          subroutine ircmpr(nofimd,typech,nbimpr,ncaimi,ncaimk,ncmprf,&
     &ncmpve,ntlcmp,nbvato,nbenec,lienec,adsd,adsl,nomaas,modele,typgeo,&
     &nomtyp,ntproa,chanom,sdcarm)
            character(*) :: nofimd
            character(len=8) :: typech
            integer :: nbimpr
            character(len=24) :: ncaimi
            character(len=24) :: ncaimk
            integer :: ncmprf
            integer :: ncmpve
            character(*) :: ntlcmp
            integer :: nbvato
            integer :: nbenec
            integer :: lienec(*)
            integer :: adsd
            integer :: adsl
            character(len=8) :: nomaas
            character(len=8) :: modele
            integer :: typgeo(*)
            character(len=8) :: nomtyp(*)
            character(*) :: ntproa
            character(len=19) :: chanom
            character(len=8) :: sdcarm
          end subroutine ircmpr
        end interface
