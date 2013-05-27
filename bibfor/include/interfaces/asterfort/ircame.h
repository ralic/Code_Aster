        interface
          subroutine ircame(ifi,nochmd,chanom,typech,modele,nbcmp,&
     &nomcmp,etiqcp,partie,numpt,instan,numord,adsk,adsd,adsc,adsv,adsl,&
     &nbenec,lienec,sdcarm,codret)
            integer :: ifi
            character(len=64) :: nochmd
            character(len=19) :: chanom
            character(len=8) :: typech
            character(len=8) :: modele
            integer :: nbcmp
            character(*) :: nomcmp(*)
            character(*) :: etiqcp
            character(*) :: partie
            integer :: numpt
            real(kind=8) :: instan
            integer :: numord
            integer :: adsk
            integer :: adsd
            integer :: adsc
            integer :: adsv
            integer :: adsl
            integer :: nbenec
            integer :: lienec(*)
            character(len=8) :: sdcarm
            integer :: codret
          end subroutine ircame
        end interface
