        interface
          subroutine ircnme(ifi,nochmd,chanom,typech,modele,nbcmp,&
     &nomcmp,partie,numpt,instan,numord,nbnoec,linoec,sdcarm,codret)
            integer :: ifi
            character(len=64) :: nochmd
            character(len=19) :: chanom
            character(len=8) :: typech
            character(len=8) :: modele
            integer :: nbcmp
            character(*) :: nomcmp(*)
            character(*) :: partie
            integer :: numpt
            real(kind=8) :: instan
            integer :: numord
            integer :: nbnoec
            integer :: linoec(*)
            character(len=8) :: sdcarm
            integer :: codret
          end subroutine ircnme
        end interface
