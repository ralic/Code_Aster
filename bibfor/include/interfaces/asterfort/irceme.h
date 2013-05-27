        interface
          subroutine irceme(ifi,nochmd,chanom,typech,modele,nbcmp,&
     &nomcmp,etiqcp,partie,numpt,instan,numord,nbmaec,limaec,sdcarm,&
     &codret)
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
            integer :: nbmaec
            integer :: limaec(*)
            character(len=8) :: sdcarm
            integer :: codret
          end subroutine irceme
        end interface
