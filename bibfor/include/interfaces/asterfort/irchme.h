        interface
          subroutine irchme(ifichi,chanom,partie,nochmd,noresu,nomsym,&
     &typech,numord,nbrcmp,nomcmp,nbnoec,linoec,nbmaec,limaec,lvarie,&
     &sdcarm,codret)
            integer :: ifichi
            character(len=19) :: chanom
            character(*) :: partie
            character(len=64) :: nochmd
            character(len=8) :: noresu
            character(len=16) :: nomsym
            character(len=8) :: typech
            integer :: numord
            integer :: nbrcmp
            character(*) :: nomcmp(*)
            integer :: nbnoec
            integer :: linoec(*)
            integer :: nbmaec
            integer :: limaec(*)
            logical :: lvarie
            character(len=8) :: sdcarm
            integer :: codret
          end subroutine irchme
        end interface
