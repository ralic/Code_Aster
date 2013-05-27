        interface
          subroutine irvari(ifi,nochmd,chanom,typech,modele,nbcmp,&
     &nomcmp,partie,numpt,instan,numord,nbmaec,limaec,noresu,carael,&
     &codret)
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
            integer :: nbmaec
            integer :: limaec(*)
            character(len=8) :: noresu
            character(len=8) :: carael
            integer :: codret
          end subroutine irvari
        end interface
