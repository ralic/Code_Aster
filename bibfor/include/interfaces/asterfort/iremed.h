        interface
          subroutine iremed(nomcon,ifichi,nocham,novcmp,partie,liordr,&
     &lresu,nbnoec,linoec,nbmaec,limaec,nomcmp,lvarie,carael)
            character(*) :: nomcon
            integer :: ifichi
            character(*) :: nocham
            character(*) :: novcmp
            character(*) :: partie
            character(*) :: liordr
            logical :: lresu
            integer :: nbnoec
            integer :: linoec(*)
            integer :: nbmaec
            integer :: limaec(*)
            character(*) :: nomcmp
            logical :: lvarie
            character(len=8) :: carael
          end subroutine iremed
        end interface
