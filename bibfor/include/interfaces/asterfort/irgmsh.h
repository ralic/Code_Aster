        interface
          subroutine irgmsh(nomcon,partie,ifi,nbcham,cham,lresu,nbordr&
     &,ordr,nbcmp,nomcmp,nbmat,nummai,versio,lgmsh,tycha)
            character(*) :: nomcon
            character(*) :: partie
            integer :: ifi
            integer :: nbcham
            character(*) :: cham(*)
            logical :: lresu
            integer :: nbordr
            integer :: ordr(*)
            integer :: nbcmp
            character(*) :: nomcmp(*)
            integer :: nbmat
            integer :: nummai(*)
            integer :: versio
            logical :: lgmsh
            character(len=8) :: tycha
          end subroutine irgmsh
        end interface
