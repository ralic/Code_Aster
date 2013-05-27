        interface
          subroutine pemaxe(resu,nomcha,lieu,nomlie,modele,chpost,&
     &nbcmp,nomcmp,nuord,inst,iocc)
            integer :: nbcmp
            character(len=19) :: resu
            character(len=24) :: nomcha
            character(len=8) :: lieu
            character(len=8) :: nomlie
            character(len=8) :: modele
            character(len=19) :: chpost
            character(len=8) :: nomcmp(nbcmp)
            integer :: nuord
            real(kind=8) :: inst
            integer :: iocc
          end subroutine pemaxe
        end interface
