        interface
          subroutine peecal(tych,resu,nomcha,lieu,nomlie,modele,ichagd&
     &,chpost,nbcmp,nomcmp,nomcp2,nuord,inst,iocc)
            integer :: nbcmp
            character(len=4) :: tych
            character(len=19) :: resu
            character(len=24) :: nomcha
            character(len=8) :: lieu
            character(*) :: nomlie
            character(len=8) :: modele
            integer :: ichagd
            character(len=19) :: chpost
            character(len=8) :: nomcmp(nbcmp)
            character(len=8) :: nomcp2(nbcmp)
            integer :: nuord
            real(kind=8) :: inst
            integer :: iocc
          end subroutine peecal
        end interface
