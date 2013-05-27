        interface
          subroutine rslesd(result,nuord,modele,materi,carele,excit,&
     &iexcit)
            character(len=8) :: result
            integer :: nuord
            character(len=8) :: modele
            character(len=8) :: materi
            character(len=8) :: carele
            character(len=19) :: excit
            integer :: iexcit
          end subroutine rslesd
        end interface
