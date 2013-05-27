        interface
          subroutine merimo(base,modele,carele,mate,comref,compor,&
     &carcri,iterat,fonact,sddyna,valinc,solalg,merigi,vefint,optioz,&
     &tabret,codere)
            character(len=1) :: base
            character(len=24) :: modele
            character(len=24) :: carele
            character(*) :: mate
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=24) :: carcri
            integer :: iterat
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: merigi
            character(len=19) :: vefint
            character(*) :: optioz
            logical :: tabret(0:10)
            character(len=24) :: codere
          end subroutine merimo
        end interface
