        interface
          subroutine nmfint(modele,mate,carele,comref,compor,carcri,&
     &fonact,iterat,sddyna,sdstat,sdtime,valinc,solalg,ldccvg,codere,&
     &vefint)
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: comref
            character(len=24) :: compor
            character(len=24) :: carcri
            integer :: fonact(*)
            integer :: iterat
            character(len=19) :: sddyna
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            integer :: ldccvg
            character(len=24) :: codere
            character(len=19) :: vefint
          end subroutine nmfint
        end interface
