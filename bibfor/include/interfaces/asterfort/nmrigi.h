        interface
          subroutine nmrigi(modelz,mate,carele,compor,carcri,sddyna,&
     &sdstat,sdtime,fonact,iterat,valinc,solalg,comref,meelem,veelem,&
     &optioz,ldccvg,codere)
            character(*) :: modelz
            character(*) :: mate
            character(len=24) :: carele
            character(len=24) :: compor
            character(len=24) :: carcri
            character(len=19) :: sddyna
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            integer :: fonact(*)
            integer :: iterat
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=24) :: comref
            character(len=19) :: meelem(*)
            character(len=19) :: veelem(*)
            character(*) :: optioz
            integer :: ldccvg
            character(len=24) :: codere
          end subroutine nmrigi
        end interface
