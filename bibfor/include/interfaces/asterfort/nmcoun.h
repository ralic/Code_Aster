        interface
          subroutine nmcoun(noma,fonact,solveu,numedz,matass,defico,&
     &resoco,deficu,resocu,iterat,valinc,solalg,veasse,instan,resigr,&
     &sdtime,sdstat,ctccvg)
            character(len=8) :: noma
            integer :: fonact(*)
            character(len=19) :: solveu
            character(*) :: numedz
            character(len=19) :: matass
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: deficu
            character(len=24) :: resocu
            integer :: iterat
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            character(len=19) :: veasse(*)
            real(kind=8) :: instan
            real(kind=8) :: resigr
            character(len=24) :: sdtime
            character(len=24) :: sdstat
            integer :: ctccvg
          end subroutine nmcoun
        end interface
