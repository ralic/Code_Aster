        interface
          subroutine nmtble(modele,noma,mate,defico,resoco,niveau,&
     &fonact,sdimpr,sdstat,sdtime,sddyna,sderro,sdconv,sddisc,valinc,&
     &solalg)
            character(len=24) :: modele
            character(len=8) :: noma
            character(len=24) :: mate
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: niveau
            integer :: fonact(*)
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=24) :: sdtime
            character(len=19) :: sddyna
            character(len=24) :: sderro
            character(len=24) :: sdconv
            character(len=19) :: sddisc
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
          end subroutine nmtble
        end interface
