        interface
          subroutine cfmxpo(noma,modelz,defico,resoco,numins,sddisc,&
     &sdstat,solalg,valinc,veasse)
            character(len=8) :: noma
            character(*) :: modelz
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: numins
            character(len=19) :: sddisc
            character(len=24) :: sdstat
            character(len=19) :: solalg(*)
            character(len=19) :: valinc(*)
            character(len=19) :: veasse(*)
          end subroutine cfmxpo
        end interface
