        interface
          subroutine cfmxre(noma,nomo,sdstat,defico,resoco,numins,&
     &sddisc,solalg,valinc,veasse)
            character(len=8) :: noma
            character(len=8) :: nomo
            character(len=24) :: sdstat
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: numins
            character(len=19) :: sddisc
            character(len=19) :: solalg(*)
            character(len=19) :: valinc(*)
            character(len=19) :: veasse(*)
          end subroutine cfmxre
        end interface
