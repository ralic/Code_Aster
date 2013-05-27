        interface
          subroutine cfmmcv(noma,modele,numedd,fonact,sddyna,sdimpr,&
     &sdstat,sddisc,sdtime,sderro,numins,iterat,defico,resoco,valinc,&
     &solalg)
            character(len=8) :: noma
            character(len=24) :: modele
            character(len=24) :: numedd
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=24) :: sdimpr
            character(len=24) :: sdstat
            character(len=19) :: sddisc
            character(len=24) :: sdtime
            character(len=24) :: sderro
            integer :: numins
            integer :: iterat
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
          end subroutine cfmmcv
        end interface
