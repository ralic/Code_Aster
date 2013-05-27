        interface
          subroutine cfconv(noma,sdstat,sdimpr,sderro,defico,resoco,&
     &solalg)
            character(len=8) :: noma
            character(len=24) :: sdstat
            character(len=24) :: sdimpr
            character(len=24) :: sderro
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: solalg(*)
          end subroutine cfconv
        end interface
