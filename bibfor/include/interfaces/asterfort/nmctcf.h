        interface
          subroutine nmctcf(noma,modele,sdimpr,sderro,defico,resoco,&
     &valinc,mmcvfr)
            character(len=8) :: noma
            character(len=24) :: modele
            character(len=24) :: sdimpr
            character(len=24) :: sderro
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            logical :: mmcvfr
          end subroutine nmctcf
        end interface
