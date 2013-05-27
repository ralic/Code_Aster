        interface
          subroutine nmctcc(noma,modele,mate,sddyna,sdimpr,sderro,&
     &defico,resoco,valinc,solalg,mmcvca)
            character(len=8) :: noma
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=19) :: sddyna
            character(len=24) :: sdimpr
            character(len=24) :: sderro
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            character(len=19) :: solalg(*)
            logical :: mmcvca
          end subroutine nmctcc
        end interface
