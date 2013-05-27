        interface
          subroutine nmctgo(noma,sdimpr,sderro,defico,resoco,valinc,&
     &mmcvgo)
            character(len=8) :: noma
            character(len=24) :: sdimpr
            character(len=24) :: sderro
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: valinc(*)
            logical :: mmcvgo
          end subroutine nmctgo
        end interface
