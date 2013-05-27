        interface
          subroutine nmctcl(numins,modele,noma,defico,resoco,sddyna,&
     &sddisc,loptin)
            integer :: numins
            character(len=24) :: modele
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=19) :: sddyna
            character(len=19) :: sddisc
            logical :: loptin
          end subroutine nmctcl
        end interface
