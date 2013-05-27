        interface
          subroutine nmetcr(modele,compor,fonact,sddyna,sdpost,defico,&
     &resoco,sdieto,carele)
            character(len=24) :: modele
            character(len=24) :: compor
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=19) :: sdpost
            character(len=24) :: defico
            character(len=24) :: resoco
            character(len=24) :: sdieto
            character(len=24) :: carele
          end subroutine nmetcr
        end interface
