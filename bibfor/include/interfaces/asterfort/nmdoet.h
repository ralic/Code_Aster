        interface
          subroutine nmdoet(modele,compor,fonact,numedd,sdpilo,sddyna,&
     &sdcriq,sdieto,solalg,lacc0,instin)
            character(len=24) :: modele
            character(len=24) :: compor
            integer :: fonact(*)
            character(len=24) :: numedd
            character(len=19) :: sdpilo
            character(len=19) :: sddyna
            character(len=24) :: sdcriq
            character(len=24) :: sdieto
            character(len=19) :: solalg(*)
            logical :: lacc0
            real(kind=8) :: instin
          end subroutine nmdoet
        end interface
