        interface
          subroutine dflldc(mcfact,iechec,dtmin,even,submet,subaut,&
     &pasmin,nbrpas,niveau,subins,subdur)
            character(len=16) :: mcfact
            integer :: iechec
            real(kind=8) :: dtmin
            character(len=16) :: even
            character(len=16) :: submet
            character(len=16) :: subaut
            real(kind=8) :: pasmin
            integer :: nbrpas
            integer :: niveau
            real(kind=8) :: subins
            real(kind=8) :: subdur
          end subroutine dflldc
        end interface
