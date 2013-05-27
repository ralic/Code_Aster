        interface
          subroutine nmfonc(parcri,parmet,method,solveu,modele,defico,&
     &lischa,lcont,lunil,sdnume,sddyna,sdcriq,mate,compoz,result,fonact)
            real(kind=8) :: parcri(*)
            real(kind=8) :: parmet(*)
            character(len=16) :: method(*)
            character(len=19) :: solveu
            character(len=24) :: modele
            character(len=24) :: defico
            character(len=19) :: lischa
            logical :: lcont
            logical :: lunil
            character(len=19) :: sdnume
            character(len=19) :: sddyna
            character(len=24) :: sdcriq
            character(len=24) :: mate
            character(*) :: compoz
            character(len=8) :: result
            integer :: fonact(*)
          end subroutine nmfonc
        end interface
