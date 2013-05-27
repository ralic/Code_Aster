        interface
          subroutine exfonc(fonact,parmet,method,solveu,defico,sddyna)
            integer :: fonact(*)
            real(kind=8) :: parmet(*)
            character(len=16) :: method(*)
            character(len=19) :: solveu
            character(len=24) :: defico
            character(len=19) :: sddyna
          end subroutine exfonc
        end interface
