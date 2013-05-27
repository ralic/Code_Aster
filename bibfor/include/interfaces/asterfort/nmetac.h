        interface
          subroutine nmetac(fonact,sddyna,defico,nbmax,chaact)
            integer :: nbmax
            integer :: fonact(*)
            character(len=19) :: sddyna
            character(len=24) :: defico
            logical :: chaact(nbmax)
          end subroutine nmetac
        end interface
