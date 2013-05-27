        interface
          subroutine extdch(typext,valinc,nocham,nocmp,dval)
            character(len=8) :: typext
            character(len=19) :: valinc(*)
            character(len=16) :: nocham
            character(len=16) :: nocmp
            real(kind=8) :: dval
          end subroutine extdch
        end interface
