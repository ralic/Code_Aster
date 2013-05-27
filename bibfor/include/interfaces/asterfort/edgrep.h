        interface
          subroutine edgrep(typmod,coord,anic,ani)
            character(len=8) :: typmod(2)
            real(kind=8) :: coord(3)
            real(kind=8) :: anic(6,6)
            real(kind=8) :: ani(6,6)
          end subroutine edgrep
        end interface
