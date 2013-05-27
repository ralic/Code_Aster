        interface
          subroutine lcopil(typ,mod,mater,kooh)
            character(len=8) :: typ
            character(len=8) :: mod
            real(kind=8) :: mater(*)
            real(kind=8) :: kooh(6,6)
          end subroutine lcopil
        end interface
