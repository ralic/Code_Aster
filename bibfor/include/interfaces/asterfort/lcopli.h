        interface
          subroutine lcopli(typ,mod,mater,hook)
            character(len=8) :: typ
            character(len=8) :: mod
            real(kind=8) :: mater(*)
            real(kind=8) :: hook(6,6)
          end subroutine lcopli
        end interface
