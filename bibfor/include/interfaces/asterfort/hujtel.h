        interface
          subroutine hujtel(mod,mater,sig,hook)
            character(len=8) :: mod
            real(kind=8) :: mater(22,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: hook(6,6)
          end subroutine hujtel
        end interface
