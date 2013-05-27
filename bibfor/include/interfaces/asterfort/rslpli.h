        interface
          subroutine rslpli(typ,mod,mater,hook,nmat,vin)
            integer :: nmat
            character(len=8) :: typ
            character(len=8) :: mod
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: hook(6,6)
            real(kind=8) :: vin(*)
          end subroutine rslpli
        end interface
