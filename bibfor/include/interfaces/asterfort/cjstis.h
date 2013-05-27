        interface
          subroutine cjstis(mod,mater,sig,vin,dsde)
            character(len=8) :: mod
            real(kind=8) :: mater(14,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: dsde(6,6)
          end subroutine cjstis
        end interface
