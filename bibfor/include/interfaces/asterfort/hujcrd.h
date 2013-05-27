        interface
          subroutine hujcrd(k,mater,sig,vin,seuild)
            integer :: k
            real(kind=8) :: mater(22,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: seuild
          end subroutine hujcrd
        end interface
