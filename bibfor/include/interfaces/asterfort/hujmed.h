        interface
          subroutine hujmed(k,mater,vin,sig)
            integer :: k
            real(kind=8) :: mater(22,2)
            real(kind=8) :: vin(*)
            real(kind=8) :: sig(6)
          end subroutine hujmed
        end interface
