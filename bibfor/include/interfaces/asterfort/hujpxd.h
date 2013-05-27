        interface
          subroutine hujpxd(k,mater,sig,vin,prox,proxc)
            integer :: k
            real(kind=8) :: mater(22,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            logical :: prox
            logical :: proxc
          end subroutine hujpxd
        end interface
