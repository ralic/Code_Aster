        interface
          subroutine hujpxs(mater,sig,vin,prox)
            real(kind=8) :: mater(22,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            logical :: prox
          end subroutine hujpxs
        end interface
