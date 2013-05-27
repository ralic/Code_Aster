        interface
          subroutine dpvpva(vin,nbmat,mater,fonecr)
            integer :: nbmat
            real(kind=8) :: vin(4)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: fonecr(3)
          end subroutine dpvpva
        end interface
