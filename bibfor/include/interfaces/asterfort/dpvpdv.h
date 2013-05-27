        interface
          subroutine dpvpdv(vin,nbmat,mater,fonder)
            integer :: nbmat
            real(kind=8) :: vin(4)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: fonder(3)
          end subroutine dpvpdv
        end interface
