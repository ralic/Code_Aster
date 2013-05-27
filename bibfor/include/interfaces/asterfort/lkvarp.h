        interface
          subroutine lkvarp(vin,nbmat,mater,paraep)
            integer :: nbmat
            real(kind=8) :: vin(7)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: paraep(3)
          end subroutine lkvarp
        end interface
