        interface
          subroutine lkdepp(vin,nbmat,mater,paraep,derpar)
            integer :: nbmat
            real(kind=8) :: vin(7)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: paraep(3)
            real(kind=8) :: derpar(3)
          end subroutine lkdepp
        end interface
