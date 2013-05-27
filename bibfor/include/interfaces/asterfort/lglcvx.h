        interface
          subroutine lglcvx(sig,vin,nbmat,mater,seuil)
            integer :: nbmat
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: seuil
          end subroutine lglcvx
        end interface
