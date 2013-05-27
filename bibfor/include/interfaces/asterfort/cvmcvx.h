        interface
          subroutine cvmcvx(nmat,mater,sig,vin,seuil)
            integer :: nmat
            real(kind=8) :: mater(nmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
            real(kind=8) :: seuil
          end subroutine cvmcvx
        end interface
