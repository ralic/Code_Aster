        interface
          subroutine lgldcm(nbmat,mater,sig,vin)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: sig(6)
            real(kind=8) :: vin(*)
          end subroutine lgldcm
        end interface
