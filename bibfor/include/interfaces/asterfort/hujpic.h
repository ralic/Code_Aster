        interface
          subroutine hujpic(kk,k,tin,vin,mater,yf,pc)
            common/tdim/ ndt,ndi
              integer :: ndt
              integer :: ndi
            integer :: kk
            integer :: k
            real(kind=8) :: tin(ndt)
            real(kind=8) :: vin(*)
            real(kind=8) :: mater(22,2)
            real(kind=8) :: yf(15)
            real(kind=8) :: pc
          end subroutine hujpic
        end interface
