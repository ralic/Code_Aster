        interface
          subroutine dstnib(qsi,eta,carat3,an,am,nfx,nfy,nmx,nmy)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: carat3(*)
            real(kind=8) :: an(3,9)
            real(kind=8) :: am(3,6)
            real(kind=8) :: nfx(9)
            real(kind=8) :: nfy(9)
            real(kind=8) :: nmx(6)
            real(kind=8) :: nmy(6)
          end subroutine dstnib
        end interface
