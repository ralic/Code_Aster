        interface
          subroutine dsqnib(qsi,eta,caraq4,an,am,nfx,nfy,nmx,nmy)
            real(kind=8) :: qsi
            real(kind=8) :: eta
            real(kind=8) :: caraq4(*)
            real(kind=8) :: an(4,12)
            real(kind=8) :: am(4,8)
            real(kind=8) :: nfx(12)
            real(kind=8) :: nfy(12)
            real(kind=8) :: nmx(8)
            real(kind=8) :: nmy(8)
          end subroutine dsqnib
        end interface
