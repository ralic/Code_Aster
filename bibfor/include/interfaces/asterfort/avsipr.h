        interface
          subroutine avsipr(nbordr,vwork,tdisp,kwork,sommw,tspaq,i,&
     &vsipr,vepsn)
            integer :: tdisp
            integer :: nbordr
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            real(kind=8) :: vsipr(nbordr)
            real(kind=8) :: vepsn(nbordr)
          end subroutine avsipr
        end interface
