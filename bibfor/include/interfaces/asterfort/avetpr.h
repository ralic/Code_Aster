        interface
          subroutine avetpr(nbordr,vwork,tdisp,kwork,sommw,tspaq,i,&
     &vetpr,vsitn)
            integer :: tdisp
            integer :: nbordr
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            real(kind=8) :: vetpr(nbordr)
            real(kind=8) :: vsitn(nbordr)
          end subroutine avetpr
        end interface
