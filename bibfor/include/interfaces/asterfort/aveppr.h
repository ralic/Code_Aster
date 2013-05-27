        interface
          subroutine aveppr(nbordr,vwork,tdisp,kwork,sommw,tspaq,i,&
     &veppr,vsipn)
            integer :: tdisp
            integer :: nbordr
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            real(kind=8) :: veppr(nbordr)
            real(kind=8) :: vsipn(nbordr)
          end subroutine aveppr
        end interface
