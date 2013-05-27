        interface
          subroutine avphyd(nbordr,vwork,tdisp,kwork,sommw,tspaq,i,&
     &vphydr)
            integer :: tdisp
            integer :: nbordr
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            real(kind=8) :: vphydr(nbordr)
          end subroutine avphyd
        end interface
