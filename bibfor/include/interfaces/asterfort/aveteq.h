        interface
          subroutine aveteq(nbordr,vwork,tdisp,kwork,sommw,tspaq,i,&
     &veteq)
            integer :: tdisp
            integer :: nbordr
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            real(kind=8) :: veteq(nbordr)
          end subroutine aveteq
        end interface
