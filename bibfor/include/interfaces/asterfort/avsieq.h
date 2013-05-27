        interface
          subroutine avsieq(nbordr,vwork,tdisp,kwork,sommw,tspaq,i,&
     &vsieq)
            integer :: tdisp
            integer :: nbordr
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            real(kind=8) :: vsieq(nbordr)
          end subroutine avsieq
        end interface
