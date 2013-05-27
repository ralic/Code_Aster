        interface
          subroutine avsign(nbvec,nbordr,vectn,vwork,tdisp,kwork,sommw&
     &,tspaq,i,vsign)
            integer :: tdisp
            integer :: nbordr
            integer :: nbvec
            real(kind=8) :: vectn(3*nbvec)
            real(kind=8) :: vwork(tdisp)
            integer :: kwork
            integer :: sommw
            integer :: tspaq
            integer :: i
            real(kind=8) :: vsign(nbvec*nbordr)
          end subroutine avsign
        end interface
