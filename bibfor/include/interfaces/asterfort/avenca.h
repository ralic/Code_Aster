        interface
          subroutine avenca(rvecpg,nbvec,nbordr,lsig0,iflag,rmima)
            integer :: nbordr
            integer :: nbvec
            real(kind=8) :: rvecpg(2*nbvec*nbordr)
            logical :: lsig0
            integer :: iflag(nbvec)
            real(kind=8) :: rmima(4*nbvec)
          end subroutine avenca
        end interface
