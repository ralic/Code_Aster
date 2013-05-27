        interface
          subroutine projax(vecpg,nbvec,nbordr,proaxe,iflag,rmima,raxe&
     &)
            integer :: nbordr
            integer :: nbvec
            real(kind=8) :: vecpg(2*nbvec*nbordr)
            character(len=16) :: proaxe
            integer :: iflag(nbvec)
            real(kind=8) :: rmima(4*nbvec)
            real(kind=8) :: raxe(nbvec*nbordr)
          end subroutine projax
        end interface
