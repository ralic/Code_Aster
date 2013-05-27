        interface
          subroutine xdecqv(nnose,it,cnset,lsn,igeom,ninter,npts,&
     &ainter,nse,cnse,heav,nsemax)
            integer :: nnose
            integer :: it
            integer :: cnset(*)
            real(kind=8) :: lsn(*)
            integer :: igeom
            integer :: ninter
            integer :: npts
            real(kind=8) :: ainter(*)
            integer :: nse
            integer :: cnse(6,6)
            real(kind=8) :: heav(*)
            integer :: nsemax
          end subroutine xdecqv
        end interface
